pub mod bot;
pub mod irc;
pub mod multiplayer;

use tokio::net::TcpStream;
use tokio::sync::{broadcast, mpsc, oneshot};
use tokio::task::JoinHandle;
use tokio::time::{self, Duration, Instant};

use core::fmt;
use std::collections::HashMap;

use self::irc::{ToMessageTarget, Transport};

use crate::{
    error::{ConversionError, StdError},
    StdResult,
};

use bitflags::bitflags;

type Result<T> = StdResult<T, Error>;

#[derive(Debug)]
pub enum Error {
    AuthError(String),
    Transport(TransportError),
    OperationTimeout,
    Irc(irc::Error),
    Io(tokio::io::Error),
}

impl StdError for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::AuthError(e) => write!(f, "Auth error: {}", e),
            Error::Transport(e) => write!(f, "Transport error: {}", e),
            Error::OperationTimeout => write!(f, "Operation timeout"),
            Error::Irc(e) => write!(f, "IRC error: {}", e),
            Error::Io(e) => write!(f, "IO error: {}", e),
        }
    }
}

impl From<irc::Error> for Error {
    fn from(e: irc::Error) -> Self {
        Error::Irc(e)
    }
}

impl From<tokio::io::Error> for Error {
    fn from(e: tokio::io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<TransportError> for Error {
    fn from(e: TransportError) -> Self {
        Error::Transport(e)
    }
}

#[derive(Debug)]
pub enum TransportError {
    PingerTimeout,
    ConnectionClosed,
}

impl StdError for TransportError {}

impl fmt::Display for TransportError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TransportError::PingerTimeout => write!(f, "Pinger timeout"),
            TransportError::ConnectionClosed => write!(f, "Connection closed"),
        }
    }
}

#[derive(Copy, Clone)]
pub enum Status {
    Unspecific,
    Disconnected,
    Established, // TcpStream connected
    Connected,   // IRC connected
}

#[derive(Debug, Clone)]
pub struct ClientOptions {
    endpoint: String,
    username: String,
    password: Option<String>,
    bot: User,
    operation_timeout: Duration,
    pinger_interval: Duration,
    pinger_timeout: Duration,
}

impl ClientOptions {
    pub fn endpoint(mut self, e: String) -> Self {
        self.endpoint = e;
        self
    }
    pub fn username(mut self, u: String) -> Self {
        self.username = u;
        self
    }
    pub fn password(mut self, p: String) -> Self {
        self.password = Some(p);
        self
    }
}

impl Default for ClientOptions {
    fn default() -> Self {
        Self {
            endpoint: "irc.ppy.sh:6667".to_owned(),
            username: "".to_owned(),
            password: None,
            bot: User {
                id: 3,
                name: "BanchoBot".to_string(),
            },
            // TODO: adjust timeout
            operation_timeout: Duration::from_secs(5),
            pinger_interval: Duration::from_secs(15),
            pinger_timeout: Duration::from_secs(30),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Sender {
    tx: mpsc::Sender<Action>,
}

impl Sender {
    pub fn into_user_writer(self, user: User) -> UserSender {
        UserSender { user, writer: self }
    }

    pub fn into_channel_writer(self, channel: Channel) -> ChannelSender {
        ChannelSender {
            channel,
            writer: self,
        }
    }

    // FIXME: all crate::Result<()> functions are unreliable, they only reflect the result of tx.send()
    //        and do not reflect the actual result of the operation.
    pub async fn send_irc(&mut self, message: irc::Message) -> crate::Result<()> {
        self.tx.send(Action::Raw(message)).await?;
        Ok(())
    }

    pub async fn send_channel_chat(&mut self, channel: Channel, body: &str) -> crate::Result<()> {
        self.tx
            .send(Action::Chat {
                target: channel.to_message_target(),
                body: body.to_string(),
            })
            .await?;
        Ok(())
    }

    pub async fn send_private_chat(&mut self, user: User, body: &str) -> crate::Result<()> {
        self.tx
            .send(Action::Chat {
                target: user.to_message_target(),
                body: body.to_string(),
            })
            .await?;
        Ok(())
    }

    pub async fn send_target_chat<T>(&mut self, target: T, body: &str) -> crate::Result<()>
    where
        T: irc::ToMessageTarget,
    {
        self.tx
            .send(Action::Chat {
                target: target.to_message_target(),
                body: body.to_string(),
            })
            .await?;
        Ok(())
    }

    pub async fn send_private_bot_command(
        &mut self,
        command: bot::Command,
    ) -> crate::Result<bot::Response> {
        self.send_bot_command(None, command).await
    }

    pub async fn send_channel_bot_command(
        &mut self,
        channel: Channel,
        command: bot::Command,
    ) -> crate::Result<bot::Response> {
        self.send_bot_command(Some(channel), command).await
    }

    pub async fn send_bot_command(
        &mut self,
        channel: Option<Channel>,
        command: bot::Command,
    ) -> crate::Result<bot::Response> {
        // Check if the command requires a channel to issue and ensure the channel is a multiplayer channel if provided.
        if (channel.is_none() && command.requires_channel())
            || (channel.is_some() && !matches!(channel, Some(Channel::Multiplayer(_))))
        {
            return Err("This command requires a multiplayer channel.".into());
        }

        let (tx, rx) = oneshot::channel();
        self.tx
            .send(Action::BotCommand {
                channel,
                command,
                tx: Some(tx),
            })
            .await?;
        rx.await?
    }

    pub async fn send_unreliable_private_bot_command(
        &mut self,
        command: bot::Command,
    ) -> crate::Result<()> {
        self.send_unreliable_bot_command(None, command).await
    }

    pub async fn send_unreliable_channel_bot_command(
        &mut self,
        channel: Channel,
        command: bot::Command,
    ) -> crate::Result<()> {
        self.send_unreliable_bot_command(Some(channel), command)
            .await
    }

    pub async fn send_unreliable_bot_command(
        &mut self,
        channel: Option<Channel>,
        command: bot::Command,
    ) -> crate::Result<()> {
        self.tx
            .send(Action::BotCommand {
                channel,
                command,
                tx: None,
            })
            .await?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
enum Target {
    Channel(Channel),
    User(User),
}

impl ToString for Target {
    fn to_string(&self) -> String {
        match self {
            Target::Channel(channel) => channel.to_string(),
            Target::User(user) => user.irc_name(),
        }
    }
}

impl From<Channel> for Target {
    fn from(channel: Channel) -> Self {
        Self::Channel(channel)
    }
}

impl From<User> for Target {
    fn from(user: User) -> Self {
        Self::User(user)
    }
}

#[derive(Debug, Clone)]
pub struct UserSender {
    user: User,
    writer: Sender,
}

impl UserSender {
    pub async fn send_chat(&mut self, body: &str) -> crate::Result<()> {
        self.writer.send_private_chat(self.user.clone(), body).await
    }
    pub fn writer(&self) -> Sender {
        self.writer.clone()
    }
    pub fn user(&self) -> &User {
        &self.user
    }
}

#[derive(Debug, Clone)]
pub struct ChannelSender {
    channel: Channel,
    writer: Sender,
}

impl ChannelSender {
    pub async fn send_chat(&mut self, body: &str) -> crate::Result<()> {
        self.writer
            .send_channel_chat(self.channel.clone(), body)
            .await
    }
    pub async fn send_bot_command(
        &mut self,
        command: bot::Command,
    ) -> crate::Result<bot::Response> {
        self.writer
            .send_channel_bot_command(self.channel.clone(), command)
            .await
    }
    pub async fn send_unreliable_bot_command(
        &mut self,
        command: bot::Command,
    ) -> crate::Result<()> {
        self.writer
            .send_unreliable_channel_bot_command(self.channel.clone(), command)
            .await
    }
    pub fn writer(&self) -> Sender {
        self.writer.clone()
    }
    pub fn channel(&self) -> &Channel {
        &self.channel
    }
}

#[derive(Debug)]
struct ChannelState {
    channel: Channel,
    topic: String,
    tx: Option<broadcast::Sender<Event>>,
}

struct ClientActor {
    options: ClientOptions,

    irc_b: broadcast::Sender<irc::Message>,
    irc_tx: mpsc::Sender<irc::Message>,
    irc_rx: mpsc::Receiver<irc::Message>,

    bot_b: broadcast::Sender<(Option<Channel>, bot::Message)>,

    action_rx: mpsc::Receiver<Action>,
    event_tx: broadcast::Sender<Event>,

    channels: HashMap<Channel, ChannelState>,
}

impl ClientActor {
    async fn process_irc(
        &mut self,
        bot_matcher: &bot::MessageMatcher<'_>,
        message: &irc::Message,
    ) -> crate::Result<()> {
        match &message.command {
            irc::Command::JOIN(channel) => {
                if let Some(channel) = Channel::try_from(channel.as_str()).ok() {
                    self.event_tx.send(Event::Join(channel.clone()))?;
                    if !self.channels.contains_key(&channel) {
                        self.channels.insert(channel.clone(), ChannelState {
                            channel,
                            topic: String::new(),
                            tx: None,
                        });
                    }
                }
            }
            irc::Command::Raw(code, params) => {
                if code == "332" && params.len() > 2 {
                    let channel = Channel::try_from(params[1].as_str()).ok();
                    if let Some(channel) = channel {
                        let topic = params[2].clone();
                        if let Some(state) = self.channels.get_mut(&channel) {
                            state.topic = topic;
                        }
                    }
                    return Ok(())
                }
            }
            irc::Command::PART(channel) => {
                if let Some(channel) = Channel::try_from(channel.as_str()).ok() {
                    self.event_tx.send(Event::Part(channel.clone()))?;
                    self.channels.remove(&channel);
                }
            }
            irc::Command::PRIVMSG { target, body } => {
                let channel = Channel::try_from(target.as_str()).ok();
                let mut is_bot = false;
                if let Some(irc::Prefix::User { nickname, .. }) = &message.prefix {
                    if nickname == self.options.bot.name.as_str() {
                        is_bot = true;
                    }
                }

                if is_bot {
                    match bot_matcher.matches(body.as_str()) {
                        Ok(message) => {
                            self.bot_b.send((channel.clone(), message.clone()))?;
                            if let Some(ref channel) = channel {
                                let event = match message {
                                    // Responses to commands, translate some of them to events
                                    bot::Message::MpAbortResponse => {
                                        Some(multiplayer::Event::MatchAborted)
                                    }

                                    // Events that we want to pass through
                                    bot::Message::PlayerJoinedEvent { user, slot, team } => {
                                        Some(multiplayer::Event::PlayerJoined { user, slot, team })
                                    }
                                    bot::Message::PlayerMovedEvent { user, slot } => {
                                        Some(multiplayer::Event::PlayerMoved { user, slot })
                                    }
                                    bot::Message::PlayerChangedTeamEvent { user, team } => {
                                        Some(multiplayer::Event::PlayerChangedTeam { user, team })
                                    }
                                    bot::Message::PlayerLeftEvent(user) => {
                                        Some(multiplayer::Event::PlayerLeft { user })
                                    }
                                    bot::Message::HostChangedEvent(user) => {
                                        Some(multiplayer::Event::HostChanged { host: user })
                                    }
                                    bot::Message::HostChangingMapEvent => {
                                        Some(multiplayer::Event::HostChangingMap)
                                    }
                                    bot::Message::MapChangedEvent(map) => {
                                        Some(multiplayer::Event::MapChanged { map })
                                    }
                                    bot::Message::PlayersReadyEvent => {
                                        Some(multiplayer::Event::PlayersReady)
                                    }
                                    bot::Message::MatchStartedEvent => {
                                        Some(multiplayer::Event::MatchStarted)
                                    }
                                    bot::Message::MatchFinishedEvent => {
                                        Some(multiplayer::Event::MatchFinished)
                                    }
                                    bot::Message::MatchPlayerScoreEvent { user, score, alive } => {
                                        Some(multiplayer::Event::MatchPlayerScore {
                                            user,
                                            score,
                                            alive,
                                        })
                                    }
                                    _ => None,
                                };
                                match event {
                                    Some(event) => {
                                        let event =
                                            Event::Multiplayer(channel.clone(), event.clone());
                                        self.event_tx.send(event.clone())?;
                                        self.channels.get(&channel).map(|state| {
                                            if let Some(tx) = &state.tx {
                                                tx.send(event);
                                            }
                                        });
                                    }
                                    _ => {}
                                };
                            }
                        }
                        Err(e) => {
                            println!("Error parsing bot message {}: {:?}", body, e);
                        }
                    }
                } else {
                    if let Some(irc::Prefix::User { nickname, .. }) = &message.prefix {
                        let event = Event::Message {
                            from: nickname.as_str().try_into()?,
                            channel: channel.clone(),
                            body: body.to_string(),
                        };
                        self.event_tx.send(event.clone())?;
                        if let Some(channel) = channel {
                            self.channels.get(&channel).map(|state| {
                                if let Some(tx) = &state.tx {
                                    tx.send(event);
                                }
                            });
                        }
                    }
                }
            }
            _ => {}
        };
        Ok(())
    }

    async fn process_action(&mut self, action: Action) -> crate::Result<()> {
        match action {
            Action::Channels(tx) => {
                let channels = self
                    .channels
                    .iter()
                    .map(|(channel, _)| channel.clone())
                    .collect::<Vec<_>>();
                tx.send(channels);
            }
            Action::Subscribe(channel, tx) => {
                if let Some(state) = self.channels.get_mut(&channel) {
                    match &state.tx {
                        Some(event_tx) => {
                            tx.send(Some(
                                (state.topic.clone(), event_tx.subscribe())
                            ));
                        },
                        None => {
                            let (event_tx, rx) = broadcast::channel(Client::CHANNEL_BUFFER);
                            tx.send(Some(
                                (state.topic.clone(), rx)
                            ));
                            state.tx = Some(event_tx);
                        }
                    }
                } else {
                    tx.send(None);
                }
            }
            Action::BotCommand {
                channel,
                command,
                tx,
            } => {
                self.irc_tx
                    .send(irc::Message {
                        prefix: None,
                        command: irc::Command::PRIVMSG {
                            target: channel
                                .clone()
                                .map_or(self.options.bot.irc_name(), |c| c.to_message_target()),
                            body: command.to_string(),
                        },
                    })
                    .await?;

                if let Some(tx) = tx {
                    let mut rx = self.bot_b.subscribe();
                    let mut fsm =
                        bot::ResponseMatcher::new(self.options.username.clone(), &command);
                    let operation_timeout = self.options.operation_timeout;
                    tokio::spawn(async move {
                        let start = Instant::now();
                        loop {
                            match time::timeout(operation_timeout, rx.recv()).await {
                                Ok(Ok((c, m))) => {
                                    if c == channel {
                                        if let Some(response) = fsm.next(&m) {
                                            tx.send(Ok(response.clone())).unwrap();
                                            break;
                                        } else if start.elapsed() > operation_timeout {
                                            tx.send(
                                                fsm.end().ok_or(Error::OperationTimeout.into()),
                                            )
                                            .unwrap();
                                            break;
                                        }
                                    }
                                }
                                Ok(Err(_)) => break,
                                Err(_) => {
                                    tx.send(fsm.end().ok_or(Error::OperationTimeout.into()))
                                        .unwrap();
                                    break;
                                }
                            }
                        }
                    });
                }
            }
            Action::Chat { target, body } => {
                self.irc_tx
                    .send(irc::Message {
                        prefix: None,
                        command: irc::Command::PRIVMSG {
                            target: target.clone(),
                            body: body.to_string(),
                        },
                    })
                    .await?;
            }
            Action::Raw(message) => {
                self.irc_tx.send(message.clone()).await?;
            }
        }
        Ok(())
    }

    async fn run(&mut self, mut transport: Transport<TcpStream>) -> crate::Result<()> {
        let mut pinger = time::interval(self.options.pinger_interval);
        let timeout = time::sleep(self.options.pinger_timeout);
        tokio::pin!(timeout);

        let ping_message = irc::Message {
            prefix: None,
            command: irc::Command::PING(self.options.username.clone(), None),
        };

        let bot_matcher = bot::MessageMatcher::new();

        loop {
            tokio::select! {
                // Pinger tick and timeout
                _ = pinger.tick() => {
                    transport.write(&ping_message).await?;
                },
                _ = &mut timeout => {
                    return Err(Error::Transport(TransportError::PingerTimeout).into());
                },
                // Read message from transport (IRC server) and send it to broadcast channel
                message = transport.read() => {
                    if let Ok(Some(message)) = message {
                        match &message.command {
                            // FIXME: We ignore QUIT messages to reduce the pressure of message passing.
                            //        This should be configurable in options.
                            irc::Command::QUIT(_) => {},
                            irc::Command::PING(s1, s2) => {
                                transport.write(&irc::Message::new(irc::Command::PONG(s1.clone(), s2.clone()), None)).await?;
                            },
                            irc::Command::PONG(..) => {
                                timeout.as_mut().reset(Instant::now() + self.options.pinger_timeout);
                            },
                            _ => {
                                self.process_irc(&bot_matcher, &message).await?;
                            }
                        }
                        self.irc_b.send(message)?;
                    } else {
                        return Err(Error::Transport(TransportError::ConnectionClosed).into());
                    }
                },
                // Collect messages from outgoing channel and write them to transport (IRC server)
                message = self.irc_rx.recv() => {
                    if let Some(message) = message {
                        transport.write(&message).await?;
                    } else {
                        return Err(Error::Transport(TransportError::ConnectionClosed).into());
                    }
                },
                action = self.action_rx.recv() => {
                    if let Some(action) = action {
                        self.process_action(action).await?;
                    } else {
                        return Err(Error::Transport(TransportError::ConnectionClosed).into());
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Client {
    options: ClientOptions,

    irc_tx: mpsc::Sender<irc::Message>,
    irc_rx: broadcast::Receiver<irc::Message>,

    bot_rx: broadcast::Receiver<(Option<Channel>, bot::Message)>,

    action_tx: mpsc::Sender<Action>,
    event_rx: broadcast::Receiver<Event>,

    actor: JoinHandle<()>,
}

impl Client {
    const CHANNEL_BUFFER: usize = 512;
    pub async fn new(options: ClientOptions) -> Result<Self> {
        let raw = TcpStream::connect(options.endpoint.clone()).await?;

        let (action_tx, action_rx) = mpsc::channel(Client::CHANNEL_BUFFER);
        let (irc_out_tx, irc_out_rx) = mpsc::channel(Client::CHANNEL_BUFFER);
        let (irc_in_tx, irc_in_rx) = broadcast::channel(Self::CHANNEL_BUFFER);
        let (bot_tx, bot_rx) = broadcast::channel(Self::CHANNEL_BUFFER);
        let (event_tx, event_rx) = broadcast::channel(Self::CHANNEL_BUFFER);
        let mut actor = ClientActor {
            options: options.clone(),

            irc_b: irc_in_tx,
            irc_rx: irc_out_rx,
            irc_tx: irc_out_tx.clone(),

            bot_b: bot_tx,
            action_rx,
            event_tx,
            channels: HashMap::new(),
        };

        Ok(Self {
            options,

            irc_tx: irc_out_tx,
            irc_rx: irc_in_rx,

            bot_rx,
            action_tx,
            event_rx,

            actor: tokio::spawn(
                async move { if let Err(e) = actor.run(Transport::new(raw)).await {} },
            ),
        })
    }

    fn bot(&self) -> User {
        self.options.bot.clone()
    }

    pub async fn auth(&mut self) -> crate::Result<()> {
        let username = &self.options.username;

        let mut rx = self.irc_rx.resubscribe();

        let mut list = Vec::new();
        if let Some(password) = &self.options.password {
            list.push(irc::Command::PASS(password.clone()))
        }
        list.append(&mut vec![
            irc::Command::USER {
                username: username.clone(),
                mode: "0".to_owned(),
                realname: username.clone(),
            },
            irc::Command::NICK(username.clone()),
        ]);

        for command in list {
            self.irc_tx.send(irc::Message::new(command, None)).await?;
        }

        tokio::time::timeout(self.options.operation_timeout, async {
            loop {
                match rx.recv().await {
                    Ok(message) => {
                        if let irc::Command::Raw(code, params) = message.command {
                            if code == "001" {
                                return Ok(());
                            } else if code == "464" {
                                return Err(Error::AuthError(
                                    params.get(1).map_or("".to_string(), |m| m.clone()),
                                )
                                .into());
                            }
                        }
                    }
                    _ => {}
                }
            }
        })
        .await
        .map_err(|_| Error::OperationTimeout)?
    }

    pub async fn channels(&self) -> crate::Result<Vec<Channel>> {
        let (tx, rx) = oneshot::channel();
        self.action_tx.send(Action::Channels(tx)).await?;
        Ok(rx.await?)
    }

    async fn subscribe(&self, channel: &Channel) -> crate::Result<Option<(String, broadcast::Receiver<Event>)>> {
        let (tx, rx) = oneshot::channel();
        self.action_tx.send(Action::Subscribe(channel.clone(), tx)).await?;
        Ok(rx.await?)
    }

    pub async fn join_channel_room(&mut self, channel: &Channel) -> crate::Result<ChannelRoom> {
        let entry = self.subscribe(channel).await?;
        if entry.is_none() {
            self.join(channel).await?;
        }

        let entry = self.subscribe(channel).await?;
        match entry {
            None => Err("Cannot join channel".into()),
            Some((topic, rx)) => Ok(ChannelRoom {
                topic,
                writer: ChannelSender { channel: channel.clone(), writer: self.writer() },
                event_rx: rx,
            }),
        }
    }

    pub async fn join_match(&mut self, id: u64) -> crate::Result<multiplayer::Match> {
        let channel = Channel::Multiplayer(id);
        let room = self.join_channel_room(&channel).await?;
        Ok(room.try_into()?)
    }

    pub async fn join(&mut self, channel: &Channel) -> crate::Result<()> {
        let command = irc::Command::JOIN(channel.to_string());
        let mut rx = self.irc_rx.resubscribe();
        self.irc_tx.send(irc::Message::new(command, None)).await?;

        time::timeout(self.options.operation_timeout, async {
            loop {
                let message = rx.recv().await?;
                match message.command {
                    irc::Command::Raw(code, params) => {
                        if code == "332" && params.len() > 2 && params[1] == channel.to_string() {
                            return Ok(())
                            
                        } else if code == "403"
                            && params.len() > 2
                            && params[1] == channel.to_string()
                        {
                            return Err(params[2].clone().into());
                        }
                    }
                    _ => {}
                }
            }
        })
        .await
        .map_err(|_| Error::OperationTimeout)?
    }

    pub async fn leave(&mut self, channel: &Channel) -> crate::Result<()> {
        let command = irc::Command::PART(channel.to_string());
        self.irc_tx.send(irc::Message::new(command, None)).await?;
        Ok(())
    }

    pub fn events(&self) -> broadcast::Receiver<Event> {
        self.event_rx.resubscribe()
    }

    pub fn writer(&self) -> Sender {
        Sender {
            tx: self.action_tx.clone(),
        }
    }

    pub fn irc_writer(&self) -> mpsc::Sender<irc::Message> {
        self.irc_tx.clone()
    }

    pub fn irc_subscriber(&self) -> broadcast::Receiver<irc::Message> {
        self.irc_rx.resubscribe()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Channel {
    Multiplayer(u64),
    Raw(String),
}

impl Channel {
    fn is_multiplayer(&self) -> bool {
        match self {
            Channel::Multiplayer(_) => true,
            _ => false,
        }
    }
}

impl irc::ToMessageTarget for Channel {
    fn to_message_target(&self) -> String {
        self.to_string()
    }
}

impl fmt::Display for Channel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Channel::Multiplayer(id) => write!(f, "#mp_{}", id),
            Channel::Raw(name) => write!(f, "#{}", name),
        }
    }
}

impl TryFrom<&str> for Channel {
    type Error = ConversionError;
    fn try_from(s: &str) -> StdResult<Self, Self::Error> {
        if !s.starts_with('#') {
            return Err(ConversionError::InvalidChannel);
        }
        let trim = s.trim_start_matches('#');
        if trim.starts_with("mp_") {
            let id = trim[3..]
                .parse()
                .map_err(|_| ConversionError::InvalidChannel)?;
            Ok(Channel::Multiplayer(id))
        } else {
            Ok(Channel::Raw(trim.to_string()))
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct User {
    id: u64,
    name: String,
}

impl User {
    pub fn merge(&self, other: &Self) -> Self {
        Self {
            id: if self.id != 0 { self.id } else { other.id },
            name: if self.name.len() != 0 {
                self.name.clone()
            } else {
                other.name.clone()
            },
        }
    }
}

impl PartialEq for User {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id || self.name.eq(&other.name) || self.irc_name().eq(&other.irc_name())
    }
}

impl fmt::Display for User {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.id == 0 {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{} (#{})", self.name, self.id)
        }
    }
}

impl User {
    pub fn id(&self) -> Option<u64> {
        if self.id == 0 {
            None
        } else {
            Some(self.id)
        }
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    // osu! client will highlight messages if they contain username,
    // to prevent this, a zero-width space is inserted between first two characters
    pub fn name_without_highlight(&self) -> String {
        let mut name = self.name.clone();
        if name.len() > 1 {
            name.insert(1, '\u{200b}');
        }
        name
    }

    // IRC name is almost same as osu! name, but all spaces are replaced with underscores
    pub fn irc_name(&self) -> String {
        self.name.replace(" ", "_")
    }
    pub fn to_parameter(&self) -> String {
        if self.id == 0 {
            self.name.clone()
        } else {
            format!("#{}", self.id)
        }
    }
}

impl irc::ToMessageTarget for User {
    fn to_message_target(&self) -> String {
        self.irc_name()
    }
}

impl From<u64> for User {
    fn from(id: u64) -> Self {
        User {
            id,
            name: String::new(),
        }
    }
}

impl TryFrom<&str> for User {
    type Error = ConversionError;
    fn try_from(name: &str) -> StdResult<Self, Self::Error> {
        if name.starts_with('#') {
            let id = name[1..]
                .parse()
                .map_err(|_| ConversionError::InvalidUser)?; // TODO: better error handling
            Ok(User {
                id,
                name: String::new(),
            })
        } else {
            Ok(User {
                id: 0,
                name: name.to_string(),
            })
        }
    }
}

#[derive(Debug, Clone)]
pub enum UserStatus {
    Offline,
    Afk,
    Idle,
    Playing,
    Lobby,
    Multiplayer,
    Multiplaying,
    Modding,
    Editing,
    Testing,
    Submitting,
    Watching,
}

impl Default for UserStatus {
    fn default() -> Self {
        UserStatus::Offline
    }
}

impl fmt::Display for UserStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UserStatus::Offline => write!(f, "Offline"),
            UserStatus::Afk => write!(f, "Afk"),
            UserStatus::Idle => write!(f, "Idle"),
            UserStatus::Playing => write!(f, "Playing"),
            UserStatus::Lobby => write!(f, "Lobby"),
            UserStatus::Multiplayer => write!(f, "Multiplayer"),
            UserStatus::Multiplaying => write!(f, "Multiplaying"),
            UserStatus::Modding => write!(f, "Modding"),
            UserStatus::Editing => write!(f, "Editing"),
            UserStatus::Testing => write!(f, "Testing"),
            UserStatus::Submitting => write!(f, "Submitting"),
            UserStatus::Watching => write!(f, "Watching"),
        }
    }
}

impl TryFrom<&str> for UserStatus {
    type Error = ConversionError;
    fn try_from(s: &str) -> StdResult<Self, Self::Error> {
        match s.to_lowercase().as_str() {
            "" => Ok(UserStatus::Offline),
            "afk" => Ok(UserStatus::Afk),
            "idle" => Ok(UserStatus::Idle),
            "playing" => Ok(UserStatus::Playing),
            "lobby" => Ok(UserStatus::Lobby),
            "multiplayer" => Ok(UserStatus::Multiplayer),
            "multiplaying" => Ok(UserStatus::Multiplaying),
            "modding" => Ok(UserStatus::Modding),
            "editing" => Ok(UserStatus::Editing),
            "testing" => Ok(UserStatus::Testing),
            "submitting" => Ok(UserStatus::Submitting),
            "watching" => Ok(UserStatus::Watching),
            _ => Err(ConversionError::InvalidUserStatus),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Map {
    id: u64,
    name: String,
}

impl Map {
    pub fn id(&self) -> u64 {
        self.id
    }
    pub fn name(&self) -> String {
        self.name.clone()
    }
}

impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Debug, Clone)]
pub enum Event {
    Join(Channel),
    Part(Channel),
    Message {
        channel: Option<Channel>,
        from: User,
        body: String,
    },
    Multiplayer(Channel, multiplayer::Event),
}

impl Event {
    pub fn is_join(&self) -> bool {
        match self {
            Event::Join(_) => true,
            _ => false,
        }
    }
    pub fn is_part(&self) -> bool {
        match self {
            Event::Part(_) => true,
            _ => false,
        }
    }
    pub fn is_message(&self) -> bool {
        match self {
            Event::Message { .. } => true,
            _ => false,
        }
    }
    pub fn is_channel_message(&self) -> bool {
        match self {
            Event::Message {
                channel: Some(_), ..
            } => true,
            _ => false,
        }
    }
    pub fn is_private_message(&self) -> bool {
        match self {
            Event::Message { channel: None, .. } => true,
            _ => false,
        }
    }

    pub fn is_multiplayer(&self) -> bool {
        match self {
            Event::Join(channel) => channel.is_multiplayer(),
            Event::Part(channel) => channel.is_multiplayer(),
            Event::Message {
                channel: Some(channel),
                ..
            } => channel.is_multiplayer(),
            Event::Multiplayer(..) => true,
            _ => false,
        }
    }

    pub fn channel(&self) -> Option<&Channel> {
        match self {
            Event::Join(channel) => Some(channel),
            Event::Part(channel) => Some(channel),
            Event::Message { channel, .. } => channel.as_ref(),
            Event::Multiplayer(channel, _) => Some(channel),
        }
    }

    pub fn is_from_channel(&self, channel: &Channel) -> bool {
        self.channel() == Some(channel)
    }
}

#[derive(Debug, Clone)]
pub enum Chat {
    Action(String),
    Raw(String),
}

impl Chat {
    pub fn new(body: &str) -> Self {
        Chat::Raw(body.to_string())
    }
    pub fn new_action(body: &str) -> Self {
        Chat::Action(body.to_string())
    }
    pub fn append(&mut self, body: &str) -> &mut Self {
        match self {
            Chat::Action(s) => s.push_str(body),
            Chat::Raw(s) => s.push_str(body),
        }
        self
    }
    pub fn append_link(&mut self, title: &str, url: &str) -> &mut Self {
        match self {
            Chat::Action(s) => s.push_str(&format!("({})[{}]", title, url)),
            Chat::Raw(s) => s.push_str(&format!("({})[{}]", title, url)),
        }
        self
    }
}

impl ToString for Chat {
    fn to_string(&self) -> String {
        match self {
            Chat::Action(body) => format!("\x01ACTION {}\x01", body),
            Chat::Raw(body) => body.clone(),
        }
    }
}

#[derive(Debug)]
pub struct ChannelRoom {
    topic: String,
    writer: ChannelSender,
    event_rx: broadcast::Receiver<Event>,
}

impl ChannelRoom {
    pub fn channel(&self) -> Channel {
        self.writer.channel.clone()
    }
    pub fn topic(&self) -> &str {
        self.topic.as_str()
    }
    pub fn channel_writer(&self) -> ChannelSender {
        self.writer.clone()
    }
    pub async fn send_chat(&mut self, body: &str) -> crate::Result<()> {
        self.writer.send_chat(body).await
    }
    pub fn events(&self) -> broadcast::Receiver<Event> {
        self.event_rx.resubscribe()
    }
}

impl TryInto<multiplayer::Match> for ChannelRoom {
    type Error = ConversionError;
    fn try_into(self) -> StdResult<multiplayer::Match, Self::Error> {
        if !self.writer.channel.is_multiplayer() {
            return Err(ConversionError::ChannelTypeMismatch);
        }
        let id = match self.writer.channel {
            Channel::Multiplayer(id) => id,
            _ => 0,
        };
        Ok(multiplayer::Match {
            id,
            inner_id: self.topic.trim_start_matches("multiplayer game #").parse().map_err(|_| ConversionError::InvalidMatchInnerId)?,
            writer: self.writer,
            event_rx: self.event_rx,
        })
    }
}

// impl ChannelRoom {
//     pub fn into_match(self) -> Std::Result<Match> {

//     }
// }


#[derive(Debug)]
enum Action {
    Channels(oneshot::Sender<Vec<Channel>>),
    Subscribe(Channel, oneshot::Sender<Option<(String, broadcast::Receiver<Event>)>>),
    BotCommand {
        channel: Option<Channel>,
        command: bot::Command,
        tx: Option<oneshot::Sender<crate::Result<bot::Response>>>,
    },
    Chat {
        target: String,
        body: String,
    },
    Raw(irc::Message),
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Default, Eq, Hash)]
    pub struct Mods: u32 {
        // const None          = 0x00000000;
        const NoFail        = 0x00000001;
        const Easy          = 0x00000002;
        const TouchDevice   = 0x00000004;
        const Hidden        = 0x00000008;
        const HardRock      = 0x00000010;
        const SuddenDeath   = 0x00000020;
        const DoubleTime    = 0x00000040;
        const Relax         = 0x00000080;
        const HalfTime      = 0x00000100;
        const Nightcore     = 0x00000200;
        const Flashlight    = 0x00000400;
        const Autoplay      = 0x00000800;
        const SpunOut       = 0x00001000;
        // const Relax2        = 0x00002000;
        const AutoPilot     = 0x00002000;
        const Perfect       = 0x00004000;
        const Key4          = 0x00008000;
        const Key5          = 0x00010000;
        const Key6          = 0x00020000;
        const Key7          = 0x00040000;
        const Key8          = 0x00080000;
        const FadeIn        = 0x00100000;
        const Random        = 0x00200000;
        const LastMod       = 0x00400000;
        const Key9          = 0x01000000;
        // const Key10         = 0x02000000;
        const KeyCoop       = 0x02000000;
        const Key1          = 0x04000000;
        const Key3          = 0x08000000;
        const Key2          = 0x10000000;
        const Mirror        = 0x40000000;
    }
}

impl Mods {
    fn display(&self) -> &'static str {
        match *self {
            m if m.is_empty() => "None",
            Mods::NoFail => "No Fail",
            Mods::Easy => "Easy",
            Mods::TouchDevice => "Touch Device",
            Mods::Hidden => "Hidden",
            Mods::HardRock => "Hard Rock",
            Mods::SuddenDeath => "Sudden Death",
            Mods::DoubleTime => "Double Time",
            Mods::Relax => "Relax",
            Mods::HalfTime => "Half Time",
            Mods::Nightcore => "Nightcore",
            Mods::Flashlight => "Flashlight",
            Mods::Autoplay => "Auto",
            Mods::SpunOut => "Spun Out",
            Mods::AutoPilot => "Auto Pilot",
            Mods::Perfect => "Perfect",
            Mods::Key4 => "Key4",
            Mods::Key5 => "Key5",
            Mods::Key6 => "Key6",
            Mods::Key7 => "Key7",
            Mods::Key8 => "Key8",
            Mods::FadeIn => "FadeIn",
            Mods::Random => "Random",
            Mods::LastMod => "Last Mod",
            Mods::Key9 => "Key9",
            Mods::KeyCoop => "KeyCoop",
            Mods::Key1 => "Key1",
            Mods::Key3 => "Key3",
            Mods::Key2 => "Key2",
            Mods::Mirror => "Mirror",
            _ => unreachable!("Invalid single mod"),
        }
    }
    fn name(&self) -> &'static str {
        match *self {
            m if m.is_empty() => "None",
            Mods::NoFail => "NoFail",
            Mods::Easy => "Easy",
            Mods::TouchDevice => "TouchDevice",
            Mods::Hidden => "Hidden",
            Mods::HardRock => "HardRock",
            Mods::SuddenDeath => "SuddenDeath",
            Mods::DoubleTime => "DoubleTime",
            Mods::Relax => "Relax",
            Mods::HalfTime => "HalfTime",
            Mods::Nightcore => "Nightcore",
            Mods::Flashlight => "Flashlight",
            Mods::Autoplay => "Auto",
            Mods::SpunOut => "SpunOut",
            Mods::AutoPilot => "Relax2",
            Mods::Perfect => "Perfect",
            Mods::Key4 => "Key4",
            Mods::Key5 => "Key5",
            Mods::Key6 => "Key6",
            Mods::Key7 => "Key7",
            Mods::Key8 => "Key8",
            Mods::FadeIn => "FadeIn",
            Mods::Random => "Random",
            Mods::LastMod => "LastMod",
            Mods::Key9 => "Key9",
            Mods::KeyCoop => "Key Coop",
            Mods::Key1 => "Key1",
            Mods::Key3 => "Key3",
            Mods::Key2 => "Key2",
            Mods::Mirror => "Mirror",
            _ => unreachable!("Invalid single mod"),
        }
    }
    fn short_name(&self) -> &'static str {
        match *self {
            m if m.is_empty() => "none",
            Mods::NoFail => "nf",
            Mods::Easy => "ez",
            Mods::TouchDevice => "td",
            Mods::Hidden => "hd",
            Mods::HardRock => "hr",
            Mods::SuddenDeath => "sd",
            Mods::DoubleTime => "dt",
            Mods::Relax => "rx",
            Mods::HalfTime => "ht",
            Mods::Nightcore => "nc",
            Mods::Flashlight => "fl",
            Mods::Autoplay => "at",
            Mods::SpunOut => "so",
            Mods::AutoPilot => "ap",
            Mods::Perfect => "pf",
            Mods::Key4 => "k4",
            Mods::Key5 => "k5",
            Mods::Key6 => "k6",
            Mods::Key7 => "k7",
            Mods::Key8 => "k8",
            Mods::FadeIn => "fi",
            Mods::Random => "rn",
            Mods::LastMod => "lm",
            Mods::Key9 => "k9",
            Mods::KeyCoop => "k10",
            Mods::Key1 => "k1",
            Mods::Key3 => "k3",
            Mods::Key2 => "k2",
            Mods::Mirror => "mi",
            _ => unreachable!("Invalid single mod"),
        }
    }
}

impl fmt::Display for Mods {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.iter();
        if let Some(first) = iter.next() {
            write!(f, "{}", first.display())?;
            for m in iter {
                write!(f, ", {}", m.display())?;
            }
        }
        Ok(())
    }
}

impl TryFrom<&str> for Mods {
    type Error = ConversionError;
    fn try_from(s: &str) -> StdResult<Self, Self::Error> {
        Self::all()
            .iter()
            .filter(|m| {
                m.name().eq_ignore_ascii_case(s)
                    || m.display().eq_ignore_ascii_case(s)
                    || m.short_name().eq_ignore_ascii_case(s)
            })
            .next()
            .ok_or(ConversionError::InvalidModName)
    }
}
