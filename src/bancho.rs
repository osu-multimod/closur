pub mod bot;
mod cache;
pub mod irc;
pub mod multiplayer;

use regex::Regex;
use tokio::net::TcpStream;
use tokio::sync::{broadcast, mpsc, oneshot};
use tokio::task::JoinHandle;
use tokio::time::{self, Duration, Instant};

use std::borrow::Borrow;
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::pin::Pin;
use std::str::FromStr;

use self::cache::UserCache;
use self::irc::Transport;
use self::multiplayer::{MatchId, MatchInternalId};

#[derive(Debug)]
pub enum TrackVerdict<T, E> {
    Skip,
    Terminate,
    Accept,
    Body(T),
    Err(E),
}

pub trait IrcCommand {
    type Body;
    type Error;
    fn command(&self) -> Option<irc::Command> {
        None
    }
    fn commands(&self) -> Vec<irc::Command> {
        Vec::new()
    }
    fn tracks_message(
        &self,
        context: &mut IrcContext,
        message: irc::Message,
    ) -> TrackVerdict<Self::Body, Self::Error>;
}

#[derive(Debug)]
pub struct IrcContext<'a> {
    handle: &'a Client,
    history: Vec<irc::Message>,
}

#[derive(Debug)]
pub struct IrcResponse<T: IrcCommand> {
    body: StdResult<T::Body, T::Error>,
}

impl<T: IrcCommand> IrcResponse<T> {
    pub fn body(&self) -> StdResult<&T::Body, &T::Error> {
        self.body.as_ref()
    }
}

impl<'a> IrcContext<'a> {
    fn new(handle: &'a Client) -> Self {
        Self {
            handle,
            history: Vec::new(),
        }
    }
    pub fn options(&self) -> &ClientOptions {
        &self.handle.options
    }
    pub fn history(&self) -> &[irc::Message] {
        &self.history
    }
    pub fn push(&mut self, message: impl Into<irc::Message>) {
        self.history.push(message.into());
    }
}

#[derive(Debug)]
pub struct Auth {
    username: String,
    password: Option<String>,
}

impl IrcCommand for Auth {
    type Body = ();
    type Error = String;
    fn commands(&self) -> Vec<irc::Command> {
        let mut commands = match &self.password {
            Some(password) => vec![irc::Command::PASS(password.clone())],
            None => Vec::new(),
        };
        commands.extend_from_slice(&[
            irc::Command::USER {
                username: self.username.clone(),
                mode: "0".to_string(),
                realname: self.username.clone(),
            },
            irc::Command::NICK(self.username.clone()),
        ]);
        commands
    }
    fn tracks_message(
        &self,
        _context: &mut IrcContext,
        message: irc::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.prefix.map(|p| p.is_server()).unwrap_or_default() {
            match message.command {
                irc::Command::Response(code, mut params) => match code {
                    irc::Response::RPL_WELCOME => TrackVerdict::Body(()),
                    irc::Response::ERR_PASSWDMISMATCH => {
                        TrackVerdict::Err(params.pop().unwrap_or_default())
                    }
                    _ => TrackVerdict::Skip,
                },
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}

#[derive(Debug)]
pub struct Join {
    channel: Channel,
}

impl IrcCommand for Join {
    type Error = JoinError;
    type Body = ChannelInfo;
    fn command(&self) -> Option<irc::Command> {
        Some(irc::Command::JOIN(self.channel.to_string()))
    }
    fn tracks_message(
        &self,
        context: &mut IrcContext,
        message: irc::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message
            .prefix
            .as_ref()
            .map(|p| {
                p.is_user() && { User::irc_normalize(&context.options().username).eq(p.name()) }
            })
            .unwrap_or_default()
        {
            match &message.command {
                irc::Command::JOIN(c) if self.channel.to_string().eq(c) => {
                    context.push(message);
                    TrackVerdict::Accept
                }
                _ => TrackVerdict::Skip,
            }
        } else if message
            .prefix
            .as_ref()
            .map(|p| p.is_server())
            .unwrap_or_default()
        {
            match message.command {
                irc::Command::Response(irc::Response::ERR_NOSUCHCHANNEL, mut params)
                    if self.channel.to_string().eq(&params[1]) =>
                {
                    TrackVerdict::Err(JoinError::NotFound(params.pop().unwrap()))
                }
                irc::Command::Response(
                    irc::Response::RPL_TOPIC | irc::Response::RPL_TOPICWHOTIME,
                    ref params,
                ) if self.channel.to_string().eq(&params[1]) => {
                    context.push(message);
                    TrackVerdict::Accept
                }
                irc::Command::Response(irc::Response::RPL_NAMREPLY, ref params)
                    if self.channel.to_string().eq(&params[2]) =>
                {
                    context.push(message);
                    TrackVerdict::Accept
                }
                irc::Command::Response(irc::Response::RPL_ENDOFNAMES, ref params)
                    if self.channel.to_string().eq(&params[1]) =>
                {
                    context.push(message);
                    let info = ChannelInfo::compose(&self.channel, context.history());
                    TrackVerdict::Body(info)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}

#[derive(Debug, Clone)]
pub enum JoinError {
    NotFound(String),
}

impl fmt::Display for JoinError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            JoinError::NotFound(channel) => write!(f, "channel not found: {}", channel),
        }
    }
}
impl StdError for JoinError {}

#[derive(Debug)]
pub struct Part {
    channel: Channel,
}

impl IrcCommand for Part {
    type Body = ();
    type Error = PartError;
    fn command(&self) -> Option<irc::Command> {
        Some(irc::Command::PART(self.channel.to_string()))
    }
    fn tracks_message(
        &self,
        context: &mut IrcContext,
        message: irc::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message
            .prefix
            .as_ref()
            .map(|p| {
                p.is_user() && { User::irc_normalize(&context.options().username).eq(p.name()) }
            })
            .unwrap_or_default()
        {
            match &message.command {
                irc::Command::PART(c) if self.channel.to_string().eq(c) => TrackVerdict::Body(()),
                _ => TrackVerdict::Skip,
            }
        } else if message
            .prefix
            .as_ref()
            .map(|p| p.is_server())
            .unwrap_or_default()
        {
            match message.command {
                irc::Command::Response(irc::Response::ERR_NOSUCHCHANNEL, mut params)
                    if self.channel.to_string().eq(&params[1]) =>
                {
                    TrackVerdict::Err(PartError::NotFound(params.pop().unwrap()))
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}

#[derive(Debug, Clone)]
pub enum PartError {
    NotFound(String),
}

impl fmt::Display for PartError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PartError::NotFound(channel) => write!(f, "channel not found: {}", channel),
        }
    }
}
impl StdError for PartError {}

#[derive(Debug)]
pub struct Whois {
    username: String,
}

impl IrcCommand for Whois {
    type Error = WhoisError;
    type Body = User;
    fn command(&self) -> Option<irc::Command> {
        Some(irc::Command::WHOIS(self.username.to_string()))
    }
    fn tracks_message(
        &self,
        context: &mut IrcContext,
        message: irc::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.prefix().map(|p| p.is_server()).unwrap_or_default() {
            match &message.command {
                irc::Command::Response(irc::Response::RPL_WHOISUSER, ref params)
                    if self.username.eq(&params[1]) =>
                {
                    let id = Matcher::whois_user_id(&params[2]).unwrap();
                    context.push(message);
                    TrackVerdict::Body(User::new(id, &self.username))
                }
                irc::Command::Response(irc::Response::RPL_ENDOFNAMES, ref params)
                    if self.username.eq(&params[1]) =>
                {
                    context.push(message);
                    TrackVerdict::Err(WhoisError::NotFound(self.username.clone()))
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}

#[derive(Debug, Clone)]
pub enum WhoisError {
    NotFound(String),
}

impl fmt::Display for WhoisError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            WhoisError::NotFound(username) => write!(f, "user not found: {}", username),
        }
    }
}
impl StdError for WhoisError {}

#[derive(Debug, Clone)]
pub struct ChannelInfo {
    channel: Channel,
    topic: String,
    match_internal_id: Option<MatchInternalId>,
    time: chrono::DateTime<chrono::Utc>,
    names: Vec<User>,
}

impl ChannelInfo {
    fn compose(channel: impl Borrow<Channel>, messages: &[irc::Message]) -> Self {
        let mut info = ChannelInfo {
            channel: channel.borrow().clone(),
            topic: String::new(),
            match_internal_id: None,
            time: chrono::DateTime::default(),
            names: Vec::new(),
        };
        for message in messages {
            match &message.command {
                irc::Command::Response(irc::Response::RPL_TOPIC, params) => {
                    info.topic = params.last().cloned().unwrap_or_default();
                    if channel.borrow().is_multiplayer() {
                        info.match_internal_id = Matcher::topic_game_id(&info.topic);
                    }
                }
                irc::Command::Response(irc::Response::RPL_TOPICWHOTIME, params) => {
                    let timestamp: i64 = params
                        .last()
                        .cloned()
                        .unwrap_or_default()
                        .parse()
                        .unwrap_or_default();
                    info.time = chrono::DateTime::from_naive_utc_and_offset(
                        chrono::NaiveDateTime::from_timestamp_millis(timestamp * 1000).unwrap(),
                        chrono::Utc,
                    );
                }
                irc::Command::Response(irc::Response::RPL_NAMREPLY, params) => info.names.extend(
                    params
                        .last()
                        .cloned()
                        .unwrap_or_default()
                        .split_terminator(' ')
                        .map(|s| User::name_only(s.trim().trim_start_matches("@+"))),
                ),
                _ => {}
            };
        }
        info.names.shrink_to_fit();
        info
    }
    pub fn channel(&self) -> &Channel {
        &self.channel
    }
    pub fn users(&self) -> &[User] {
        &self.names
    }
    pub fn topic(&self) -> &str {
        &self.topic
    }
    pub fn created_at(&self) -> chrono::DateTime<chrono::Utc> {
        self.time
    }
    pub fn match_internal_id(&self) -> MatchInternalId {
        self.match_internal_id.unwrap_or_default()
    }
}

use crate::{
    error::{ConversionError, StdError},
    StdResult,
};

use bitflags::bitflags;

type Result<T> = StdResult<T, Error>;

#[derive(Debug)]
pub enum Error {
    PingerTimeout,
    AuthError(String),
    Irc(irc::Error),
    Io(tokio::io::Error),
}

impl StdError for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::PingerTimeout => write!(f, "pinger timeout"),
            Error::AuthError(e) => write!(f, "authenticate error: {}", e),
            Error::Irc(e) => write!(f, "irc error: {}", e),
            Error::Io(e) => write!(f, "io error: {}", e),
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

#[derive(Debug)]
pub enum OperatorError<T> {
    Timeout,
    TrackerLimitExceeded,
    Cancelled,
    Bot(bot::CommandError),
    Queue(QueueError<T>),
}

#[derive(Debug)]
pub enum QueueError<T> {
    SendError(T),
    RecvClosed,
    RecvLagged(u64),
}

impl<T> fmt::Display for OperatorError<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OperatorError::Timeout => write!(f, "operation timeout"),
            OperatorError::TrackerLimitExceeded => write!(f, "message tracker limit exceeded"),
            OperatorError::Cancelled => write!(f, "operation cancelled"),
            OperatorError::Bot(e) => write!(f, "bot error: {}", e),
            OperatorError::Queue(e) => write!(f, "queue error: {}", e),
        }
    }
}

impl<T: fmt::Debug> StdError for OperatorError<T> {}

impl<T> fmt::Display for QueueError<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            QueueError::SendError(_) => write!(f, "send error"),
            QueueError::RecvClosed => write!(f, "receive channel closed"),
            QueueError::RecvLagged(n) => write!(f, "receive channel lagged: skipped {}", n),
        }
    }
}

impl<T: fmt::Debug> StdError for QueueError<T> {}

impl<T> From<mpsc::error::SendError<T>> for QueueError<T> {
    fn from(e: mpsc::error::SendError<T>) -> Self {
        Self::SendError(e.0)
    }
}

impl<T> From<broadcast::error::RecvError> for QueueError<T> {
    fn from(e: broadcast::error::RecvError) -> Self {
        match e {
            broadcast::error::RecvError::Closed => Self::RecvClosed,
            broadcast::error::RecvError::Lagged(n) => Self::RecvLagged(n),
        }
    }
}

impl<T> From<mpsc::error::SendError<T>> for OperatorError<T> {
    fn from(e: mpsc::error::SendError<T>) -> Self {
        Self::Queue(e.into())
    }
}

impl<T> From<broadcast::error::RecvError> for OperatorError<T> {
    fn from(e: broadcast::error::RecvError) -> Self {
        Self::Queue(e.into())
    }
}

impl<T> From<bot::CommandError> for OperatorError<T> {
    fn from(e: bot::CommandError) -> Self {
        Self::Bot(e)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Message {
    sender: User,
    channel: Option<Channel>,
    content: String,
}

impl Message {
    /// Returns the sender of the message.
    pub fn user(&self) -> &User {
        &self.sender
    }
    /// Returns the channel where the message is sent.
    pub fn channel(&self) -> Option<&Channel> {
        self.channel.as_ref()
    }
    /// Returns the content of the message.
    pub fn content(&self) -> &str {
        &self.content
    }
    /// Checks if the message is sent directly.
    pub fn is_private(&self) -> bool {
        self.channel.is_none()
    }
    /// Checks if the message is sent in a channel.
    pub fn is_public(&self) -> bool {
        self.channel.is_some()
    }
}

#[derive(Debug, Clone)]
pub struct ChatBuilder(String);

impl ChatBuilder {
    pub fn new() -> Self {
        Self(String::new())
    }
    pub fn push(&mut self, content: impl AsRef<str>) -> &mut Self {
        self.0.push_str(content.as_ref());
        self
    }
    pub fn push_link(&mut self, title: impl AsRef<str>, url: impl AsRef<str>) -> &mut Self {
        use std::fmt::Write;
        write!(self.0, "({})[{}]", title.as_ref(), url.as_ref()).unwrap();
        self
    }
    pub fn chat(&self) -> String {
        self.0.clone()
    }
    pub fn action(&self) -> String {
        format!("\x01ACTION {}\x01", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Event {
    kind: EventKind,
    instant: Instant,
    time: chrono::DateTime<chrono::Utc>,
}

impl Event {
    pub fn kind(&self) -> &EventKind {
        &self.kind
    }
    pub fn instant(&self) -> Instant {
        self.instant
    }
    pub fn time(&self) -> chrono::DateTime<chrono::Utc> {
        self.time
    }
    pub fn relates_to_channel(&self, channel: &Channel) -> bool {
        self.kind.relates_to_channel(channel)
    }
    pub fn relates_to_match(&self, match_id: MatchId) -> bool {
        self.kind.relates_to_match(match_id)
    }
}

impl From<Message> for Event {
    fn from(message: Message) -> Self {
        Event {
            kind: message.into(),
            instant: Instant::now(),
            time: chrono::Utc::now(),
        }
    }
}

impl From<bot::Message> for Event {
    fn from(message: bot::Message) -> Self {
        Event {
            kind: message.into(),
            instant: Instant::now(),
            time: chrono::Utc::now(),
        }
    }
}

impl From<multiplayer::Event> for Event {
    fn from(event: multiplayer::Event) -> Self {
        Event {
            kind: event.into(),
            instant: Instant::now(),
            time: chrono::Utc::now(),
        }
    }
}

impl From<EventKind> for Event {
    fn from(kind: EventKind) -> Self {
        Event {
            kind,
            instant: Instant::now(),
            time: chrono::Utc::now(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EventKind {
    Quit(User),
    Join(Channel),
    Part(Channel),
    Message(Message),
    Bot(bot::Message),

    Multiplayer(multiplayer::Event),

    /// Maintenance alerts and countdown timers.
    Maintenance(Option<Duration>),

    /// The peer connection is closed.
    Closed,
}

impl EventKind {
    pub fn relates_to_channel(&self, channel: &Channel) -> bool {
        match self {
            EventKind::Join(c) | EventKind::Part(c) => c == channel,
            EventKind::Message(m) => m.channel() == Some(channel),
            EventKind::Bot(m) => m.channel() == Some(channel),
            EventKind::Multiplayer(m) => &m.channel() == channel,
            _ => false,
        }
    }
    pub fn relates_to_match(&self, match_id: MatchId) -> bool {
        self.relates_to_channel(&Channel::Multiplayer(match_id))
    }
}

impl From<Message> for EventKind {
    fn from(message: Message) -> Self {
        EventKind::Message(message)
    }
}

impl From<bot::Message> for EventKind {
    fn from(message: bot::Message) -> Self {
        EventKind::Bot(message)
    }
}

impl From<multiplayer::Event> for EventKind {
    fn from(event: multiplayer::Event) -> Self {
        EventKind::Multiplayer(event)
    }
}

struct Matcher {}
impl Matcher {
    // const MULTIPLAYER_CHANNEL_TOPIC: Regex = Regex::new(r"^multiplayer game #(\d+)$").unwrap();
    // const WHOIS_URL: Regex = Regex::new(r"^https?://osu.ppy.sh/u/(\d+)$").unwrap();
    fn topic_game_id(s: &str) -> Option<MatchInternalId> {
        let pattern = Regex::new(r"^multiplayer game #(\d+)$").unwrap();
        pattern
            .captures(s)
            .and_then(|c| c.get(1))
            .and_then(|m| m.as_str().parse().ok())
    }
    fn whois_user_id(s: &str) -> Option<UserId> {
        let pattern = Regex::new(r"^https?://osu.ppy.sh/u/(\d+)$").unwrap();
        pattern
            .captures(s)
            .and_then(|c| c.get(1))
            .and_then(|m| m.as_str().parse().ok())
    }
}

#[derive(Debug, Clone)]
pub struct ClientOptions {
    // basic IRC options
    /// Endpoint of IRC server, in form of `host:port`
    endpoint: String,
    /// Username of IRC credentials
    username: String,
    /// (Optional) Password of IRC credentials
    password: Option<String>,

    /// BanchoBot user instance
    bot: User,

    /// Operation timeout
    operation_timeout: Duration,
    /// Interval of sending IRC command `PING` to keepalive
    pinger_interval: Duration,
    /// Timeout of receiving IRC command `PONG` to keepalive
    pinger_timeout: Duration,

    message_tracker_limit: usize,
    // TODO: implement this
    // ignore_irc_quit: bool,

    // TODO: implement this
    // ignore BanchoBot's raw IRC messages
    // ignore_irc_from_banchobot: bool,
}

impl ClientOptions {
    pub fn endpoint(&self) -> &str {
        &self.endpoint
    }
    pub fn username(&self) -> &str {
        &self.username
    }
    pub fn password(&self) -> Option<&str> {
        self.password.as_ref().map(|p| p.as_str())
    }
    pub fn bot(&self) -> &User {
        &self.bot
    }
    pub fn operation_timeout(&self) -> Duration {
        self.operation_timeout
    }
    pub fn pinger_interval(&self) -> Duration {
        self.pinger_interval
    }
    pub fn pinger_timeout(&self) -> Duration {
        self.pinger_timeout
    }
    pub fn message_tracker_limit(&self) -> usize {
        self.message_tracker_limit
    }
}

impl Default for ClientOptions {
    fn default() -> Self {
        Self {
            endpoint: "irc.ppy.sh:6667".to_owned(),
            username: "".to_owned(),
            password: None,
            bot: User {
                prefer_id: false,
                id: Some(3),
                name: "BanchoBot".to_string(),
                flags: UserFlags::BanchoBot,
            },
            operation_timeout: Duration::from_secs(3),
            pinger_interval: Duration::from_secs(15),
            pinger_timeout: Duration::from_secs(30),
            message_tracker_limit: 48,
        }
    }
}

#[derive(Debug, Default)]
pub struct ClientOptionsBuilder(ClientOptions);

impl ClientOptionsBuilder {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn endpoint(&mut self, endpoint: String) -> &mut Self {
        self.0.endpoint = endpoint;
        self
    }
    pub fn username(&mut self, username: String) -> &mut Self {
        self.0.username = username;
        self
    }
    pub fn password(&mut self, password: String) -> &mut Self {
        self.0.password = Some(password);
        self
    }
    pub fn bot_user(&mut self, bot: User) -> &mut Self {
        self.0.bot = bot;
        self
    }
    pub fn operation_timeout(&mut self, timeout: Duration) -> &mut Self {
        self.0.operation_timeout = timeout;
        self
    }
    pub fn pinger_interval(&mut self, timeout: Duration) -> &mut Self {
        self.0.pinger_interval = timeout;
        self
    }
    pub fn pinger_timeout(&mut self, timeout: Duration) -> &mut Self {
        self.0.pinger_timeout = timeout;
        self
    }
    pub fn build(&mut self) -> ClientOptions {
        self.0.clone()
    }
}

#[derive(Debug)]
pub struct Operator<'a> {
    handle: &'a Client,
    tx: mpsc::Sender<Action>,
    rx: broadcast::Receiver<Event>,
    irc_rx: broadcast::Receiver<irc::Message>,
}

impl<'a> Operator<'a> {
    /// Sends a raw IRC command to the server.
    ///
    /// This method is unreliable, because it does not track the response. To
    /// track the response, use [`Operator::send_irc_command`] instead.
    pub async fn send_irc_command_unreliable<C: IrcCommand>(
        &self,
        command: C,
    ) -> StdResult<(), OperatorError<Action>> {
        match command.command() {
            Some(command) => self.tx.send(Action::Raw(command)).await?,
            None => self.tx.send(Action::RawGroup(command.commands())).await?,
        };
        Ok(())
    }
    /// Sends a raw IRC command to the server.
    ///
    /// This method tracks the response and thus generally costs more time than
    /// [`Operator::send_irc_command_unreliable`].
    pub async fn send_irc_command<C: IrcCommand>(
        &self,
        command: C,
    ) -> StdResult<IrcResponse<C>, OperatorError<Action>> {
        let mut irc_rx = self.irc_rx.resubscribe();
        match command.command() {
            Some(command) => self.tx.send(Action::Raw(command)).await?,
            None => self.tx.send(Action::RawGroup(command.commands())).await?,
        };
        let mut context = IrcContext::new(self.handle);

        let mut skip_count: usize = 0;

        loop {
            let message = irc_rx.recv().await?;
            match command.tracks_message(&mut context, message) {
                TrackVerdict::Accept => continue,
                TrackVerdict::Body(body) => return Ok(IrcResponse { body: Ok(body) }),
                TrackVerdict::Err(e) => return Ok(IrcResponse { body: Err(e) }),
                TrackVerdict::Skip => {
                    skip_count += 1;
                    if skip_count > self.options().message_tracker_limit {
                        break Err(OperatorError::TrackerLimitExceeded);
                    }
                }
                TrackVerdict::Terminate => break Err(OperatorError::Cancelled),
            }
        }
    }
    async fn send_action(&self, action: Action) -> StdResult<(), OperatorError<Action>> {
        Ok(self.tx.send(action).await?)
    }
    pub fn options(&'a self) -> &'a ClientOptions {
        &self.handle.options
    }
    /// Subscribes to the event stream.
    pub fn subscribe(&self) -> broadcast::Receiver<Event> {
        self.rx.resubscribe()
    }
    /// Subscribes to the raw IRC message stream.
    pub fn irc(&self) -> broadcast::Receiver<irc::Message> {
        self.irc_rx.resubscribe()
    }
    async fn send_target_chats<S: AsRef<str>>(
        &self,
        target: impl Into<MessageTarget>,
        contents: impl AsRef<[S]>,
    ) -> StdResult<(), OperatorError<Action>> {
        let target = User::irc_normalize(&target.into().to_string());

        let messages = contents
            .as_ref()
            .iter()
            .map(|line| irc::Command::PRIVMSG {
                target: target.clone(),
                body: line.as_ref().to_string(),
            })
            .collect();
        self.send_action(Action::RawGroup(messages)).await?;
        Ok(())
    }
    pub async fn send_user_chats<S: AsRef<str>>(
        &self,
        user: impl Borrow<User>,
        contents: impl AsRef<[S]>,
    ) -> StdResult<(), OperatorError<Action>> {
        self.send_target_chats(user.borrow().clone(), contents)
            .await
    }
    pub async fn send_channel_chats<S: AsRef<str>>(
        &self,
        channel: impl Borrow<Channel>,
        contents: impl AsRef<[S]>,
    ) -> StdResult<(), OperatorError<Action>> {
        self.send_target_chats(channel.borrow().clone(), contents)
            .await
    }
    async fn send_target_chat(
        &self,
        target: impl Into<MessageTarget>,
        content: impl AsRef<str>,
    ) -> StdResult<(), OperatorError<Action>> {
        let action = Action::Raw(irc::Command::PRIVMSG {
            target: User::irc_normalize(&target.into().to_string()),
            body: content.as_ref().to_string(),
        });
        self.send_action(action).await
    }
    pub async fn send_user_chat(
        &self,
        user: impl Borrow<User>,
        content: impl AsRef<str>,
    ) -> StdResult<(), OperatorError<Action>> {
        let content = content.as_ref();
        self.send_target_chat(user.borrow().clone(), content).await
    }
    pub async fn send_channel_chat(
        &self,
        channel: impl Borrow<Channel>,
        content: impl AsRef<str>,
    ) -> StdResult<(), OperatorError<Action>> {
        let content = content.as_ref();
        self.send_target_chat(channel.borrow().clone(), content)
            .await
    }
    async fn send_target_bot_command_unreliable<C: bot::Command>(
        &self,
        target: impl Into<MessageTarget>,
        command: &C,
    ) -> StdResult<(), OperatorError<Action>> {
        let target = target.into();
        if match &target {
            MessageTarget::User(u) => command.sendable_to_user(u),
            MessageTarget::Channel(c) => command.sendable_in_channel(c),
        } {
            self.tx
                .send(Action::Raw(irc::Command::PRIVMSG {
                    target: target.to_string(),
                    body: command.command_string(),
                }))
                .await?;
            Ok(())
        } else {
            Err(match target {
                MessageTarget::User(u) => bot::CommandError::UserNotApplicable(u),
                MessageTarget::Channel(c) => bot::CommandError::ChannelNotApplicable(c),
            }
            .into())
        }
    }
    async fn send_target_bot_command<C: bot::Command>(
        &self,
        target: impl Into<MessageTarget>,
        command: &C,
    ) -> StdResult<bot::Response<C>, OperatorError<Action>> {
        let target = target.into();
        if match &target {
            MessageTarget::User(u) => command.sendable_to_user(u),
            MessageTarget::Channel(c) => command.sendable_in_channel(c),
        } {
            let mut rx = self.rx.resubscribe();
            self.tx
                .send(Action::Raw(irc::Command::PRIVMSG {
                    target: target.to_string(),
                    body: command.command_string(),
                }))
                .await?;
            let mut context =
                bot::CommandContext::new(User::name_only(self.options().username()), target);

            let mut skip_count: usize = 0;

            loop {
                let event = rx.recv().await?;
                match event.kind {
                    EventKind::Bot(message) => {
                        match command.tracks_message(&mut context, message) {
                            TrackVerdict::Accept => continue,
                            TrackVerdict::Body(body) => {
                                return Ok(bot::Response {
                                    command: command.clone(),
                                    messages: context.history().to_vec(),
                                    body: Ok(body),
                                })
                            }
                            TrackVerdict::Err(e) => {
                                return Ok(bot::Response {
                                    command: command.clone(),
                                    messages: context.history().to_vec(),
                                    body: Err(e),
                                })
                            }
                            TrackVerdict::Skip => {
                                skip_count += 1;
                                if skip_count > self.options().message_tracker_limit {
                                    break Err(OperatorError::TrackerLimitExceeded);
                                }
                            }
                            TrackVerdict::Terminate => break Err(OperatorError::Cancelled),
                        }
                    }
                    _ => {
                        skip_count += 1;
                        if skip_count > self.options().message_tracker_limit {
                            break Err(OperatorError::TrackerLimitExceeded);
                        }
                    }
                }
            }
        } else {
            Err(match target {
                MessageTarget::User(u) => bot::CommandError::UserNotApplicable(u),
                MessageTarget::Channel(c) => bot::CommandError::ChannelNotApplicable(c),
            }
            .into())
        }
    }
    pub async fn send_bot_command<C: bot::Command>(
        &self,
        channel: Option<Channel>,
        command: impl Borrow<C>,
    ) -> StdResult<bot::Response<C>, OperatorError<Action>> {
        match channel {
            None => {
                self.send_target_bot_command(self.options().bot.clone(), command.borrow())
                    .await
            }
            Some(channel) => {
                self.send_target_bot_command(channel, command.borrow())
                    .await
            }
        }
    }
    pub async fn send_bot_command_unreliable<C: bot::Command>(
        &self,
        channel: Option<Channel>,
        command: impl Borrow<C>,
    ) -> StdResult<(), OperatorError<Action>> {
        match channel {
            None => {
                self.send_target_bot_command_unreliable(
                    self.options().bot.clone(),
                    command.borrow(),
                )
                .await
            }
            Some(channel) => {
                self.send_target_bot_command_unreliable(channel, command.borrow())
                    .await
            }
        }
    }

    /// Checks whether the client joined a channel, and joins if not.
    ///
    /// If the client has already joined the channel, only topic and creation
    /// time are returned.
    pub async fn ensure_join(
        &self,
        channel: impl Borrow<Channel>,
    ) -> StdResult<IrcResponse<Join>, OperatorError<Action>> {
        if let Some(info) = self.channel_info(channel.borrow()).await? {
            Ok(IrcResponse { body: Ok(info) })
        } else {
            self.join(channel).await
        }
    }

    /// Joins a channel and returns channel information.
    ///
    /// # Errors
    /// The outer error indicates any operator error, such as timeout or
    /// cancellation. The error for inner response is [`JoinError`].
    /// When the client has already joined a channel, [`OperatorError::Timeout`]
    /// is returned. Use [`Operator::ensure_join`] to avoid this.
    pub async fn join(
        &self,
        channel: impl Borrow<Channel>,
    ) -> StdResult<IrcResponse<Join>, OperatorError<Action>> {
        self.send_irc_command(Join {
            channel: channel.borrow().clone(),
        })
        .await
    }
    pub async fn join_unreliable(
        &self,
        channel: impl Borrow<Channel>,
    ) -> StdResult<(), OperatorError<Action>> {
        self.send_irc_command_unreliable(Join {
            channel: channel.borrow().clone(),
        })
        .await
    }
    pub async fn part(
        &self,
        channel: impl Borrow<Channel>,
    ) -> StdResult<IrcResponse<Part>, OperatorError<Action>> {
        self.send_irc_command(Part {
            channel: channel.borrow().clone(),
        })
        .await
    }
    pub async fn part_unreliable(
        &self,
        channel: impl Borrow<Channel>,
    ) -> StdResult<(), OperatorError<Action>> {
        self.send_irc_command_unreliable(Part {
            channel: channel.borrow().clone(),
        })
        .await
    }
    /// Returns channel information if the client has joined the channel.
    async fn channel_info(
        &self,
        channel: impl Borrow<Channel>,
    ) -> StdResult<Option<ChannelInfo>, OperatorError<Action>> {
        let (tx, rx) = oneshot::channel();
        self.tx
            .send(Action::Channel(channel.borrow().clone(), tx))
            .await?;
        rx.await.map_err(|_| OperatorError::Cancelled)
    }
    /// Joins a multiplayer match channel as a referee.
    ///
    /// Returns an optional [`multiplayer::Match`] instance. If the multiplayer
    /// channel is not found or the client is not a referee, [`None`] is
    /// returned.
    ///
    /// # Errors
    /// The outer error indicates any operator error, such as timeout or
    /// cancellation.
    pub async fn join_match(
        &self,
        id: multiplayer::MatchId,
    ) -> StdResult<Option<multiplayer::Match>, OperatorError<Action>> {
        let response = self.ensure_join(Channel::Multiplayer(id)).await?;
        Ok(response.body().ok().map(|info| multiplayer::Match {
            id,
            internal_id: info.match_internal_id(),
            operator: self.handle.operator(),
        }))
    }
    pub async fn has_joined(
        &self,
        channel: impl Borrow<Channel>,
    ) -> StdResult<bool, OperatorError<Action>> {
        self.channel_info(channel).await.map(|e| e.is_some())
    }
    pub async fn channels(&self) -> StdResult<Vec<Channel>, OperatorError<Action>> {
        let (tx, rx) = oneshot::channel();
        self.tx.send(Action::Channels(tx)).await?;
        rx.await.map_err(|_| OperatorError::Cancelled)
    }
}

#[derive(Debug)]
pub enum Action {
    Raw(irc::Command),
    RawGroup(Vec<irc::Command>),
    Channel(Channel, oneshot::Sender<Option<ChannelInfo>>),
    Channels(oneshot::Sender<Vec<Channel>>),
    User(String),
}

#[derive(Debug, Clone)]
struct ChannelState {
    topic: String,
    match_internal_id: Option<MatchInternalId>,
    time: chrono::DateTime<chrono::Utc>,
}

struct ClientActor {
    options: ClientOptions,

    assembler: bot::MessageAssembler,
    channels: HashMap<Channel, ChannelState>,
    user_cache: UserCache,
    transport: Transport<TcpStream>,

    action_rx: mpsc::Receiver<Action>,
    event_tx: broadcast::Sender<Event>,
    irc_tx: broadcast::Sender<irc::Message>,
}

impl ClientActor {
    async fn dispatch_irc_privmsg(&mut self, from: String, target: String, body: String) {
        let mut sender = User::name_only(from);
        let is_bancho_bot = self.options.bot == sender;
        if is_bancho_bot {
            sender.flags |= UserFlags::BanchoBot;
        }
        let message = Message {
            sender,
            channel: target.parse().ok(),
            content: body,
        };
        if is_bancho_bot {
            match self.assembler.convert(&message) {
                Some(bot_message) => {
                    if let Some(terminated) = self.assembler.terminate(Some(&bot_message)) {
                        self.event_tx.send(terminated.into()).unwrap();
                    }
                    self.event_tx.send(bot_message.clone().into()).unwrap();
                    if bot_message.is_stateful() {
                        if let Some(tracked) = self.assembler.track(&bot_message) {
                            self.event_tx.send(tracked.into()).unwrap();
                        }
                    }

                    if bot_message.is_multiplayer_event() {
                        if let Some(channel) = bot_message.channel() {
                            match channel {
                                Channel::Multiplayer(match_id) => {
                                    let kind = multiplayer::EventKind::from_bot(
                                        bot_message.kind().clone(),
                                    );
                                    let state = self.channels.get(channel).unwrap();
                                    self.event_tx
                                        .send(
                                            multiplayer::Event {
                                                match_id: *match_id,
                                                match_internal_id: state.match_internal_id.unwrap(),
                                                kind,
                                            }
                                            .into(),
                                        )
                                        .unwrap();
                                }
                                _ => {}
                            }
                        }
                    } else if bot_message.is_maintenance() {
                        self.event_tx
                            .send(EventKind::Maintenance(bot_message.maintenance()).into())
                            .unwrap();
                    }
                }
                None => {
                    self.event_tx.send(message.into()).unwrap();
                }
            }
        } else {
            self.event_tx.send(message.into()).unwrap();
        }
    }
    async fn dispatch_irc(
        &mut self,
        timeout: Pin<&mut time::Sleep>,
        message: irc::Message,
    ) -> StdResult<(), Error> {
        let prefix = message.prefix;
        let command = message.command;

        match command {
            irc::Command::QUIT(..) => {
                if let Some(message) = self.assembler.terminate(None) {
                    self.event_tx.send(message.into()).unwrap();
                }
                if let Some(prefix) = &prefix {
                    self.event_tx
                        .send(EventKind::Quit(User::name_only(prefix.name())).into())
                        .unwrap();
                }
            }
            irc::Command::PING(s1, s2) => {
                self.transport
                    .write(irc::Message {
                        prefix: None,
                        command: irc::Command::PONG(s1, s2),
                    })
                    .await?;
            }
            irc::Command::PONG(..) => {
                timeout.reset(Instant::now() + self.options.pinger_timeout);
            }
            irc::Command::PRIVMSG { target, body } => match prefix {
                Some(prefix) => match prefix {
                    irc::Prefix::User { nickname, .. } => {
                        self.dispatch_irc_privmsg(nickname, target, body).await;
                    }
                    _ => {}
                },
                None => {}
            },
            irc::Command::JOIN(channel)
                if prefix
                    .as_ref()
                    .map(|p| p.name() == self.options.username)
                    .unwrap_or_default() =>
            {
                let channel: Channel = channel.parse().unwrap();
                self.channels
                    .entry(channel.clone())
                    .or_insert(ChannelState {
                        topic: String::new(),
                        match_internal_id: None,
                        time: chrono::DateTime::default(),
                    });
                self.event_tx.send(EventKind::Join(channel).into()).unwrap();
            }
            irc::Command::PART(channel)
                if prefix
                    .as_ref()
                    .map(|p| p.name() == self.options.username)
                    .unwrap_or_default() =>
            {
                let channel = channel.parse().unwrap();
                self.channels.remove(&channel);
                self.event_tx.send(EventKind::Part(channel).into()).unwrap();
            }
            irc::Command::Response(irc::Response::RPL_TOPIC, params) => {
                let mut deque = VecDeque::from(params);
                let _client = deque.pop_front().unwrap();
                let channel = deque.pop_front().unwrap().parse().unwrap();
                let topic = deque.pop_front().unwrap_or_default();
                self.channels.entry(channel).and_modify(|s| {
                    s.topic = topic;
                    s.match_internal_id = Matcher::topic_game_id(&s.topic);
                });
            }
            irc::Command::Response(irc::Response::RPL_TOPICWHOTIME, params) => {
                let mut deque = VecDeque::from(params);
                let _client = deque.pop_front().unwrap();
                let channel = deque.pop_front().unwrap().parse().unwrap();
                let _creator = deque.pop_front().unwrap();
                let timestamp: i64 = deque.pop_front().unwrap().parse().unwrap_or_default();
                let naive = chrono::NaiveDateTime::from_timestamp_millis(timestamp * 1000).unwrap();
                self.channels.entry(channel).and_modify(|s| {
                    s.time = chrono::DateTime::from_naive_utc_and_offset(naive, chrono::Utc);
                });
            }
            irc::Command::Response(irc::Response::RPL_WHOISUSER, params) => {
                let username = &params[1];
                let id = Matcher::whois_user_id(&params[2]).unwrap();
                self.user_cache.put(User::new(id, username));
            }
            _ => {}
        }

        Ok(())
    }
    async fn dispatch_action(&mut self, action: Action) -> Result<()> {
        match action {
            Action::Raw(command) => {
                self.transport
                    .write(irc::Message {
                        prefix: None,
                        command,
                    })
                    .await?;
            }
            Action::RawGroup(commands) => {
                for command in commands {
                    self.transport
                        .write(irc::Message {
                            prefix: None,
                            command,
                        })
                        .await?;
                }
            }
            Action::Channel(channel, tx) => {
                tx.send(self.channels.get(&channel).map(|state| ChannelInfo {
                    channel: channel.clone(),
                    topic: state.topic.clone(),
                    match_internal_id: state.match_internal_id,
                    time: state.time,
                    names: Vec::new(),
                }))
                .unwrap();
            }
            Action::Channels(tx) => {
                tx.send(self.channels.keys().cloned().collect()).unwrap();
            }
            Action::User(s) => {
                let _u = self.user_cache.find_name(&s);
            }
        }
        Ok(())
    }
    async fn run(&mut self) -> StdResult<(), Error> {
        let mut pinger = time::interval(self.options.pinger_interval);
        pinger.tick().await;
        let timeout = time::sleep(self.options.pinger_timeout);
        tokio::pin!(timeout);

        let ping_message = irc::Message {
            prefix: None,
            command: irc::Command::PING(self.options.username.clone(), None),
        };

        loop {
            tokio::select! {
                action = self.action_rx.recv() => {
                    if let Some(action) = action {
                        self.dispatch_action(action).await?;
                    }
                },
                _ = pinger.tick() => {
                    self.transport.write(&ping_message).await?;
                },
                _ = &mut timeout => {
                    return Err(Error::PingerTimeout.into());
                },
                message = self.transport.read() => {
                    match message? {
                        Some(message) => {
                            self.irc_tx.send(message.clone()).unwrap();
                            self.dispatch_irc(timeout.as_mut(), message).await?;
                        },
                        None => {
                            self.event_tx.send(EventKind::Closed.into()).unwrap();
                            break;
                        },
                    }
                },
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Client {
    options: ClientOptions,
    actor: JoinHandle<Result<()>>,
    action_tx: mpsc::Sender<Action>,
    event_rx: broadcast::Receiver<Event>,
    irc_rx: broadcast::Receiver<irc::Message>,
}

impl Client {
    const CHANNEL_BUFFER: usize = 2048;
    const CACHE_SIZE: usize = 2048;
    pub async fn new(options: ClientOptions) -> Result<Self> {
        let stream = TcpStream::connect(options.endpoint.clone()).await?;
        let transport = Transport::new(stream);

        let (irc_tx, irc_rx) = broadcast::channel(Self::CHANNEL_BUFFER);
        let (event_tx, _event_rx) = broadcast::channel(Self::CHANNEL_BUFFER);
        let (action_tx, action_rx) = mpsc::channel(Self::CHANNEL_BUFFER);

        let rx = event_tx.subscribe();
        let tx = action_tx.clone();

        let mut actor = ClientActor {
            assembler: bot::MessageAssembler::new(),
            options: options.clone(),
            channels: HashMap::new(),
            user_cache: UserCache::new(Self::CACHE_SIZE),
            transport,
            event_tx,
            action_rx,
            irc_tx,
        };

        let actor = tokio::spawn(async move { actor.run().await });

        Ok(Self {
            options,
            actor,
            action_tx: tx,
            event_rx: rx,
            irc_rx,
        })
    }
    pub async fn auth(&self) -> StdResult<(), Error> {
        let response = self
            .operator()
            .send_irc_command(Auth {
                username: self.options().username().to_string(),
                password: self.options().password().map(|s| s.to_string()),
            })
            .await
            .unwrap();
        response
            .body()
            .copied()
            .map_err(|s| Error::AuthError(s.to_string()))
    }
    pub fn options(&self) -> &ClientOptions {
        &self.options
    }
    pub fn operator<'a>(&'a self) -> Operator<'a> {
        Operator {
            handle: &self,
            tx: self.action_tx.clone(),
            rx: self.event_rx.resubscribe(),
            irc_rx: self.irc_rx.resubscribe(),
        }
    }
    pub async fn shutdown(self) -> Result<()> {
        self.actor.await.unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Channel {
    Multiplayer(MatchId),
    Raw(String),
}

impl Channel {
    pub fn match_id(&self) -> MatchId {
        match self {
            Channel::Multiplayer(id) => *id,
            _ => panic!("not a multiplayer channel"),
        }
    }
    pub fn is_multiplayer(&self) -> bool {
        match self {
            Channel::Multiplayer(_) => true,
            _ => false,
        }
    }
}

impl From<multiplayer::MatchId> for Channel {
    fn from(value: multiplayer::MatchId) -> Self {
        Channel::Multiplayer(value)
    }
}

impl From<&str> for Channel {
    fn from(value: &str) -> Self {
        if value.starts_with('#') {
            value.parse().unwrap()
        } else {
            Channel::Raw(value.to_string())
        }
    }
}

impl From<String> for Channel {
    fn from(value: String) -> Self {
        if value.starts_with('#') {
            value.parse().unwrap()
        } else {
            Channel::Raw(value)
        }
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

impl FromStr for Channel {
    type Err = ConversionError;
    fn from_str(s: &str) -> StdResult<Self, Self::Err> {
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

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Default, Eq, Hash)]
    pub struct UserFlags: u32 {
        // TODO: implement these flags
        // const Moderator     = 1 << 0;
        // const IrcConnected  = 1 << 1;
        const BanchoBot     = 1 << 2;
    }
}

#[derive(Debug, Clone, Default)]
pub struct User {
    prefer_id: bool,
    id: Option<UserId>,
    name: String,
    flags: UserFlags,
}

impl PartialEq for User {
    fn eq(&self, other: &Self) -> bool {
        other
            .id
            .map(|other| self.eq(&other))
            .unwrap_or(self.eq(&other.name()))
    }
}

impl PartialEq<UserId> for User {
    fn eq(&self, other: &UserId) -> bool {
        self.id == Some(*other)
    }
}

impl PartialEq<&str> for User {
    fn eq(&self, other: &&str) -> bool {
        self.irc_normalized_name()
            .eq_ignore_ascii_case(&Self::irc_normalize(other))
    }
}

impl From<&str> for User {
    fn from(name: &str) -> Self {
        Self::name_only(name)
    }
}

impl From<String> for User {
    fn from(name: String) -> Self {
        Self::name_only(name)
    }
}

impl From<UserId> for User {
    fn from(id: UserId) -> Self {
        Self::id_only(id)
    }
}

impl User {
    #[allow(dead_code)]
    const MAX_LENGTH: usize = 32;
    pub fn new(id: UserId, name: impl AsRef<str>) -> Self {
        Self {
            prefer_id: false,
            id: Some(id),
            name: name.as_ref().to_string(),
            flags: UserFlags::default(),
        }
    }
    pub fn id_only(id: UserId) -> Self {
        Self {
            prefer_id: false,
            id: Some(id),
            flags: UserFlags::default(),
            name: String::new(),
        }
    }
    pub fn name_only(name: impl AsRef<str>) -> Self {
        Self {
            prefer_id: false,
            id: None,
            flags: UserFlags::default(),
            name: name.as_ref().to_string(),
        }
    }
}

impl User {
    /// Normalize a name according to IRC specification.
    pub fn irc_normalize(name: impl AsRef<str>) -> String {
        name.as_ref().replace(' ', "_")
    }

    /// Normalize a name to canonical form (favoring underscore over space, and
    /// lowercasing all characters).
    ///
    /// Names like `X_YZ`, `X YZ`, `x_Yz`, `x yz` are all refering to the exact
    /// same user.
    ///
    /// ```
    /// # use closur::bancho::User;
    /// assert_eq!(User::normalize("X_YZ"), "x_yz");
    /// assert_eq!(User::normalize("X YZ"), "x_yz");
    /// assert_eq!(User::normalize("x_Yz"), "x_yz");
    /// assert_eq!(User::normalize("x yz"), "x_yz");
    /// ```
    ///
    /// This is useful for comparing two usernames.
    pub fn normalize(name: impl AsRef<str>) -> String {
        Self::irc_normalize(name).to_lowercase()
    }

    /// Returns user ID if available.
    pub fn id(&self) -> Option<UserId> {
        self.id
    }

    /// Returns username if available.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// This function returns username with a zero-width space inserted between
    /// first two characters.
    ///
    /// This is quite useful because osu! client will highlight a message for
    /// users if it contains their username, the insertion of zero-space can
    /// prevent this kind of disturbance.
    pub fn name_without_highlight(&self) -> String {
        let mut name = self.name.clone();
        if name.len() > 1 {
            name.insert(1, '\u{200b}');
        }
        name
    }

    /// IRC name is almost same as osu! username, but all spaces are replaced
    /// by underscores.
    pub fn irc_normalized_name(&self) -> String {
        Self::irc_normalize(&self.name)
    }

    /// Normalized name is the lowercase form of IRC name, as an representative
    /// of all the names that are referring to the same user.
    pub fn normalized_name(&self) -> String {
        Self::normalize(&self.name)
    }

    /// Convert users to incomplete ones, they have only usernames and no user
    /// IDs (i.e. ID equals 0).
    pub fn to_name_only(&self) -> Self {
        Self {
            prefer_id: self.prefer_id,
            id: None,
            name: self.name.clone(),
            flags: self.flags,
        }
    }

    /// Convert users to incomplete ones, they have only user IDs and no
    /// usernames (i.e. username is empty).
    pub fn to_id_only(&self) -> Self {
        Self {
            prefer_id: self.prefer_id,
            id: self.id,
            name: String::new(),
            flags: self.flags,
        }
    }

    /// In some scenarios, we want to prefer user ID over username, this method
    /// returns a user with ID preferred.
    ///
    /// ```
    /// use closur::bancho::User;
    /// use closur::bancho::bot::{Command, command::MpHost};
    ///
    /// let user = User::new(1234, "test_user");
    /// assert_eq!(
    ///     MpHost(user.clone()).command_string(),
    ///     "!mp host test_user"
    /// );
    /// assert_eq!(
    ///     MpHost(user.to_id_preferred()).command_string(),
    ///     "!mp host #1234"
    /// );
    /// ```
    pub fn to_id_preferred(&self) -> Self {
        Self {
            prefer_id: true,
            id: self.id,
            name: self.name.clone(),
            flags: self.flags,
        }
    }

    /// Reverse operation of [`User::to_id_preferred`].
    pub fn to_name_preferred(&self) -> Self {
        Self {
            prefer_id: false,
            id: self.id,
            name: self.name.clone(),
            flags: self.flags,
        }
    }

    /// Tells whether the user is incomplete and only has username.
    pub fn is_id_only(&self) -> bool {
        self.has_id() && !self.has_name()
    }

    /// Tells whether the user is incomplete and only has user ID.
    pub fn is_name_only(&self) -> bool {
        !self.has_id() && self.has_name()
    }

    /// Tells whether the user has user ID.
    pub fn has_id(&self) -> bool {
        self.id.is_some()
    }

    /// Tells whether the user has username.
    pub fn has_name(&self) -> bool {
        !self
            .name
            .trim_matches(|c| c == '_' || char::is_ascii_whitespace(&c))
            .is_empty()
    }

    /// Tells whether the user is `BanchoBot`.
    pub fn is_bancho_bot(&self) -> bool {
        self.flags.contains(UserFlags::BanchoBot)
    }
}

pub type UserId = u64;
pub type UserScore = u64;
pub type UserRank = u32;
pub type UserPlayCount = u32;
pub type UserLevel = u16;
pub type UserAccuracy = f32;

impl FromStr for User {
    type Err = ConversionError;
    fn from_str(s: &str) -> StdResult<Self, Self::Err> {
        if s.starts_with('#') {
            let id = s[1..]
                .parse()
                // TODO: better error handling
                .map_err(|_| ConversionError::InvalidUser)?;
            Ok(User::id_only(id))
        } else {
            Ok(User::name_only(s))
        }
    }
}

impl fmt::Display for User {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.prefer_id {
            if self.has_id() {
                write!(f, "#{}", self.id.unwrap())
            } else if self.has_name() {
                write!(f, "{}", self.name)
            } else {
                write!(f, "{}", self.name)
            }
        } else {
            if self.has_name() {
                write!(f, "{}", self.name)
            } else if self.has_id() {
                write!(f, "#{}", self.id.unwrap())
            } else {
                write!(f, "{}", self.name)
            }
        }
    }
}

impl AsRef<str> for User {
    fn as_ref(&self) -> &str {
        self.name.as_str()
    }
}

trait Recipient: fmt::Display + Clone {}
impl Recipient for String {}
impl Recipient for &str {}
impl Recipient for User {}
impl Recipient for &User {}
impl Recipient for Channel {}
impl Recipient for &Channel {}

#[derive(Debug, Clone, PartialEq)]
pub enum MessageTarget {
    User(User),
    Channel(Channel),
}

impl fmt::Display for MessageTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MessageTarget::User(user) => write!(f, "{}", user),
            MessageTarget::Channel(channel) => write!(f, "{}", channel),
        }
    }
}

impl MessageTarget {
    pub fn is_user(&self) -> bool {
        match self {
            MessageTarget::User(_) => true,
            _ => false,
        }
    }
    pub fn is_channel(&self) -> bool {
        match self {
            MessageTarget::Channel(_) => true,
            _ => false,
        }
    }
    pub fn user(&self) -> Option<&User> {
        match self {
            MessageTarget::User(user) => Some(user),
            _ => None,
        }
    }
    pub fn channel(&self) -> Option<&Channel> {
        match self {
            MessageTarget::Channel(channel) => Some(channel),
            _ => None,
        }
    }
    pub fn name(&self) -> String {
        match self {
            MessageTarget::User(user) => user.to_string(),
            MessageTarget::Channel(channel) => channel.to_string(),
        }
    }
}

impl From<User> for MessageTarget {
    fn from(user: User) -> Self {
        MessageTarget::User(user)
    }
}

impl From<Channel> for MessageTarget {
    fn from(channel: Channel) -> Self {
        MessageTarget::Channel(channel)
    }
}

impl PartialEq<User> for MessageTarget {
    fn eq(&self, other: &User) -> bool {
        match self {
            MessageTarget::User(user) => user == other,
            _ => false,
        }
    }
}

impl PartialEq<Channel> for MessageTarget {
    fn eq(&self, other: &Channel) -> bool {
        match self {
            MessageTarget::Channel(channel) => channel == other,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
            "offline" => Ok(UserStatus::Offline),
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

impl FromStr for UserStatus {
    type Err = ConversionError;

    fn from_str(s: &str) -> StdResult<Self, Self::Err> {
        Self::try_from(s)
    }
}

pub type MapId = u64;

#[derive(Debug, Clone, Default)]
pub struct Map {
    id: MapId,
    name: String,
}

impl Map {
    pub fn new(id: MapId, name: impl AsRef<str>) -> Self {
        Self {
            id,
            name: name.as_ref().to_string(),
        }
    }
    pub fn id_only(id: MapId) -> Self {
        Self {
            id,
            name: String::new(),
        }
    }
}

impl From<u64> for Map {
    fn from(id: MapId) -> Self {
        Map {
            id,
            name: String::new(),
        }
    }
}

impl Map {
    pub fn id(&self) -> MapId {
        self.id
    }
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
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
    fn display(&self) -> &str {
        match *self {
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
            m if m.is_empty() => "None",
            _ => unreachable!(""),
        }
    }
    fn name(&self) -> &'static str {
        match *self {
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
            m if m.is_empty() => "None",
            _ => unreachable!(""),
        }
    }
    fn short_name(&self) -> &'static str {
        match *self {
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
            m if m.is_empty() => "none",
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Mods {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            return write!(f, "None");
        }
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
        s.split(',')
            .map(|s| s.trim())
            .try_fold(Mods::empty(), |acc, s| {
                Self::all()
                    .iter()
                    .chain([Mods::empty()])
                    .find(|m| {
                        m.name().eq_ignore_ascii_case(s)
                            || m.display().eq_ignore_ascii_case(s)
                            || m.short_name().eq_ignore_ascii_case(s)
                    })
                    .map(|m| m | acc)
                    .ok_or(ConversionError::InvalidModName)
            })
    }
}

impl FromStr for Mods {
    type Err = ConversionError;

    fn from_str(s: &str) -> StdResult<Self, Self::Err> {
        Self::try_from(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn user_display() {
        assert_eq!(User::name_only("y5c4l3").to_string(), "y5c4l3");
        assert_eq!(User::new(1234, "y5c4l3"), "y5c4l3");
        assert_eq!(User::id_only(1234).to_string(), "#1234");
    }
    #[test]
    fn user_partial_eq() {
        assert_eq!(User::id_only(1234), User::id_only(1234));
    }
}
