pub mod command;
mod message;

use super::{Channel, MessageTarget, TrackVerdict, User};

pub use message::{Message, MessageAssembler, MessageKind, MessageLine};

pub struct CommandBuilder;

pub trait Command: Clone + Sized {
    type Body;
    type Error;

    /// Indicates whether the command can be sent to a user.
    fn sendable_to_user(&self, user: &User) -> bool {
        user.is_bancho_bot()
    }
    /// Indicates whether the command can be sent in a channel.
    fn sendable_in_channel(&self, _channel: &Channel) -> bool {
        true
    }
    /// Returns a valid command string.
    fn command_string(&self) -> String;
    /// This is a stateful message tracker, to capture necessary messages for
    /// further response construction.
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: Message,
    ) -> TrackVerdict<Self::Body, Self::Error>;
}

#[derive(Debug)]
pub struct CommandContext {
    issuer: User,
    target: MessageTarget,
    history: Vec<Message>,
}

impl CommandContext {
    pub fn new(issuer: User, target: impl Into<MessageTarget>) -> Self {
        CommandContext {
            issuer,
            target: target.into(),
            history: Vec::new(),
        }
    }
    pub fn issuer(&self) -> &User {
        &self.issuer
    }
    pub fn target(&self) -> &MessageTarget {
        &self.target
    }
    pub fn history(&self) -> &[Message] {
        &self.history
    }
    pub fn push(&mut self, message: impl Into<Message>) {
        self.history.push(message.into());
    }
}

/// This is defined for those commands which can be followed by a tail text with
/// unchanged semantics.
///
/// For example, `!mp size` is tailable because the following command is valid
///
/// ```bancho
/// !mp size 16 | Welcome to my lobby.
/// ```
///
/// and its effect remains the same as `!mp size 16`.
pub trait TailableCommand: Command + Sized {
    /// Returns a valid command string tailed with specific string.
    fn tailed_command_string(&self, tail: &str) -> String {
        let mut s = self.command_string();
        s.push_str(&tail);
        s
    }
    fn into_tailed(self, tail: impl AsRef<str>) -> TailedCommand<Self> {
        TailedCommand::new(self, tail.as_ref().to_string())
    }
}

/// This is a wrapper for commands that implements [`TailableCommand`], which
/// adds a tail string to the command.
///
/// For example, `TailedCommand::new(MpSettings).push(" | Hello")` is equivalent
/// to sending `!mp settings | Hello` command.
#[derive(Debug, Clone)]
pub struct TailedCommand<T: TailableCommand> {
    inner: T,
    tail: Option<String>,
}

impl<T: TailableCommand> TailedCommand<T> {
    pub fn new(command: T, tail: impl AsRef<str>) -> Self {
        Self {
            inner: command,
            tail: Some(tail.as_ref().to_string()),
        }
    }
    pub fn tail(&self) -> Option<&str> {
        self.tail.as_deref()
    }
    pub fn set_tail(&mut self, tail: Option<impl AsRef<str>>) {
        self.tail = tail.map(|t| t.as_ref().to_string());
    }
    pub fn push(&mut self, tail: impl AsRef<str>) -> &mut Self {
        match &mut self.tail {
            None => {
                self.tail = Some(tail.as_ref().to_string());
            }
            Some(s) => {
                s.push_str(tail.as_ref());
            }
        }
        self
    }
}

impl<T: TailableCommand> Command for TailedCommand<T> {
    type Body = T::Body;
    type Error = T::Error;
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        self.inner.sendable_in_channel(channel)
    }
    fn sendable_to_user(&self, user: &User) -> bool {
        self.inner.sendable_to_user(user)
    }
    fn command_string(&self) -> String {
        match &self.tail {
            None => self.inner.command_string(),
            Some(t) => self.inner.tailed_command_string(t),
        }
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        self.inner.tracks_message(context, message)
    }
}

#[derive(Debug, Clone)]
pub struct Response<T: Command> {
    pub(super) command: T,
    pub(super) messages: Vec<Message>,
    pub(super) body: Result<T::Body, T::Error>,
}

impl<T: Command> Response<T> {
    pub fn command(&self) -> &T {
        &self.command
    }

    pub fn body(&self) -> Result<&T::Body, &T::Error> {
        self.body.as_ref()
    }

    pub fn messages(&self) -> &[Message] {
        self.messages.as_slice()
    }
}

#[derive(Debug, Clone)]
pub enum CommandError {
    UserNotApplicable(User),
    ChannelNotApplicable(Channel),
}

impl std::fmt::Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandError::UserNotApplicable(user) => {
                write!(f, "command is not applicable to user: {}", user)
            }
            CommandError::ChannelNotApplicable(channel) => {
                write!(f, "command is not applicable to channel: {}", channel)
            }
        }
    }
}

impl std::error::Error for CommandError {}

#[cfg(test)]
mod tests {}
