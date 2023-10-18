mod command;
pub use command::{Command, Response};

use std::borrow::Borrow;
use std::fmt;
use std::io::Cursor;
use std::str::Utf8Error;

use tokio::io::{
    split, AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, BufWriter, ReadHalf, WriteHalf,
};

use bytes::{Buf, BytesMut};

use crate::{error::StdError, StdResult};
type Result<T> = StdResult<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Prefix {
    Server(String),
    User {
        nickname: String,
        username: Option<String>,
        hostname: Option<String>,
    },
}

impl Prefix {
    pub fn is_server(&self) -> bool {
        match self {
            Prefix::Server(_) => true,
            _ => false,
        }
    }

    pub fn is_user(&self) -> bool {
        match self {
            Prefix::User { .. } => true,
            _ => false,
        }
    }

    /// Returns the nick of a user or the name of a server.
    pub fn name(&self) -> &str {
        match self {
            Prefix::Server(name) => name,
            Prefix::User { nickname, .. } => nickname,
        }
    }

    /// Returns the username of a user.
    pub fn username(&self) -> Option<&str> {
        match self {
            Prefix::Server(_name) => None,
            Prefix::User { username, .. } => username.as_ref().map(|s| s.as_str()),
        }
    }

    /// Returns the hostname.
    pub fn host(&self) -> Option<&str> {
        match self {
            Prefix::Server(name) => Some(name),
            Prefix::User { hostname, .. } => hostname.as_ref().map(|s| s.as_str()),
        }
    }

    fn parse(s: &str) -> Option<Self> {
        let splitted = s.split('@').collect::<Vec<&str>>();
        if splitted.len() == 1 {
            Some(Self::Server(splitted[0].to_string()))
        } else {
            splitted.split_last().and_then(|(hostname, rest)| {
                let rest = rest.concat();
                let splitted = rest.split('!').collect::<Vec<&str>>();
                if splitted.len() == 1 {
                    Some(Self::User {
                        nickname: splitted[0].to_string(),
                        username: None,
                        hostname: Some(hostname.to_string()),
                    })
                } else {
                    splitted.split_last().map(|(username, rest)| Self::User {
                        nickname: rest.concat(),
                        username: Some(username.to_string()),
                        hostname: Some(hostname.to_string()),
                    })
                }
            })
        }
    }
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Prefix::Server(name) => f.write_str(name),
            Prefix::User {
                nickname,
                username,
                hostname,
            } => {
                f.write_str(nickname)?;
                if let Some(username) = username {
                    write!(f, "!{}", username)?;
                }
                if let Some(hostname) = hostname {
                    write!(f, "@{}", hostname)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Message {
    pub command: Command,
    pub prefix: Option<Prefix>,
}

impl Message {
    /// This is a hard-coded message length limit. It's been verified that osu!
    /// IRC sometimes sends a message that is longer than 512 bytes, which is
    /// actually not allowed by the protocol
    pub const MAX_LENGTH: usize = 1024;

    pub fn new(command: Command, prefix: Option<Prefix>) -> Self {
        Self { command, prefix }
    }
    pub fn to_string(&self) -> String {
        match &self.prefix {
            Some(prefix) => format!(":{} {}\r\n", prefix.to_string(), self.command.to_string()),
            None => format!("{}\r\n", self.command.to_string()),
        }
    }
    pub fn prefix(&self) -> Option<&Prefix> {
        self.prefix.as_ref()
    }
    pub fn command(&self) -> &Command {
        &self.command
    }

    /// Checks if the buffer contains a complete message. If so, returns the
    /// slice of the buffer that the first message lies in (including the LF).
    /// Otherwise, returns an error.
    ///
    /// # Errors
    ///
    /// * Returns `ParserError::Incomplete` if the buffer does not contain LF
    ///   and still does not exceed the max length.
    /// * Returns `ParserError::MessageTooLong` if the buffer exceeds the max
    ///   length and still does not contain LF.
    ///
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use closur::bancho::irc::Message;
    ///
    /// let buffer = b":irc.example.net 001 alice :Welcome to the Internet Relay Network\r\n";
    /// let message = Message::frame(buffer).unwrap();
    /// assert_eq!(message, b":irc.example.net 001 alice :Welcome to the Internet Relay Network\r\n");
    /// ```
    ///
    /// ```ignore
    /// use closur::bancho::irc::Message;
    ///
    /// let buffer = b":irc.example.net 001 alice :Welcome to the Internet Relay Network\r\n:irc.example.net 002 alice :Your host is irc.example.net, running version InspIRCd-3.8\r\n";
    /// let message = Message::frame(buffer).unwrap();
    /// assert_eq!(message, b":irc.example.net 001 alice :Welcome to the Internet Relay Network\r\n");
    /// ```
    ///
    /// ```ignore
    /// use closur::bancho::irc::Message;
    ///
    /// let buffer = b":irc.example.ne";
    /// let message = Message::frame(buffer).unwrap();
    /// assert_eq!(message, b":irc.example.ne");
    /// ```
    ///
    /// The following example shows a byte buffer containing invalid UTF-8 byte
    /// sequences.
    ///
    fn frame<'a>(buffer: &'a [u8]) -> StdResult<&'a [u8], FramingError> {
        buffer
            .iter()
            .enumerate()
            .take(Self::MAX_LENGTH)
            .find(|(_i, &b)| b == b'\n')
            .map(|(i, _)| buffer.split_at(i + 1).0)
            .ok_or_else(|| FramingError {
                buffer: buffer.to_vec(),
                kind: if buffer.len() >= Self::MAX_LENGTH {
                    FramingErrorKind::MessageTooLong
                } else {
                    FramingErrorKind::Incomplete
                },
            })
    }

    pub fn parse(buffer: &[u8]) -> StdResult<Self, Error> {
        let mut parser = Parser::new(buffer);
        let prefix = parser.read_prefix()?;
        let command = parser.read_command()?;
        let mut params = Vec::new();
        while let Some(param) = parser.read_parameter()? {
            params.push(param);
        }
        Ok(Self {
            prefix,
            command: Command::make(command, params),
        })
    }
}

impl From<Command> for Message {
    fn from(command: Command) -> Self {
        Self {
            command,
            prefix: None,
        }
    }
}

#[derive(Debug)]
pub struct Transport<T> {
    buffer: BytesMut,
    reader: ReadHalf<T>,
    writer: BufWriter<WriteHalf<T>>,
}

impl<T> Transport<T>
where
    T: AsyncRead + AsyncWrite,
{
    const BUFFER_SIZE: usize = 4096;
    pub fn new(inner: T) -> Self {
        let (reader, writer) = split(inner);
        Self {
            reader,
            writer: BufWriter::new(writer),
            buffer: BytesMut::with_capacity(Self::BUFFER_SIZE),
        }
    }

    pub async fn read(&mut self) -> Result<Option<Message>> {
        loop {
            if let Some(message) = self.extract_message()? {
                return Ok(Some(message));
            }
            if 0 == self.reader.read_buf(&mut self.buffer).await? {
                return Ok(None);
            }
        }
    }

    fn extract_message(&mut self) -> Result<Option<Message>> {
        let frame = Message::frame(&self.buffer[..]);

        match frame {
            Ok(frame) => {
                let message = Message::parse(frame)?;
                self.buffer.advance(frame.len());
                Ok(Some(message))
            }
            Err(e) if e.kind() == FramingErrorKind::Incomplete => {
                // The buffer still does not contain a complete message.
                Ok(None)
            }
            Err(e) => Err(e.into()),
        }
    }

    pub async fn write(&mut self, message: impl Borrow<Message>) -> Result<usize> {
        let s = message.borrow().to_string();
        let n = self.writer.write(s.as_bytes()).await?;
        self.writer.flush().await?;
        Ok(n)
    }
}

// TODO: implement message throttling
// pub struct Throttle {
//     burst: usize,
//     window: Duration,
// }

#[derive(Debug, Clone)]
pub struct EncodingError {
    buffer: Vec<u8>,
    token: Vec<u8>,
    inner: Utf8Error,
}

/// Encoding errors when converting a token in IRC message bytes to a string.
impl EncodingError {
    pub fn buffer(&self) -> &[u8] {
        &self.buffer
    }
    pub fn token(&self) -> &[u8] {
        &self.token
    }
    pub fn inner(&self) -> &Utf8Error {
        &self.inner
    }
}

impl fmt::Display for EncodingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "failed to decode token {:?} in buffer {:?}: {}",
            self.token, self.buffer, self.inner
        )
    }
}

impl std::error::Error for EncodingError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        return Some(&self.inner);
    }
}

#[derive(Debug, Clone)]
pub struct FramingError {
    /// The accumulated buffer when error
    buffer: Vec<u8>,
    /// The specific error kind
    kind: FramingErrorKind,
}

impl FramingError {
    /// Returns the complete unconsumed buffer when the error happens.
    pub fn buffer(&self) -> &[u8] {
        &self.buffer
    }
    /// Returns the specific error kind.
    pub fn kind(&self) -> FramingErrorKind {
        self.kind
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FramingErrorKind {
    /// When the buffer still does not hold a complete message and does not
    /// exceed the max length, this error kind is returned.
    Incomplete,
    /// When the accumulated buffer exceeds the max length and does not exist
    /// CRLF in it, this error kind is returned.
    MessageTooLong,
}

impl fmt::Display for FramingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            FramingErrorKind::Incomplete => {
                write!(f, "incomplete message in buffer {:?}", self.buffer)
            }
            FramingErrorKind::MessageTooLong => {
                write!(f, "message too long in buffer {:?}", self.buffer)
            }
        }
    }
}

impl std::error::Error for FramingError {}

#[derive(Debug)]
pub enum Error {
    Framing(FramingError),
    Encoding(EncodingError),
    Parser(ParserError),
    /// IO errors.
    Io(std::io::Error),
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Error::Framing(e) => Some(e),
            Error::Encoding(e) => Some(e),
            Error::Parser(e) => Some(e),
            Error::Io(e) => Some(e),
        }
    }
}

impl From<FramingError> for Error {
    fn from(e: FramingError) -> Self {
        Error::Framing(e)
    }
}

impl From<EncodingError> for Error {
    fn from(e: EncodingError) -> Self {
        Error::Encoding(e)
    }
}

impl From<ParserError> for Error {
    fn from(e: ParserError) -> Self {
        Error::Parser(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Framing(e) => {
                write!(f, "framing error: {}", e)
            }
            Error::Encoding(e) => {
                write!(f, "encoding error: {}", e)
            }
            Error::Parser(e) => {
                write!(f, "parser error: {}", e)
            }
            Error::Io(error) => write!(f, "io error: {}", error),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParserState {
    Prefix,
    Command,
    Params,
    End,
}

#[derive(Debug)]
pub struct ParserError {
    buffer: Vec<u8>,
    state: ParserState,
    offset: usize,
    token: bool,
}

impl ParserError {
    pub fn buffer(&self) -> &[u8] {
        &self.buffer
    }
    pub fn offset(&self) -> usize {
        self.offset
    }
    pub fn state(&self) -> ParserState {
        self.state
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.token {
            write!(
                f,
                "cannot read next token starting at offset {} when parsing message {:?} in state {:?}",
                self.offset, self.buffer, self.state
            )
        } else {
            write!(
                f,
                "unexpected byte at offset {} when parsing message {:?} in state {:?}",
                self.offset, self.buffer, self.state
            )
        }
    }
}

impl StdError for ParserError {}

#[derive(Debug)]
struct Parser<'a> {
    cursor: Cursor<&'a [u8]>,
}

impl<'a> Parser<'a> {
    fn new(buffer: &'a [u8]) -> Self {
        Self {
            cursor: Cursor::new(buffer),
        }
    }
    fn make_parser_error(&self, state: &ParserState) -> Error {
        ParserError {
            buffer: self.cursor.get_ref().to_vec(),
            state: state.clone(),
            offset: self.cursor.position() as usize,
            token: true,
        }
        .into()
    }
    fn make_encoding_error(&self, token: &[u8], inner: Utf8Error) -> Error {
        EncodingError {
            buffer: self.cursor.get_ref().to_vec(),
            token: token.to_vec(),
            inner,
        }
        .into()
    }
    fn skip_spaces(&mut self) {
        let offset = self
            .cursor
            .chunk()
            .iter()
            .take_while(|&&b| b == b' ')
            .count();
        self.cursor.advance(offset);
    }
    fn read_token(&mut self, boundary: &[u8]) -> Result<&str> {
        let chunk = self.cursor.chunk();
        let offset = chunk
            .iter()
            .position(|b| boundary.contains(b))
            .unwrap_or(chunk.len());

        let token = chunk.split_at(offset).0;
        // let token = self.cursor.chunk();
        let token = std::str::from_utf8(token).map_err(|e| self.make_encoding_error(token, e))?;
        Ok(token)
    }
    fn read_prefix(&mut self) -> Result<Option<Prefix>> {
        let state = ParserState::Prefix;
        if self.cursor.has_remaining() {
            let peek = self.cursor.chunk()[0];
            if peek == b':' {
                self.cursor.advance(1);
                let token = self.read_token(b" ")?;
                let length = token.len();
                let prefix = Prefix::parse(token);
                self.cursor.advance(length);
                Ok(prefix)
            } else {
                Ok(None)
            }
        } else {
            Err(self.make_parser_error(&state))
        }
    }
    fn read_command(&mut self) -> Result<String> {
        let state = ParserState::Command;
        self.skip_spaces();
        if self.cursor.has_remaining() {
            let _peek = self.cursor.chunk()[0];
            let token = self.read_token(b" \r\n")?.to_string();
            self.cursor.advance(token.len());
            Ok(token)
        } else {
            Err(self.make_parser_error(&state))
        }
    }
    fn read_parameter(&mut self) -> Result<Option<String>> {
        self.skip_spaces();
        if self.cursor.has_remaining() {
            let peek = self.cursor.chunk()[0];
            match peek {
                b'\r' | b'\n' => Ok(None),
                b':' => {
                    self.cursor.advance(1);
                    let token = self.read_token(b"\r\n")?.to_string();
                    self.cursor.advance(token.len());
                    Ok(Some(token))
                }
                _ => {
                    let token = self.read_token(b" \r\n")?.to_string();
                    self.cursor.advance(token.len());
                    Ok(Some(token))
                }
            }
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug)]
pub enum CommandError {
    ParameterCountMismatch,
}

impl StdError for CommandError {}
impl fmt::Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CommandError::ParameterCountMismatch => write!(f, "Parameter count mismatch."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_server_prefix() {
        let message = Message::parse(
            b":irc.example.net 001 alice :Welcome to the Internet Relay Network\r\n",
        );
        assert!(message.is_ok());
        let message = message.unwrap();
        assert_eq!(
            message.prefix.unwrap(),
            Prefix::Server("irc.example.net".to_string())
        );
        assert_eq!(
            message.command,
            Command::Response(
                Response::from(1),
                ["alice", "Welcome to the Internet Relay Network"]
                    .iter()
                    .map(|p| p.to_string())
                    .collect(),
            )
        );
    }

    #[test]
    fn test_parse_command() {
        let message = Message::parse(b"001 alice :Welcome to the Internet Relay Network\r\n");
        assert!(message.is_ok());
        let message = message.unwrap();
        assert_eq!(message.prefix, None);
        assert_eq!(
            message.command,
            Command::Response(
                Response::from(1),
                ["alice", "Welcome to the Internet Relay Network"]
                    .iter()
                    .map(|p| p.to_string())
                    .collect(),
            )
        );
    }

    #[test]
    fn test_parse_privmsg() {
        let message = Message::parse(
            b":bob!b@irc.example.net PRIVMSG alice :Welcome to the Internet Relay Network\r\n",
        );
        assert!(message.is_ok());
        let message = message.unwrap();
        assert_eq!(
            message.prefix.unwrap(),
            Prefix::User {
                nickname: "bob".to_string(),
                username: Some("b".to_string()),
                hostname: Some("irc.example.net".to_string())
            }
        );
        assert_eq!(
            message.command,
            Command::PRIVMSG {
                target: "alice".to_string(),
                body: "Welcome to the Internet Relay Network".to_string(),
            }
        );
    }
}
