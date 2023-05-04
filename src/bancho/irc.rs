use std::io::Cursor;
use std::str::Utf8Error;

use tokio::io::{
    split, AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, BufWriter, ReadHalf, WriteHalf,
};

use bytes::{Buf, BytesMut};

use crate::{error::StdError, StdResult};

type Result<T> = StdResult<T, Error>;

#[derive(Debug)]
pub enum Error {
    TransportClosed,
    Parser(ParserError),
    Io(tokio::io::Error),
}

impl StdError for Error {}

impl From<tokio::io::Error> for Error {
    fn from(e: tokio::io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<ParserError> for Error {
    fn from(e: ParserError) -> Self {
        Error::Parser(e)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::TransportClosed => write!(f, "Transport closed."),
            Error::Parser(e) => write!(f, "Parser error: {}", e),
            Error::Io(e) => write!(f, "IO error: {}", e),
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    Incomplete(usize),
    MessageTooLong(usize),
    Corrupted,
    InvalidCommand(CommandError),
    EncodingError(Utf8Error),
}

impl StdError for ParserError {}
impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParserError::Incomplete(length) => {
                write!(f, "Message ({} bytes) is incomplete.", length)
            }
            ParserError::MessageTooLong(length) => {
                write!(f, "Message ({} bytes) exceeded length limit.", length)
            }
            ParserError::Corrupted => write!(f, "Message is corrupted."),
            ParserError::InvalidCommand(e) => write!(f, "Invalid command: {:?}", e),
            ParserError::EncodingError(e) => write!(f, "Encoding error: {}", e),
        }
    }
}

impl From<Utf8Error> for ParserError {
    fn from(e: Utf8Error) -> Self {
        ParserError::EncodingError(e)
    }
}

impl From<CommandError> for ParserError {
    fn from(e: CommandError) -> Self {
        ParserError::InvalidCommand(e)
    }
}

#[derive(Debug)]
pub enum CommandError {
    ParameterCountMismatch,
}

impl StdError for CommandError {}
impl std::fmt::Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CommandError::ParameterCountMismatch => write!(f, "Parameter count mismatch."),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    PING(String, Option<String>),
    PONG(String, Option<String>),
    PASS(String),
    NICK(String),
    USER {
        username: String,
        mode: String,
        realname: String,
    },
    PRIVMSG {
        target: String,
        body: String,
    },
    JOIN(String),
    PART(String),
    NAMES(String),
    QUIT(Option<String>),
    Raw(String, Vec<String>),
}

pub trait ToMessageTarget {
    fn to_message_target(&self) -> String;
}

impl ToMessageTarget for &str {
    fn to_message_target(&self) -> String {
        self.to_string()
    }
}

impl ToMessageTarget for String {
    fn to_message_target(&self) -> String {
        self.clone()
    }
}

impl Command {
    fn join(command: &str, params: &[&str], trailing: bool) -> String {
        let mut string = command.to_string();
        for (i, param) in params.iter().enumerate() {
            if params.len() - 1 == i && (param.contains(' ') || trailing) {
                string.push_str(&format!(" :{}", param));
            } else {
                string.push_str(&format!(" {}", param));
            }
        }
        string
    }
    pub fn to_string(&self) -> String {
        match self {
            Command::PING(ref s1, None) => Command::join("PING", &[s1], false),
            Command::PING(ref s1, Some(ref s2)) => Command::join("PING", &[s1, s2], false),
            Command::PONG(ref s1, None) => Command::join("PONG", &[s1], false),
            Command::PONG(ref s1, Some(ref s2)) => Command::join("PONG", &[s1, s2], false),
            Command::PASS(ref p) => Command::join("PASS", &[p], false),
            Command::NICK(ref n) => Command::join("NICK", &[n], false),
            Command::USER {
                username: ref u,
                mode: ref m,
                realname: ref r,
            } => Command::join("USER", &[u, m, "*", r], true),
            Command::PRIVMSG {
                target: ref t,
                body: ref m,
            } => Command::join("PRIVMSG", &[t, m], true),
            Command::JOIN(ref c) => Command::join("JOIN", &[c], false),
            Command::PART(ref c) => Command::join("PART", &[c], false),
            Command::NAMES(ref c) => Command::join("NAMES", &[c], false),
            Command::QUIT(None) => Command::join("QUIT", &[], false),
            Command::QUIT(Some(ref m)) => Command::join("QUIT", &[m], true),
            Command::Raw(command, params) => {
                let mut string = command.to_string();
                for (i, param) in params.iter().enumerate() {
                    if params.len() - 1 == i && param.contains(' ') {
                        string.push_str(&format!(" :{}", param));
                    } else {
                        string.push_str(&format!(" {}", param));
                    }
                }
                string
            }
        }
    }
    pub fn make(command: String, params: Vec<String>) -> StdResult<Self, CommandError> {
        let mapper = |x: &String| x.to_owned();
        match command.as_str() {
            "PING" => Ok(Command::PING(
                params.get(0).map(mapper).unwrap_or("".to_owned()),
                params.get(1).map(mapper),
            )),
            "PONG" => Ok(Command::PONG(
                params.get(0).map(mapper).unwrap_or("".to_owned()),
                params.get(1).map(mapper),
            )),
            "PASS" => Ok(Command::PASS(
                params
                    .get(0)
                    .map(mapper)
                    .ok_or(CommandError::ParameterCountMismatch)?,
            )),
            "NICK" => Ok(Command::NICK(
                params
                    .get(0)
                    .map(mapper)
                    .ok_or(CommandError::ParameterCountMismatch)?,
            )),
            "USER" => Ok(Command::USER {
                username: params
                    .get(0)
                    .map(mapper)
                    .ok_or(CommandError::ParameterCountMismatch)?,
                mode: params
                    .get(1)
                    .map(mapper)
                    .ok_or(CommandError::ParameterCountMismatch)?,
                realname: params
                    .get(3)
                    .map(mapper)
                    .ok_or(CommandError::ParameterCountMismatch)?,
            }),
            "PRIVMSG" => Ok(Command::PRIVMSG {
                target: params.get(0).map(mapper).unwrap_or("".to_owned()),
                body: params.get(1).map(mapper).unwrap_or("".to_owned()),
            }),
            "JOIN" => Ok(Command::JOIN(
                params
                    .get(0)
                    .map(mapper)
                    .ok_or(CommandError::ParameterCountMismatch)?,
            )),
            "PART" => Ok(Command::PART(
                params
                    .get(0)
                    .map(mapper)
                    .ok_or(CommandError::ParameterCountMismatch)?,
            )),
            "NAMES" => Ok(Command::NAMES(
                params
                    .get(0)
                    .map(mapper)
                    .ok_or(CommandError::ParameterCountMismatch)?,
            )),
            "QUIT" => Ok(Command::QUIT(params.get(0).map(mapper))),
            _ => Ok(Command::Raw(command, params)),
        }
    }
}

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
    pub fn to_string(&self) -> String {
        match self {
            Prefix::Server(name) => name.clone(),
            Prefix::User {
                nickname,
                username,
                hostname,
            } => {
                let mut string = nickname.to_string();
                if let Some(username) = username {
                    string.push_str(&format!("!{}", username))
                }
                if let Some(hostname) = hostname {
                    string.push_str(&format!("@{}", hostname))
                }
                string
            }
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

pub trait ToMessage {
    fn to_message(&self) -> Message;
}

#[derive(Debug, Clone)]
pub struct Message {
    pub command: Command,
    pub prefix: Option<Prefix>,
}

impl Message {
    // sometimes receives a message that is longer than 512 bytes, which is not allowed by the protocol
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

    pub fn check(buffer: &[u8]) -> StdResult<&[u8], ParserError> {
        // Check CR and LF in the buffer slice of at most MAX_LENGTH bytes.
        for i in 0..std::cmp::min(buffer.len(), Self::MAX_LENGTH) {
            if buffer[i] == b'\n' {
                return Ok(buffer.split_at(i + 1).0);
            }
        }

        if buffer.len() < Self::MAX_LENGTH {
            // Buffer length is still less than the max length, so we can assume that the message is not complete.
            return Err(ParserError::Incomplete(buffer.len()));
        } else {
            // In this case, neither CR nor LF exists in &buffer[0..MAX_LENGTH], so the message is too long.
            return Err(ParserError::MessageTooLong(buffer.len()));
        }
    }

    pub fn parse(line: &[u8]) -> StdResult<Self, ParserError> {
        #[derive(Debug)]
        enum State {
            Prefix,
            Command,
            Params,
            End,
        }

        // Expecting a prefix at the beginning.
        // If there is no prefix, then the command is the first token.
        let mut state = State::Prefix;

        let mut cursor = Cursor::new(line);

        let mut prefix: Option<Prefix> = None;
        let mut command: String = String::new();
        let mut params: Vec<String> = Vec::new();

        while cursor.has_remaining() {
            let peek = cursor.chunk()[0];
            #[inline(always)]
            fn read_token<'a, P>(
                cursor: &'a Cursor<&'a [u8]>,
                bound: P,
            ) -> StdResult<&'a str, ParserError>
            where
                P: FnMut(&u8) -> bool,
            {
                Ok(std::str::from_utf8(
                    cursor
                        .chunk()
                        .iter()
                        .position(bound)
                        .map(|i| cursor.chunk().split_at(i).0)
                        .ok_or(ParserError::Corrupted)?,
                )?)
            }

            match (&state, peek) {
                // Expect a prefix at the very beginning.
                (State::Prefix, b':') => {
                    cursor.advance(1);
                    let token = read_token(&cursor, |&b| b == b' ')?;
                    prefix = Prefix::parse(token);
                    state = State::Command;
                    cursor.advance(token.len());
                }
                (State::Prefix, _) => {
                    state = State::Command;
                }
                (State::Command, b' ') | (State::Params, b' ') => {
                    cursor.advance(1);
                }
                (State::Command, _) => {
                    let token = read_token(&cursor, |&b| b == b' ' || b == b'\r' || b == b'\n')?;
                    command = token.to_string();
                    cursor.advance(token.len());
                    state = State::Params;
                }
                (State::Params, b':') => {
                    cursor.advance(1);
                    let token = read_token(&cursor, |&b| b == b'\r' || b == b'\n')?;
                    params.push(token.to_string());
                    cursor.advance(token.len());
                    state = State::End;
                    break;
                }
                (State::Params, b'\r') | (State::Params, b'\n') => {
                    state = State::End;
                    break;
                }
                (State::Params, _) => {
                    let token = read_token(&cursor, |&b| b == b' ' || b == b'\r' || b == b'\n')?;
                    params.push(token.to_string());
                    cursor.advance(token.len());
                    state = State::Params;
                }
                (_, _) => {
                    return Err(ParserError::Corrupted);
                }
            }
        }

        match state {
            State::End | State::Params => Ok(Message {
                prefix: prefix,
                command: Command::make(command, params)?,
            }),
            _ => Err(ParserError::Corrupted),
        }
    }
}

impl ToMessage for Message {
    fn to_message(&self) -> Message {
        self.clone()
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

// TODO: implement message throttling
// pub struct Throttle {
//     burst: usize,
//     window: Duration,
// }

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
            if let Some(message) = self.parse()? {
                return Ok(Some(message));
            }
            if 0 == self.reader.read_buf(&mut self.buffer).await? {
                if self.buffer.is_empty() {
                    return Ok(None);
                } else {
                    return Err(Error::TransportClosed);
                }
            }
        }
    }

    fn parse(&mut self) -> StdResult<Option<Message>, ParserError> {
        // Message::check will try to extract one line from the buffer
        match Message::check(&self.buffer[..]) {
            Ok(line) => match Message::parse(line) {
                Ok(message) => {
                    self.buffer.advance(line.len());
                    Ok(Some(message))
                }
                Err(_) => {
                    self.buffer.advance(line.len());
                    Ok(None)
                }
            },
            Err(e) => match e {
                ParserError::Incomplete(_) => Ok(None),
                _ => Err(e.into()),
            },
        }
    }

    pub async fn write(&mut self, message: &Message) -> Result<usize> {
        let n = self.writer.write(message.to_string().as_bytes()).await?;
        self.writer.flush().await?;
        Ok(n)
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_parse_server_prefix() {
        let mut message = Message::parse(
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
            Command::Raw(
                "001".to_string(),
                ["alice", "Welcome to the Internet Relay Network"]
                    .iter()
                    .map(|p| p.to_string())
                    .collect(),
            )
        );
    }

    #[test]
    fn test_parse_command() {
        let mut message = Message::parse(b"001 alice :Welcome to the Internet Relay Network\r\n");
        assert!(message.is_ok());
        let message = message.unwrap();
        assert_eq!(message.prefix, None);
        assert_eq!(
            message.command,
            Command::Raw(
                "001".to_string(),
                ["alice", "Welcome to the Internet Relay Network"]
                    .iter()
                    .map(|p| p.to_string())
                    .collect(),
            )
        );
    }

    #[test]
    fn test_parse_privmsg() {
        let mut message = Message::parse(
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
