use std::{collections::VecDeque, str::FromStr};

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
    WHOIS(String),
    QUIT(Option<String>),
    Response(Response, Vec<String>),
    Raw(String, Vec<String>),
}

impl Command {
    pub fn response(&self) -> Option<&Response> {
        match &self {
            Command::Response(r, ..) => Some(r),
            _ => None,
        }
    }

    pub fn command(&self) -> Option<&str> {
        match &self {
            Command::PING(..) => Some("PING"),
            Command::PONG(..) => Some("PONG"),
            Command::PASS(..) => Some("PASS"),
            Command::NICK(..) => Some("NICK"),
            Command::USER { .. } => Some("USER"),
            Command::PRIVMSG { .. } => Some("PRIVMSG"),
            Command::JOIN(..) => Some("JOIN"),
            Command::PART(..) => Some("PART"),
            Command::NAMES(..) => Some("NAMES"),
            Command::WHOIS(..) => Some("WHOIS"),
            Command::QUIT(..) => Some("QUIT"),
            Command::Response(..) => None,
            Command::Raw(c, _) => Some(c),
        }
    }
}

impl PartialEq<Response> for Command {
    fn eq(&self, other: &Response) -> bool {
        match self {
            Command::Response(c, _) => c == other,
            _ => false,
        }
    }
}

impl PartialEq<u16> for Command {
    fn eq(&self, other: &u16) -> bool {
        match self {
            Command::Response(c, _) => c == other,
            _ => false,
        }
    }
}

impl PartialEq<&str> for Command {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Command::PING(..) => *other == "PING",
            Command::PONG(..) => *other == "PONG",
            Command::PASS(..) => *other == "PASS",
            Command::NICK(..) => *other == "NICK",
            Command::USER { .. } => *other == "USER",
            Command::PRIVMSG { .. } => *other == "PRIVMSG",
            Command::JOIN(..) => *other == "JOIN",
            Command::PART(..) => *other == "PART",
            Command::NAMES(..) => *other == "NAMES",
            Command::WHOIS(..) => *other == "WHOIS",
            Command::QUIT(..) => *other == "QUIT",
            Command::Raw(c, _) => c == other,
            _ => false,
        }
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
            Command::WHOIS(ref u) => Command::join("WHOIS", &[u], false),
            Command::QUIT(None) => Command::join("QUIT", &[], false),
            Command::QUIT(Some(ref m)) => Command::join("QUIT", &[m], true),
            Command::Response(response, params) => {
                let mut string = response.as_u16().to_string();
                for (i, param) in params.iter().enumerate() {
                    if params.len() - 1 == i && param.contains(' ') {
                        string.push_str(&format!(" :{}", param));
                    } else {
                        string.push_str(&format!(" {}", param));
                    }
                }
                string
            }
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
    pub(super) fn make(command: String, params: Vec<String>) -> Self {
        let mut deque = VecDeque::from(params);
        match command.as_str() {
            "PING" => Command::PING(deque.pop_front().unwrap_or_default(), deque.pop_front()),
            "PONG" => Command::PONG(deque.pop_front().unwrap_or_default(), deque.pop_front()),
            "PASS" => Command::PASS(deque.pop_front().unwrap_or_default()),
            "NICK" => Command::NICK(deque.pop_front().unwrap_or_default()),
            "USER" => Command::USER {
                username: deque.pop_front().unwrap_or_default(),
                mode: deque.pop_front().unwrap_or_default(),
                realname: deque.pop_front().unwrap_or_default(),
            },
            "PRIVMSG" => Command::PRIVMSG {
                target: deque.pop_front().unwrap_or_default(),
                body: deque.pop_front().unwrap_or_default(),
            },
            "JOIN" => Command::JOIN(deque.pop_front().unwrap_or_default()),
            "PART" => Command::PART(deque.pop_front().unwrap_or_default()),
            "NAMES" => Command::NAMES(deque.pop_front().unwrap_or_default()),
            "QUIT" => Command::QUIT(deque.pop_front()),
            c => {
                let params = Vec::from(deque);
                match c.parse::<Response>() {
                    Ok(code) => Command::Response(code, params),
                    Err(_) => Command::Raw(command, params),
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Response(u16);

impl Response {
    pub const RPL_WELCOME: Response = Response(001);

    pub const RPL_WHOISUSER: Response = Response(311);
    pub const RPL_WHOISSERVER: Response = Response(312);
    pub const RPL_WHOISOPERATOR: Response = Response(313);
    pub const RPL_WHOISIDLE: Response = Response(317);
    pub const RPL_ENDOFWHOIS: Response = Response(318);
    pub const RPL_WHOISCHANNELS: Response = Response(319);

    pub const RPL_NOTOPIC: Response = Response(331);
    pub const RPL_TOPIC: Response = Response(332);
    pub const RPL_TOPICWHOTIME: Response = Response(333);
    pub const RPL_NAMREPLY: Response = Response(353);
    pub const RPL_ENDOFNAMES: Response = Response(366);

    pub const RPL_MOTDSTART: Response = Response(375);
    pub const RPL_MOTD: Response = Response(372);
    pub const RPL_ENDOFMOTD: Response = Response(376);

    pub const ERR_NOSUCHNICK: Response = Response(401);
    pub const ERR_NOSUCHSERVER: Response = Response(402);
    pub const ERR_NOSUCHCHANNEL: Response = Response(403);
    pub const ERR_CANNOTSENDTOCHAN: Response = Response(404);
    pub const ERR_TOOMANYCHANNELS: Response = Response(405);
    pub const ERR_PASSWDMISMATCH: Response = Response(464);

    pub fn as_u16(&self) -> u16 {
        self.0
    }
}

impl FromStr for Response {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Response(s.parse().map_err(|_| ())?))
    }
}

impl From<u16> for Response {
    fn from(code: u16) -> Self {
        Response(code)
    }
}

impl PartialEq<u16> for Response {
    fn eq(&self, other: &u16) -> bool {
        self.0 == *other
    }
}
