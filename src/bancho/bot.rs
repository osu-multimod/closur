use std::time::{Duration, Instant};

use regex::{Captures, Regex, RegexSet};

use crate::{error::StdError, StdResult};

use super::{
    multiplayer::{GameMode, ScoreMode, Slot, SlotStatus, Slots, Team, TeamMode, MAX_SLOTS},
    Map, Mods, User, UserStatus,
};

#[derive(Debug, Clone)]
pub enum Command {
    // standard commands
    Where(User),
    Stats(User),
    Roll(u32),

    // create or close multiplayer games
    Make(String),
    MakePrivate(String),
    Close,

    // configure multiplayer game
    Name(String),
    Password(String),
    Size(usize),
    Set {
        team_mode: Option<TeamMode>,
        score_mode: Option<ScoreMode>,
        size: Option<usize>,
    },
    Mods {
        mods: Mods,
        freemod: bool,
    },
    Map(Map, Option<GameMode>),

    // multiplayer slot-related commands
    Host(User),
    ClearHost,
    Lock,
    Unlock,
    Move {
        user: User,
        slot: usize,
    },
    Team {
        user: User,
        team: Team,
    },

    // get multiplayer game settings
    Settings,

    // multiplayer match commands
    Start(Option<Duration>),
    Timer(Option<Duration>),
    AbortTimer,
    Abort,

    // multiplayer player management
    Invite(User),
    Kick(User),
    Ban(User),
    AddRef(User),
    RemoveRef(User),
    ListRefs,

    // attach tailing message to command
    Tailing(Box<Command>, String),
}

impl Command {
    pub fn requires_channel(&self) -> bool {
        match self {
            Command::Stats(_) => false,
            Command::Where(_) => false,
            Command::Make(_) => false,
            _ => true,
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            // standard commands
            Command::Where(user) => format!("!where {}", user.to_parameter()),
            Command::Stats(user) => format!("!stats {}", user.to_parameter()),
            Command::Roll(range) => format!("!roll {}", range),

            // create or close multiplayer games
            Command::Make(name) => format!("!mp make {}", name),
            Command::MakePrivate(name) => format!("!mp makeprivate {}", name),
            Command::Close => "!mp close".to_string(),

            // configure multiplayer game
            Command::Name(name) => format!("!mp name {}", name),
            Command::Password(password) => format!("!mp password {}", password),
            Command::Size(size) => format!("!mp size {}", std::cmp::min(*size, MAX_SLOTS)),
            Command::Set {
                team_mode,
                score_mode,
                size,
            } => match (team_mode, score_mode, size) {
                (Some(team_mode), Some(score_mode), Some(size)) => format!(
                    "!mp set {} {} {}",
                    *team_mode as usize,
                    *score_mode as usize,
                    std::cmp::min(*size, MAX_SLOTS)
                ),
                (Some(team_mode), Some(score_mode), None) => {
                    format!("!mp set {} {}", *team_mode as usize, *score_mode as usize)
                }
                (Some(team_mode), None, Some(size)) => {
                    format!(
                        "!mp set {} _ {}",
                        *team_mode as usize,
                        std::cmp::min(*size, MAX_SLOTS)
                    )
                }
                (None, Some(score_mode), Some(size)) => {
                    format!(
                        "!mp set _ {} {}",
                        *score_mode as usize,
                        std::cmp::min(*size, MAX_SLOTS)
                    )
                }
                (Some(team_mode), None, None) => format!("!mp set {}", *team_mode as usize),
                (None, Some(score_mode), None) => format!("!mp set _ {}", *score_mode as usize),
                (None, None, Some(size)) => {
                    format!("!mp set _ _ {}", std::cmp::min(*size, MAX_SLOTS))
                }
                (None, None, None) => "!mp set".to_string(),
            },
            Command::Mods { mods, freemod } => format!(
                "!mp mods {}{}",
                mods.bits(),
                if *freemod { " Freemod" } else { "" }
            ),
            Command::Map(map, mode) => match mode {
                Some(mode) => format!("!mp map {} {}", map.id, *mode as usize),
                None => format!("!mp map {}", map.id),
            },

            // multiplayer slot-related commands
            Command::Host(user) => format!("!mp host {}", user.to_parameter()),
            Command::ClearHost => "!mp clearhost".to_string(),
            Command::Lock => "!mp lock".to_string(),
            Command::Unlock => "!mp unlock".to_string(),
            Command::Move { user, slot } => {
                format!("!mp move {} {}", user.to_parameter(), slot + 1)
            }
            Command::Team { user, team } => format!("!mp team {} {}", user.to_parameter(), team),

            // get multiplayer settings
            Command::Settings => "!mp settings".to_string(),

            // multiplayer match commands
            Command::Start(duration) => match duration {
                Some(duration) => format!("!mp start {}", duration.as_secs()),
                None => "!mp start".to_string(),
            },
            Command::Timer(duration) => match duration {
                Some(duration) => format!("!mp timer {}", duration.as_secs()),
                None => "!mp timer".to_string(),
            },
            Command::AbortTimer => "!mp aborttimer".to_string(),
            Command::Abort => "!mp abort".to_string(),

            // multiplayer player management
            Command::Invite(user) => format!("!mp invite {}", user.to_parameter()),
            Command::Kick(user) => format!("!mp kick {}", user.to_parameter()),
            Command::Ban(user) => format!("!mp ban {}", user.to_parameter()),
            Command::AddRef(user) => format!("!mp addref {}", user.to_parameter()),
            Command::RemoveRef(user) => format!("!mp removeref {}", user.to_parameter()),
            Command::ListRefs => "!mp listrefs".to_string(),

            // attach tailing message to command
            Command::Tailing(c, t) => format!("{} {}", c.to_string(), t),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct WhereResponse {
    user: User,
    location: String,
}

#[derive(Debug, Clone, Default)]
pub struct StatsResponse {
    user: User,
    status: UserStatus,
    score: u64,
    rank: u32,
    play_count: u32,
    level: u32,
    accuracy: f32,
}

#[derive(Debug, Clone, Default)]
pub struct MpMakeResponse {
    id: u64,
    title: String,
}

impl MpMakeResponse {
    pub fn id(&self) -> u64 {
        self.id
    }
    pub fn title(&self) -> &str {
        self.title.as_str()
    }
}

#[derive(Debug, Clone, Default)]
pub struct MpSettingsResponse {
    name: String,
    id: u64,
    map: Option<Map>,
    team_mode: TeamMode,
    score_mode: ScoreMode,
    mods: Mods,
    freemod: bool,
    size: usize,
    slots: Slots,
}

impl MpSettingsResponse {
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
    pub fn id(&self) -> u64 {
        self.id
    }
    pub fn map(&self) -> Option<Map> {
        self.map.clone()
    }
    pub fn team_mode(&self) -> TeamMode {
        self.team_mode
    }
    pub fn score_mode(&self) -> ScoreMode {
        self.score_mode
    }
    pub fn mods(&self) -> Mods {
        self.mods
    }
    pub fn freemod(&self) -> bool {
        self.freemod
    }
    pub fn size(&self) -> usize {
        self.size
    }
    pub fn slots(&self) -> &Slots {
        &self.slots
    }
    pub fn valid_slots(&self) -> Vec<Slot> {
        self.slots.iter().filter_map(|s| s.clone()).collect()
    }
}

#[derive(Debug, Clone, Default)]
pub struct MpSetResponse {
    size: Option<usize>,
    team_mode: Option<TeamMode>,
    score_mode: Option<ScoreMode>,
}

#[derive(Debug, Clone, Default)]
pub struct MpMapResponse {
    map: Map,
    mode: Option<GameMode>,
}

#[derive(Debug, Clone, Default)]
pub struct MpTeamResponse {
    user: User,
    team: Team,
}

#[derive(Debug, Clone, Default)]
pub struct MpMoveResponse {
    user: User,
    slot: usize,
}

#[derive(Debug, Clone, Default)]
pub struct MpModsResponse {
    mods: Mods,
    freemod: bool,
}

#[derive(Debug, Clone)]
pub struct Response {
    source: Command,
    inner: ResponseInner,
}

impl Response {
    pub fn inner(&self) -> &ResponseInner {
        &self.inner
    }
    pub fn source(&self) -> Command {
        self.source.clone()
    }
    pub fn make(&self) -> &MpMakeResponse {
        match self.inner {
            ResponseInner::MpMake(ref make) => make,
            _ => {
                unreachable!("")
            }
        }
    }
    pub fn settings(&self) -> &MpSettingsResponse {
        match self.inner {
            ResponseInner::MpSettings(ref settings) => settings,
            _ => {
                unreachable!("")
            }
        }
    }

    pub fn is_ok(&self) -> bool {
        match self.inner {
            ResponseInner::Err(_) => false,
            _ => true,
        }
    }

    pub fn is_err(&self) -> bool {
        !self.is_ok()
    }

    pub fn error(&self) -> Error {
        match self.inner {
            ResponseInner::Err(e) => e,
            _ => unreachable!(""),
        }
    }

    pub fn user(&self) -> User {
        match self.inner {
            ResponseInner::MpHost(ref user) => user.clone(),
            ResponseInner::MpAddRef(ref user) => user.clone(),
            ResponseInner::MpRemoveRef(ref user) => user.clone(),
            _ => unreachable!(""),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ResponseInner {
    Ok,

    // standard commands
    Where(WhereResponse),
    Stats(StatsResponse),
    Roll(u32),

    // multiplayer commands
    MpMake(MpMakeResponse),

    MpName(String),

    MpSize(usize),
    MpSet(MpSetResponse),
    MpMods(MpModsResponse),
    MpMap(MpMapResponse),

    MpHost(User),
    MpMove(MpMoveResponse),
    MpTeam(MpTeamResponse),

    MpSettings(MpSettingsResponse),

    MpAddRef(User),
    MpRemoveRef(User),
    MpListRefs(Vec<User>),

    Err(Error),
}

#[derive(Debug, Copy, Clone)]
pub enum Error {
    UserNotFound,

    WhereOffline,

    MpMakeExceeded,
    MpSizeInvalid,
    MpSetInvalid,
    MpMapInvalid,

    MpMoveFailed,

    MpStartDuplicated,

    MpInviteAlreadyPresent,
    MpRefNotFound,
}

#[derive(Debug)]
struct RegexSetMatcher {
    set: RegexSet,
    regexes: Vec<Regex>,
}

impl RegexSetMatcher {
    fn new<I, S>(patterns: I) -> Self
    where
        S: AsRef<str> + Clone,
        I: IntoIterator<Item = S>,
    {
        let patterns: Vec<S> = patterns.into_iter().collect();
        let set = RegexSet::new(patterns.as_slice()).unwrap();
        Self {
            set,
            regexes: patterns
                .into_iter()
                .map(|p| Regex::new(p.as_ref()).unwrap())
                .collect(),
        }
    }
    fn matches<'a>(&'a self, s: &'a str) -> Option<(usize, regex::Captures)> {
        self.set
            .matches(s)
            .into_iter()
            .next()
            .map(|i| (i, self.regexes[i].captures(s).unwrap()))
    }
}

#[derive(Debug, Clone)]
pub enum Message {
    // standard commands
    WhereResponse {
        user: String,
        location: String,
    },
    WhereOfflineError,

    StatsResponseStatus {
        user: User,
        status: UserStatus,
    },
    StatsResponseScoreRank {
        score: u64,
        rank: u32,
    },
    StatsResponsePlayCountLevel {
        play_count: u32,
        level: u32,
    },
    StatsResponseAccuracy {
        accuracy: f32,
    },

    RollResponse {
        user: String,
        value: u32,
    },

    // create or close multiplayer games
    MpMakeResponse {
        id: u64,
        title: String,
    },
    MpMakeError,
    MpCloseResponse,

    // configure multiplayer game
    MpNameResponse(String),
    MpPasswordResponseChanged,
    MpPasswordResponseRemoved,
    MpSizeResponse(usize),
    MpSizeError,
    MpSetResponse {
        size: Option<usize>,
        team_mode: Option<TeamMode>,
        score_mode: Option<ScoreMode>,
    },
    MpSetError,
    MpModsResponse {
        mods: Mods,
        freemod: bool,
    },
    MpMapResponseMap(Map),
    MpMapResponseGameMode(GameMode),
    MpMapInvalidError,

    // multiplayer slot-related commands
    MpHostResponse(User),
    MpClearHostResponse,
    MpLockResponse,
    MpUnlockResponse,
    MpMoveResponse {
        user: User,
        slot: usize,
    },
    MpMoveError {
        user: User,
        slot: usize,
    },
    MpTeamResponse {
        user: User,
        team: String,
    },

    // get multiplayer game settings
    MpSettingsResponseRoomName {
        name: String,
        id: u64,
    },
    MpSettingsResponseMap {
        name: String,
        id: u64,
    },
    MpSettingsResponseTeamMode {
        team_mode: TeamMode,
        score_mode: ScoreMode,
    },
    MpSettingsResponseActiveMods {
        mods: Mods,
        freemod: bool,
    },
    MpSettingsResponsePlayers(usize),
    MpSettingsResponseSlot {
        slot: usize,
        status: SlotStatus,
        user: User,
        mods: Mods,
        host: bool,
        team: Option<Team>,
    },

    // multiplayer match commands
    MpStartResponse,
    MpStartTimerResponse,
    MpStartDuplicatedError,
    MpAbortStartTimerResponse,
    MpTimerResponse,
    MpAbortTimerResponse,
    MpAbortResponse,

    // multiplayer player management
    MpInviteResponse(User),
    MpInviteAlready,
    MpKickResponse(User),
    MpBanResponse(User),
    MpAddrefResponse(User),
    MpRemoverefResponse(User),
    MpListRefsPrompt,
    MpRefError,

    // general errors
    UserNotFoundError,

    // multiplayer game events
    // slot-related events
    PlayerJoinedEvent {
        user: User,
        slot: usize,
        team: Option<Team>,
    },
    PlayerMovedEvent {
        user: User,
        slot: usize,
    },
    PlayerChangedTeamEvent {
        user: User,
        team: Team,
    },
    PlayerLeftEvent(User),

    // host-related events
    HostChangedEvent(User),
    HostChangingMapEvent,
    MapChangedEvent(Map),

    // match-related events
    PlayersReadyEvent,
    MatchStartedEvent,
    MatchPlayerScoreEvent {
        user: User,
        score: u64,
        alive: bool,
    },
    MatchFinishedEvent,

    // timer related events
    StartTimer(usize),
    StartTimerEnd,
    Timer(usize),
    TimerEnd,

    Raw(String),
}

#[derive(Debug)]
pub struct MessageMatcher<'a> {
    matcher: RegexSetMatcher,
    keys: Vec<&'a str>,
}

static MESSAGE_MAP: &[(&str, &str)] = &[
    // standard commands
    // response to `!where`
    ("where-response", r"^(?P<user>.+) is in ?(?P<location>.*)$"),
    ("where-error", r"^The user is currently not online.$"),
    // responses to `!stats`
    (
        "stats-status",
        r"^Stats for \((?P<user>.+)\)\[https://osu.ppy.sh/u/(?P<id>\d+)\](?: is )?(?P<status>Idle|Afk|Playing|Lobby|Multiplayer|Multiplaying|Modding|Watching)?:$",
    ),
    (
        "stats-score",
        r"^Score: *(?P<score>[\d,]+) \(#(?P<rank>\d+)\)$",
    ),
    (
        "stats-playcount",
        r"^Plays: *(?P<play_count>\d+) \(lv(?P<level>\d+)\)$",
    ),
    ("stats-accuracy", r"^Accuracy: +(?P<accuracy>[\d.]+)%$"),
    (
        "roll-response",
        r"^(?P<user>.+) rolls (?P<value>\d+) point\(s\)$",
    ),
    // create or close multiplayer games
    // responses to `!mp make` `!mp makeprivate` and `!mp close`
    (
        "make-response",
        r"^Created the tournament match https://osu\.ppy\.sh/mp/(?P<id>\d+) (?P<name>.+)$",
    ),
    (
        "make-error",
        r"^You cannot create any more tournament matches. Please close any previous tournament matches you have open.$",
    ),
    ("close-response", r"^Closed the match$"),
    // configure multiplayer game
    // response to `!mp name`
    ("name-response", "^Room name updated to \"(?P<name>.+)\"$"),
    // responses to `!mp password`
    ("password-changed", r"^Changed the match password$"),
    ("password-removed", r"^Removed the match password$"), // when password is empty
    // response to `!mp size`
    ("size-response", r"^Changed match to size (?P<size>\d+)$"),
    ("size-error", r"^Invalid or no size provided$"),
    // response to `!mp set`
    (
        "set-response",
        r"^Changed match settings to (?:(?P<size>\d+) slots)?(?:, *)?(?P<team>HeadToHead|TagCoop|TeamVs|TagTeamVs)(?:, *)?(?P<score>Score|Accuracy|Combo|ScoreV2)?$",
    ),
    ("set-error", r"^Invalid or no settings provided$"),
    // response to `!mp mods`
    (
        "mods-response",
        r"^(Enabled (?P<mods>.+)|Disabled all mods), (?P<freemod>disabled|enabled) FreeMod",
    ),
    // responses to `!mp map`
    (
        "map-response",
        r"^Changed beatmap to https://osu\.ppy\.sh/b/(?P<id>\d+) (?P<name>.+)$",
    ),
    (
        "map-gamemode",
        r"^Changed match mode to (?P<mode>Osu|Taiko|CatchTheBeat|OsuMania)$",
    ),
    ("map-error", r"^Invalid map ID provided$"),
    // multiplayer slot-related commands
    // responses to `!mp host` and `!mp clearhost`
    ("host-response", r"^Changed match host to (?P<user>.+)$"),
    ("clearhost-response", r"^Cleared match host$"),
    // responses to `!mp lock` and `!mp unlock`
    ("lock-response", r"^Locked the match$"),
    ("unlock-response", r"^Unlocked the match$"),
    // response to `!mp move`
    (
        "move-response",
        r"^Moved (?P<user>.+) into slot (?P<slot>\d+)$",
    ),
    (
        "move-error",
        r"^Failed to move player to slot (?P<slot>\d+)$",
    ),
    // response to `!mp team`
    (
        "team-response",
        r"^Moved (?P<user>.+) to team (?P<team>Red|Blue)$",
    ),
    // get multiplayer game settings
    // responses to `!mp settings`
    (
        "settings-name",
        r"^Room name: (?P<name>.+), History: https://osu\.ppy\.sh/mp/(?P<id>\d+)$",
    ),
    (
        "settings-map",
        r"^Beatmap: https://osu\.ppy\.sh/b/(?P<id>\d+) (?P<name>.+)$",
    ),
    (
        "settings-team",
        r"^Team mode: (?P<team>.+), Win condition: (?P<score>.+)$",
    ),
    ("settings-mods", r"^Active mods: (?P<mods>.+)$"),
    ("settings-size", r"^Players: (?P<size>\d+)$"),
    (
        "settings-slot",
        r"^Slot (?P<slot>\d+) +(?P<status>Not Ready|Ready|No Map) +https://osu\.ppy\.sh/u/(?P<id>\d+) (?P<user>.{16,}?) *(?:\[(?P<attrs>(.+))\])?$",
    ),
    // multiplayer match commands
    // responses to `!mp start`
    ("start-response", r"^Started the match$"),
    (
        "start-timer",
        r"^Queued the match to start in (?P<time>\d+) seconds?$",
    ),
    ("start-duplicated", r"^The match has already been started$"),
    // response to `!mp aborttimer`
    ("aborttimer-response", r"^Countdown aborted$"),
    ("aborttimer-start", r"^Aborted the match start timer$"),
    // response to `!mp abort`
    ("abort-response", r"^Aborted the match$"),
    // multiplayer player management
    // response to `!mp invite`
    ("invite-response", r"^Invited (?P<user>.+) to the room$"),
    ("invite-already", r"^User is already in the room$"),
    // responses to `!mp kick` and `!mp ban`
    ("kick-response", r"^Kicked (?P<user>.+) from the match$"),
    ("ban-response", r"^Banned (?P<user>.+) from the match$"),
    // responses to `!mp addref` and `!mp removeref`
    (
        "addref-response",
        r"^Added (?P<referee>.+) to the match referees$",
    ),
    (
        "removeref-response",
        r"^Removed (?P<referee>.+) from the match referees$",
    ),
    ("listrefs-prompt", "^Match referees:$"),
    ("ref-error", r"^User not found: (?P<referee>.+)$"),
    // general errors
    // when parameter of a command is an invalid user, e.g. `!mp host` `!stats`
    ("error-user-not-found", r"^User not found$"),
    // multiplayer game events
    // slot-related events
    (
        "event-player-joined",
        r"^(?P<user>.+) joined in slot (?P<slot>\d+)(?: for team (?P<team>red|blue))?\.$",
    ),
    (
        "event-player-moved",
        r"^(?P<user>.+) moved to slot (?P<slot>\d+)$",
    ),
    (
        "event-player-team",
        r"^(?P<user>.+) changed to (?P<team>Blue|Red)$",
    ),
    ("event-player-left", r"^(?P<user>.+) left the game\.$"),
    // host-related events
    ("event-host-changed", r"^(?P<user>.+) became the host\.$"), // host
    ("event-host-changing-map", r"^Host is changing map\.\.\.$"), // host is changing map
    (
        "event-map-changed",
        r"^Beatmap changed to: (?P<name>.+) \(https://osu\.ppy\.sh/b/(?P<id>\d+)\)$",
    ),
    // match-related events
    ("event-players-ready", r"^All players are ready$"), // all players are ready
    ("event-match-started", r"^The match has started!$"), // match started
    (
        "event-match-player-result",
        r"^(?P<user>.+) finished playing \(Score: (?P<score>\d+), (?P<alive>FAILED|PASSED)\)\.$",
    ),
    ("event-match-finished", r"^The match has finished!$"), // match finished
    // timer related events
    // Match starts in ((?P<s>\d+) seconds?|(?P<m>\d+) minutes?|(?P<s>\d+) minutes and 1 second)
    // ("event-starttimer", r"^Match starts in ((\d+) (second|minute)s?)?( and (\d+) seconds?)?$"), // `!mp start`
    // ("event-starttimer-end", r"^Good luck, have fun!$"),
    // ("event-timer", r"^Countdown ends in ((\d+) (second|minute)s?)?( and (\d+) seconds?)?$"), // `!mp timer`
    // ("event-timer-end", r"^Countdown finished$"),
    (
        "event-starttimer",
        r"^Match starts in (?:(\d+) (?P<type>second|minute)s?)?(?: and (\d+) seconds?)?$",
    ),
    // ("event-starttimer-end", r"^Good luck, have fun!$"),
    (
        "event-timer",
        r"^Countdown ends in (?:(\d+) (?P<type>second|minute)s?)?(?: and (\d+) seconds?)?$",
    ),
    ("event-timer-end", r"^Countdown finished$"),
];

impl MessageMatcher<'_> {
    pub fn new() -> Self {
        Self {
            keys: MESSAGE_MAP.iter().map(|(k, _)| k.clone()).collect(),
            matcher: RegexSetMatcher::new(MESSAGE_MAP.iter().map(|(_, p)| p.clone())),
        }
    }
    fn try_match(key: &str, captures: Captures) -> crate::Result<Message> {
        Ok(match key {
            // standard commands
            // response to `!where`
            "where-response" => Message::WhereResponse {
                user: captures["user"].to_string(),
                location: captures["location"].to_string(),
            },
            "where-error" => Message::WhereOfflineError,

            // responses to `!stats`
            "stats-status" => Message::StatsResponseStatus {
                user: User {
                    name: captures["user"].to_string(),
                    id: captures["id"].parse()?,
                },
                status: match captures.name("status") {
                    Some(s) => s.as_str().try_into(),
                    None => Ok(UserStatus::Offline),
                }?,
            },
            "stats-score" => Message::StatsResponseScoreRank {
                score: captures["score"].replace(',', "").parse()?,
                rank: captures["rank"].parse()?,
            },
            "stats-playcount" => Message::StatsResponsePlayCountLevel {
                play_count: captures["play_count"].parse()?,
                level: captures["level"].parse()?,
            },
            "stats-accuracy" => Message::StatsResponseAccuracy {
                accuracy: captures["accuracy"].parse()?,
            },

            "roll-response" => Message::RollResponse {
                user: captures["user"].to_string(),
                value: captures["value"].parse()?,
            },

            // create or close multiplayer games
            // responses to `!mp make` `!mp makeprivate` and `!mp close`
            "make-response" => Message::MpMakeResponse {
                id: captures["id"].parse()?,
                title: captures["name"].to_string(),
            },
            "make-error" => Message::MpMakeError,
            "close-response" => Message::MpCloseResponse,
            // responses to `!mp password`
            "password-changed" => Message::MpPasswordResponseChanged,
            "password-removed" => Message::MpPasswordResponseRemoved,

            // configure multiplayer game
            // response to `!mp name`
            "name-response" => Message::MpNameResponse(captures["name"].to_string()),
            // response to `!mp size`
            "size-response" => Message::MpSizeResponse(captures["size"].parse()?),
            "size-error" => Message::MpSizeError,
            "set-response" => Message::MpSetResponse {
                size: captures
                    .name("size")
                    .map(|s| s.as_str().parse())
                    .transpose()?,
                team_mode: captures
                    .name("team")
                    .map(|x| x.as_str().try_into())
                    .transpose()?,
                score_mode: captures
                    .name("score")
                    .map(|x| x.as_str().try_into())
                    .transpose()?,
            },
            "set-error" => Message::MpSetError,
            "mods-response" => Message::MpModsResponse {
                mods: captures
                    .name("mods")
                    .map_or(crate::Result::Ok(Mods::empty()), |m| {
                        m.as_str()
                            .split(',')
                            .try_fold(Mods::empty(), |acc, m| Ok(acc | m.trim().try_into()?))
                    })?,
                freemod: captures["freemod"].eq("enabled"),
            },
            "map-response" => Message::MpMapResponseMap(Map {
                id: captures["id"].parse()?,
                name: captures["name"].to_string(),
            }),
            "map-gamemode" => Message::MpMapResponseGameMode(captures["mode"].try_into()?),
            "map-error" => Message::MpMapInvalidError,

            // multiplayer slot-related commands
            // responses to `!mp host` and `!mp clearhost`
            "host-response" => Message::MpHostResponse(captures["user"].try_into()?),
            "clearhost-response" => Message::MpClearHostResponse,
            // responses to `!mp lock` and `!mp unlock`
            "lock-response" => Message::MpLockResponse,
            "unlock-response" => Message::MpUnlockResponse,
            // response to `!mp move`
            "move-response" => Message::MpMoveResponse {
                user: captures["user"].try_into()?,
                slot: captures["slot"].parse().map(|x: usize| x - 1)?,
            },
            "move-error" => Message::MpMoveError {
                user: captures["user"].try_into()?,
                slot: captures["slot"].parse().map(|x: usize| x - 1)?,
            },
            // response to `!mp team`
            "team-response" => Message::MpTeamResponse {
                user: captures["user"].try_into()?,
                team: captures["team"].try_into()?,
            },

            // get multiplayer game settings
            // responses to `!mp settings`
            "settings-name" => Message::MpSettingsResponseRoomName {
                name: captures["name"].to_string(),
                id: captures["id"].parse()?,
            },
            "settings-map" => Message::MpSettingsResponseMap {
                name: captures["name"].to_string(),
                id: captures["id"].parse()?,
            },
            "settings-team" => Message::MpSettingsResponseTeamMode {
                team_mode: captures["team"].try_into()?,
                score_mode: captures["score"].try_into()?,
            },
            "settings-mods" => {
                let mut freemod = false;
                let mods: Vec<_> = captures["mods"]
                    .split(',')
                    .filter(|&m| match m.trim() {
                        "Freemod" => {
                            freemod = true;
                            false
                        }
                        _ => true,
                    })
                    .collect();
                Message::MpSettingsResponseActiveMods {
                    freemod,
                    mods: mods.iter().try_fold(Mods::empty(), |acc, &m| {
                        crate::Result::Ok(acc | m.try_into()?)
                    })?,
                }
            }
            "settings-size" => Message::MpSettingsResponsePlayers(captures["size"].parse()?),
            "settings-slot" => {
                // Raw `attrs` from captures are some string like "Host / Team Red  / Hidden, HardRock".
                // Separate a compact `attrs` and `mods` from raw `attrs` for convenience.
                let (attrs, mods) = match captures.name("attrs") {
                    Some(attrs) => {
                        let (p_attrs, p_mods): (Vec<_>, Vec<_>) = attrs
                            .as_str()
                            .split(&['/', ','])
                            .into_iter()
                            .map(|a| a.trim())
                            .partition(|&a| a.eq("Host") || a.starts_with("Team "));
                        (Some(p_attrs), Some(p_mods))
                    }
                    None => (None, None),
                };
                Message::MpSettingsResponseSlot {
                    slot: captures["slot"].parse().map(|x: usize| x - 1)?,
                    status: captures["status"].try_into()?,
                    user: User {
                        id: captures["id"].parse()?,
                        name: captures["user"].trim().to_string(),
                    },

                    // Find "Host" in `attrs` as a host indicator
                    host: attrs
                        .as_ref()
                        .map_or(false, |attrs| attrs.into_iter().any(|&a| a == "Host")),
                    // Find "Team Red" / "Team Blue" in `attrs` and convert them into `Team` values
                    team: attrs.as_ref().and_then(|attrs| {
                        attrs
                            .into_iter()
                            .find(|a| a.starts_with("Team "))
                            .and_then(|t| t.trim_start_matches("Team ").try_into().ok())
                    }),
                    // Convert `mods` string to `Mods` value and collect them
                    mods: mods
                        .as_ref()
                        .map_or(crate::Result::Ok(Mods::empty()), |m| {
                            m.into_iter()
                                .try_fold(Mods::empty(), |acc, &m| Ok(acc | m.try_into()?))
                        })?,
                }
            }

            // multiplayer match commands
            // responses to `!mp start`
            "start-response" => Message::MpStartResponse,
            "start-timer" => Message::MpStartTimerResponse,
            "start-duplicated" => Message::MpStartDuplicatedError,
            // response to `!mp aborttimer`
            "aborttimer-response" => Message::MpAbortTimerResponse,
            "aborttimer-start" => Message::MpAbortStartTimerResponse,
            // response to `!mp abort`
            "abort-response" => Message::MpAbortResponse,

            // multiplayer player management
            // response to `!mp invite`
            "invite-response" => Message::MpInviteResponse(captures["user"].try_into()?),
            "invite-already" => Message::MpInviteAlready,
            // responses to `!mp kick` and `!mp ban`
            "kick-response" => Message::MpKickResponse(captures["user"].try_into()?),
            "ban-response" => Message::MpBanResponse(captures["user"].try_into()?),
            // responses to `!mp addref` and `!mp removeref`
            "addref-response" => Message::MpAddrefResponse(captures["referee"].try_into()?),
            "removeref-response" => Message::MpRemoverefResponse(captures["referee"].try_into()?),
            "listrefs-prompt" => Message::MpListRefsPrompt,
            "ref-error" => Message::MpRefError,

            // general errors
            // when parameter of a command is an invalid user, e.g. `!mp host` `!stats`
            "error-user-not-found" => Message::UserNotFoundError,

            // multiplayer game events
            // slot-related events
            "event-player-joined" => Message::PlayerJoinedEvent {
                user: captures["user"].try_into()?,
                slot: captures["slot"].parse().map(|x: usize| x - 1)?,
                team: captures
                    .name("team")
                    .map(|x| x.as_str().try_into())
                    .transpose()?,
            },
            "event-player-moved" => Message::PlayerMovedEvent {
                user: captures["user"].try_into()?,
                slot: captures["slot"].parse().map(|x: usize| x - 1)?,
            },
            "event-player-team" => Message::PlayerChangedTeamEvent {
                user: captures["user"].try_into()?,
                team: captures["team"].try_into()?,
            },
            "event-player-left" => Message::PlayerLeftEvent(captures["user"].try_into()?),

            // host-related events
            "event-host-changed" => Message::HostChangedEvent(captures["user"].try_into()?),
            "event-host-changing-map" => Message::HostChangingMapEvent,
            "event-map-changed" => Message::MapChangedEvent(Map {
                id: captures["id"].parse()?,
                name: captures["name"].try_into()?,
            }),

            // match-related events
            "event-players-ready" => Message::PlayersReadyEvent,
            "event-match-started" => Message::MatchStartedEvent,
            "event-match-player-result" => Message::MatchPlayerScoreEvent {
                user: captures["user"].try_into()?,
                score: captures["score"].parse()?,
                alive: captures["alive"].eq("PASSED"),
            },
            "event-match-finished" => Message::MatchFinishedEvent,

            // timer related events
            "event-starttimer" => Message::StartTimer(
                captures[1].parse::<usize>()?
                    * (match &captures[2] {
                        "second" => 1,
                        "minute" => 60,
                        _ => 0,
                    })
                    + captures
                        .get(3)
                        .map(|x| x.as_str().parse::<usize>())
                        .transpose()?
                        .unwrap_or(0),
            ),
            // "event-starttimer-end" =>
            "event-timer" => Message::Timer(
                captures[1].parse::<usize>()?
                    * (match &captures[2] {
                        "second" => 1,
                        "minute" => 60,
                        _ => 0,
                    })
                    + captures
                        .get(3)
                        .map(|x| x.as_str().parse::<usize>())
                        .transpose()?
                        .unwrap_or(0),
            ),
            "event-timer-end" => Message::TimerEnd,
            k => crate::Result::Err(format!("Unimplemented bot message type \"{}\"", k).into())?,
        })
    }

    pub fn matches(&self, s: &str) -> crate::Result<Message> {
        self.matcher
            .matches(s)
            .into_iter()
            .next()
            .map_or(Ok(Message::Raw(s.to_string())), |(i, captures)| {
                MessageMatcher::try_match(self.keys[i], captures)
            })
    }
}

#[derive(Debug, Clone)]
pub struct ResponseMatcher {
    username: String,
    command: Command,
    history: Vec<Message>,
}

impl ResponseMatcher {
    pub fn new(username: String, command: &Command) -> Self {
        Self {
            username,
            command: command.clone(),
            history: Vec::new(),
        }
    }
    fn next_inner(&mut self, message: &Message) -> Option<ResponseInner> {
        let last = self.history.last().take().clone();

        match &self.command {
            Command::Where(u) => match message {
                Message::WhereResponse { user, location } => {
                    if u.name.eq(user) {
                        Some(ResponseInner::Where(WhereResponse {
                            user: u.clone(),
                            location: location.clone(),
                        }))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            Command::Stats(u) => match (last, message) {
                (None, Message::StatsResponseStatus { user, .. }) => {
                    if u.eq(user) {
                        self.history.push(message.clone());
                    }
                    None
                }
                (
                    Some(Message::StatsResponseStatus { .. }),
                    Message::StatsResponseScoreRank { .. },
                )
                | (
                    Some(Message::StatsResponseScoreRank { .. }),
                    Message::StatsResponsePlayCountLevel { .. },
                ) => {
                    self.history.push(message.clone());
                    None
                }
                (
                    Some(Message::StatsResponsePlayCountLevel { .. }),
                    Message::StatsResponseAccuracy { .. },
                ) => {
                    self.history.push(message.clone());
                    let mut response = StatsResponse::default();
                    for message in self.history.iter() {
                        match message.clone() {
                            Message::StatsResponseStatus { user, status } => {
                                response.user = user;
                                response.status = status;
                            }
                            Message::StatsResponseScoreRank { score, rank } => {
                                response.score = score;
                                response.rank = rank;
                            }
                            Message::StatsResponsePlayCountLevel { play_count, level } => {
                                response.play_count = play_count;
                                response.level = level;
                            }
                            Message::StatsResponseAccuracy { accuracy } => {
                                response.accuracy = accuracy;
                            }
                            _ => unreachable!(),
                        }
                    }
                    Some(ResponseInner::Stats(response))
                }
                _ => None,
            },
            Command::Roll(_) => match message {
                Message::RollResponse { user, value } => {
                    if user.eq(&self.username) {
                        Some(ResponseInner::Roll(*value))
                    } else {
                        None
                    }
                }
                _ => None,
            },

            Command::Make(t) => match message {
                Message::MpMakeResponse { id, title } => {
                    if t.eq(title) {
                        Some(ResponseInner::MpMake(MpMakeResponse {
                            id: *id,
                            title: title.clone(),
                        }))
                    } else {
                        None
                    }
                }
                _ => None,
            },

            Command::Close => match message {
                Message::MpCloseResponse => Some(ResponseInner::Ok),
                _ => None,
            },

            Command::Name(n) => match message {
                Message::MpNameResponse(name) => {
                    if n.eq(name) {
                        Some(ResponseInner::Ok)
                    } else {
                        None
                    }
                }
                _ => None,
            },

            Command::Password(p) => match message {
                Message::MpPasswordResponseChanged => {
                    if !p.is_empty() {
                        Some(ResponseInner::Ok)
                    } else {
                        None
                    }
                }
                Message::MpPasswordResponseRemoved => {
                    if p.is_empty() {
                        Some(ResponseInner::Ok)
                    } else {
                        None
                    }
                }
                _ => None,
            },

            Command::Size(s) => match message {
                Message::MpSizeResponse(size) => {
                    if std::cmp::min(*s, MAX_SLOTS) == *size {
                        Some(ResponseInner::Ok)
                    } else {
                        None
                    }
                }
                _ => None,
            },

            Command::Set {
                team_mode: tm,
                score_mode: sm,
                size: s,
            } => match message {
                Message::MpSetResponse {
                    team_mode,
                    score_mode,
                    size,
                } => {
                    if tm.eq(team_mode)
                        && sm.eq(score_mode)
                        && s.map(|s| std::cmp::min(s, MAX_SLOTS)).eq(size)
                    {
                        Some(ResponseInner::MpSet(MpSetResponse {
                            size: *size,
                            team_mode: *team_mode,
                            score_mode: *score_mode,
                        }))
                    } else {
                        None
                    }
                }
                _ => None,
            },

            Command::Mods {
                mods: m,
                freemod: fm,
            } => match message {
                Message::MpModsResponse { mods, freemod } => {
                    Some(ResponseInner::MpMods(MpModsResponse {
                        mods: *mods,
                        freemod: *freemod,
                    }))
                }
                _ => None,
            },

            Command::Map(m, None) => match message {
                Message::MpMapResponseMap(map) => {
                    if m.eq(map) {
                        Some(ResponseInner::MpMap(MpMapResponse {
                            map: map.clone(),
                            mode: None,
                        }))
                    } else {
                        None
                    }
                }
                _ => None,
            },

            Command::Map(m, Some(gm)) => match (last, message) {
                (None, Message::MpMapResponseMap(map)) => {
                    if m.eq(map) {
                        self.history.push(message.clone());
                    }
                    None
                }
                (
                    Some(Message::MpMapResponseMap(map)),
                    Message::MpMapResponseGameMode(game_mode),
                ) => Some(ResponseInner::MpMap(MpMapResponse {
                    map: map.clone(),
                    mode: Some(game_mode.clone()),
                })),
                _ => None,
            },

            Command::Host(h) => match message {
                Message::MpHostResponse(host) => {
                    if h.eq(host) {
                        Some(ResponseInner::MpHost(h.merge(host)))
                    } else {
                        None
                    }
                }
                _ => None,
            },

            Command::ClearHost => match message {
                Message::MpClearHostResponse => Some(ResponseInner::Ok),
                _ => None,
            },

            Command::Lock => match message {
                Message::MpLockResponse => Some(ResponseInner::Ok),
                _ => None,
            },

            Command::Unlock => match message {
                Message::MpUnlockResponse => Some(ResponseInner::Ok),
                _ => None,
            },

            Command::Move { user: u, slot: s } => match message {
                Message::MpMoveResponse { user, slot } => {
                    if u.eq(user) && s.eq(slot) {
                        Some(ResponseInner::MpMove(MpMoveResponse {
                            user: u.merge(user),
                            slot: *slot,
                        }))
                    } else {
                        None
                    }
                }
                Message::MpMoveError { user, slot } => {
                    if u.eq(user) && s.eq(slot) {
                        Some(ResponseInner::Err(Error::MpMoveFailed))
                    } else {
                        None
                    }
                }
                _ => None,
            },

            Command::Settings => match (last, message) {
                (None, Message::MpSettingsResponseRoomName { .. })
                | (
                    Some(Message::MpSettingsResponseRoomName { .. }),
                    Message::MpSettingsResponseMap { .. },
                ) 
                | (
                    Some(Message::MpSettingsResponseRoomName { .. }),
                    Message::MpSettingsResponseTeamMode { .. },
                )
                | (
                    Some(Message::MpSettingsResponseMap { .. }),
                    Message::MpSettingsResponseTeamMode { .. },
                )
                | (
                    Some(Message::MpSettingsResponseTeamMode { .. }),
                    Message::MpSettingsResponseActiveMods { .. },
                )
                | (
                    Some(Message::MpSettingsResponseTeamMode { .. }),
                    Message::MpSettingsResponsePlayers { .. },
                )
                | (
                    Some(Message::MpSettingsResponseActiveMods { .. }),
                    Message::MpSettingsResponsePlayers { .. },
                ) => {
                    self.history.push(message.clone());
                    None
                }
                (
                    Some(&Message::MpSettingsResponsePlayers(counter)),
                    Message::MpSettingsResponseSlot { .. },
                ) => {
                    // This is a bit of a hack, but it works.
                    // We will get total count of current player slots in MpSettingsResponsePlayers.
                    // By prepending slot messages to history, when we get the same count of slot messages
                    // as in MpSettingsResponsePlayers, we know that we have all the slots.
                    self.history.insert(0, message.clone());

                    if self
                        .history
                        .iter()
                        .filter(|m| matches!(m, Message::MpSettingsResponseSlot { .. }))
                        .count()
                        == counter
                    {
                        let mut response = MpSettingsResponse::default();
                        for message in self.history.iter() {
                            match message.clone() {
                                Message::MpSettingsResponseRoomName { id, name } => {
                                    response.id = id;
                                    response.name = name;
                                }
                                Message::MpSettingsResponseMap { id, name } => {
                                    response.map = Some(Map { id, name });
                                }
                                Message::MpSettingsResponseTeamMode {
                                    team_mode,
                                    score_mode,
                                } => {
                                    response.team_mode = team_mode;
                                    response.score_mode = score_mode;
                                }
                                Message::MpSettingsResponseActiveMods { mods, freemod } => {
                                    response.mods = mods;
                                    response.freemod = freemod;
                                }
                                Message::MpSettingsResponsePlayers(size) => {
                                    response.size = size;
                                }
                                Message::MpSettingsResponseSlot {
                                    slot,
                                    status,
                                    user,
                                    mods,
                                    host,
                                    team,
                                } => {
                                    response.slots[slot] = Some(Slot {
                                        status,
                                        user,
                                        mods,
                                        host,
                                        team,
                                    });
                                }
                                _ => {}
                            }
                        }
                        Some(ResponseInner::MpSettings(response))
                    } else {
                        None
                    }
                }
                _ => None,
            },

            Command::Start(None) => match message {
                Message::MpStartResponse
                | Message::MpStartDuplicatedError
                | Message::MatchStartedEvent => Some(ResponseInner::Ok),
                _ => None,
            },

            Command::ListRefs => match (last, message) {
                (None, Message::MpListRefsPrompt)
                | (Some(Message::MpListRefsPrompt), Message::Raw(_))
                | (Some(Message::Raw(_)), Message::Raw(_)) => {
                    self.history.push(message.clone());
                    None
                }
                (Some(Message::Raw(_)), _) => {
                    let mut referees = Vec::new();
                    for message in self.history.iter() {
                        match message {
                            Message::Raw(referee) => match referee.as_str().try_into() {
                                Ok(referee) => {
                                    referees.push(referee);
                                }
                                Err(_) => {}
                            },
                            _ => {}
                        }
                    }
                    Some(ResponseInner::MpListRefs(referees))
                }
                _ => None,
            },
            _ => None,
        }
    }

    pub fn next(&mut self, message: &Message) -> Option<Response> {
        self.next_inner(message).map(|inner| Response {
            source: self.command.clone(),
            inner,
        })
    }

    fn end_inner(&mut self) -> Option<ResponseInner> {
        let last = self.history.last();
        match (&self.command, last) {
            (Command::ListRefs, Some(Message::Raw(_))) => {
                let mut referees = Vec::new();
                for message in self.history.iter() {
                    match message {
                        Message::Raw(referee) => match referee.as_str().try_into() {
                            Ok(referee) => {
                                referees.push(referee);
                            }
                            Err(_) => {}
                        },
                        _ => {}
                    }
                }
                Some(ResponseInner::MpListRefs(referees))
            }
            (_, _) => None,
        }
    }

    /// `ResponseMatcher::end` is supposed to be called via some timeout mechanism.
    ///
    /// This is designed for those commands (e.g. `!mp listrefs`) which don't have clear response boundaries.
    /// Considering the following message sequence:
    ///
    /// Message::MpListRefsPrompt
    /// Message::Raw("ref1")
    /// Message::Raw("ref2")
    /// Message::Raw("ref3")
    ///
    /// If there are no further messages `ResponseMatcher::next`, the state machine will be stuck.
    pub fn end(&mut self) -> Option<Response> {
        self.end_inner().map(|inner| Response {
            source: self.command.clone(),
            inner,
        })
    }

    pub fn reset(&mut self) {
        self.history.clear();
    }

    #[allow(dead_code)]
    fn history(&self) -> &[Message] {
        &self.history
    }
}

#[cfg(test)]
mod tests {
    use crate::bancho::{
        multiplayer::{ScoreMode, Slot, SlotStatus, Team, TeamMode},
        Map, Mods, User,
    };

    use super::{Command, Message, ResponseMatcher};
    #[test]
    fn make() {
        let mut matcher = ResponseMatcher::new("".to_string(), &Command::Make("lobby".to_string()));
        let messages = &[Message::MpMakeResponse {
            id: 123,
            title: "lobby".to_string(),
        }];
        for message in messages {
            eprintln!("Mock bot message: {:?}", message);
            match matcher.next(message) {
                Some(response) => {
                    let settings = response.make();
                    assert_eq!(settings.id, 123);
                    assert_eq!(settings.title, "lobby".to_string());
                    return;
                }
                _ => {}
            }
        }
        panic!("Expected response not found")
    }
    #[test]
    fn match_settings() {
        let mut matcher = ResponseMatcher::new("".to_string(), &Command::Settings);
        let messages = &[
            Message::MpSettingsResponseRoomName {
                name: "test".to_string(),
                id: 0xdeadbeefu64,
            },
            Message::MpSettingsResponseMap {
                name: "map".to_string(),
                id: 0xbeefdeadu64,
            },
            Message::MpSettingsResponseTeamMode {
                team_mode: TeamMode::TagTeam,
                score_mode: ScoreMode::ScoreV2,
            },
            Message::MpSettingsResponseActiveMods {
                mods: Mods::HardRock | Mods::DoubleTime,
                freemod: false,
            },
            Message::MpSettingsResponsePlayers(3),
            Message::MpSettingsResponseSlot {
                slot: 0,
                status: SlotStatus::NotReady,
                user: User {
                    id: 1,
                    name: "user1".to_string(),
                },
                mods: Mods::empty(),
                host: false,
                team: Some(Team::Red),
            },
            Message::MpSettingsResponseSlot {
                slot: 1,
                status: SlotStatus::NoMap,
                user: User {
                    id: 4,
                    name: "user2".to_string(),
                },
                mods: Mods::empty(),
                host: false,
                team: Some(Team::Red),
            },
            Message::MpSettingsResponseSlot {
                slot: 15,
                status: SlotStatus::NoMap,
                user: User {
                    id: 7,
                    name: "user3".to_string(),
                },
                mods: Mods::empty(),
                host: true,
                team: Some(Team::Blue),
            },
        ];
        for message in messages {
            eprintln!("Mock bot message: {:?}", message);
            match matcher.next(message) {
                Some(response) => {
                    let settings = response.settings();
                    eprintln!("Get mocked bot response: {:?}", settings);
                    assert_eq!(settings.name, "test");
                    assert_eq!(settings.id, 0xdeadbeefu64);
                    assert_eq!(settings.map, Some(Map { id: 0xbeefdeadu64, name: "map".to_string() }));
                    assert_eq!(settings.mods, Mods::HardRock | Mods::DoubleTime);
                    assert_eq!(settings.freemod, false);
                    assert_eq!(settings.team_mode, TeamMode::TagTeam);
                    assert_eq!(settings.score_mode, ScoreMode::ScoreV2);
                    assert_eq!(settings.size, 3);
                    assert_eq!(
                        settings.slots[0].as_ref().unwrap(),
                        &Slot {
                            user: User {
                                id: 1,
                                name: "user1".to_string()
                            },
                            status: SlotStatus::NotReady,
                            mods: Mods::empty(),
                            host: false,
                            team: Some(Team::Red),
                        }
                    );
                    assert_eq!(
                        settings.slots[1].as_ref().unwrap(),
                        &Slot {
                            user: User {
                                id: 4,
                                name: "user2".to_string()
                            },
                            status: SlotStatus::NoMap,
                            mods: Mods::empty(),
                            host: false,
                            team: Some(Team::Red),
                        }
                    );
                    assert_eq!(
                        settings.slots[15].as_ref().unwrap(),
                        &Slot {
                            user: User {
                                id: 7,
                                name: "user3".to_string()
                            },
                            status: SlotStatus::NoMap,
                            mods: Mods::empty(),
                            host: true,
                            team: Some(Team::Blue),
                        }
                    );
                    return;
                }
                _ => {}
            }
        }
        assert_eq!(matcher.history().len(), messages.len());
        panic!("Expected response not found")
    }
}
