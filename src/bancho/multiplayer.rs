use core::fmt;
use tokio::sync::broadcast;

use super::{
    bot::{self, MpSettingsResponse},
    Channel, ChannelSender, Map, Mods, User,
};

use crate::{
    error::{ConversionError},
    StdResult,
};

pub const MAX_SLOTS: usize = 16;

pub type Slots = [Option<Slot>; MAX_SLOTS];

#[derive(Debug)]
pub struct Match {
    pub(super) id: u64,
    /// internal match id, appeared in invite link e.g. `osump://123456`
    pub(super) inner_id: u32,
    pub(super) event_rx: broadcast::Receiver<super::Event>,
    pub(super) writer: ChannelSender,
}

impl Match {
    pub fn id(&self) -> u64 {
        self.id
    }
    pub fn inner_id(&self) -> u32 {
        self.inner_id
    }
    pub async fn send_chat(&mut self, body: &str) -> crate::Result<()> {
        self.writer.send_chat(body).await
    }
    pub async fn send_bot_command(
        &mut self,
        command: bot::Command,
    ) -> crate::Result<bot::Response> {
        self.writer.send_bot_command(command).await
    }
    pub async fn send_unreliable_bot_command(
        &mut self,
        command: bot::Command,
    ) -> crate::Result<()> {
        self.writer.send_unreliable_bot_command(command).await
    }
    pub async fn settings(&mut self) -> crate::Result<MpSettingsResponse> {
        Ok(self
            .writer
            .send_bot_command(bot::Command::Settings)
            .await?
            .settings()
            .clone())
    }
    pub fn channel(&self) -> Channel {
        self.writer.channel.clone()
    }
    pub fn invite_link(&self, password: Option<&str>) -> String {
        format!("osump://{}/{}", self.inner_id, password.unwrap_or(""))
    }
    pub fn events(&self) -> broadcast::Receiver<super::Event> {
        self.event_rx.resubscribe()
    }
    pub async fn channel_writer(&self) -> &ChannelSender {
        &self.writer
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Slot {
    pub(super) user: User,
    pub(super) host: bool,
    pub(super) status: SlotStatus,
    pub(super) team: Option<Team>,
    pub(super) mods: Mods,
}

impl Slot {
    pub fn user(&self) -> &User {
        &self.user
    }
    pub fn host(&self) -> bool {
        self.host
    }
    pub fn status(&self) -> SlotStatus {
        self.status
    }
    pub fn team(&self) -> Option<Team> {
        self.team
    }
    pub fn mods(&self) -> Mods {
        self.mods
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Team {
    Blue,
    Red,
}

impl Default for Team {
    fn default() -> Self {
        Team::Red
    }
}

impl fmt::Display for Team {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Team::Blue => write!(f, "Blue"),
            Team::Red => write!(f, "Red"),
        }
    }
}

impl TryFrom<&str> for Team {
    type Error = ConversionError;
    fn try_from(s: &str) -> StdResult<Self, Self::Error> {
        match s.to_lowercase().as_str() {
            "blue" => Ok(Team::Blue),
            "red" => Ok(Team::Red),
            _ => Err(ConversionError::InvalidTeam),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TeamMode {
    Individual,
    TagCoop,
    Team,
    TagTeam,
}

impl Default for TeamMode {
    fn default() -> Self {
        TeamMode::Individual
    }
}

impl fmt::Display for TeamMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TeamMode::Individual => write!(f, "Individual"),
            TeamMode::TagCoop => write!(f, "Tag Coop"),
            TeamMode::Team => write!(f, "Team"),
            TeamMode::TagTeam => write!(f, "Tag Team"),
        }
    }
}

impl TryFrom<&str> for TeamMode {
    type Error = ConversionError;
    fn try_from(s: &str) -> StdResult<Self, Self::Error> {
        match s {
            "HeadToHead" => Ok(TeamMode::Individual),
            "TagCoop" => Ok(TeamMode::TagCoop),
            "TeamVs" => Ok(TeamMode::Team),
            "TagTeamVs" => Ok(TeamMode::TagTeam),
            _ => Err(ConversionError::InvalidTeamMode),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ScoreMode {
    Score,
    Accuracy,
    Combo,
    ScoreV2,
}

impl Default for ScoreMode {
    fn default() -> Self {
        ScoreMode::Score
    }
}

impl fmt::Display for ScoreMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScoreMode::Score => write!(f, "Score"),
            ScoreMode::Accuracy => write!(f, "Accuracy"),
            ScoreMode::Combo => write!(f, "Combo"),
            ScoreMode::ScoreV2 => write!(f, "Score V2"),
        }
    }
}

impl TryFrom<&str> for ScoreMode {
    type Error = ConversionError;
    fn try_from(s: &str) -> StdResult<Self, Self::Error> {
        match s.to_lowercase().as_str() {
            "score" => Ok(ScoreMode::Score),
            "accuracy" => Ok(ScoreMode::Accuracy),
            "combo" => Ok(ScoreMode::Combo),
            "score v2" => Ok(ScoreMode::ScoreV2),
            "scorev2" => Ok(ScoreMode::ScoreV2),
            _ => Err(ConversionError::InvalidScoreMode),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SlotStatus {
    NotReady,
    NoMap,
    Ready,
    Playing,
}

impl Default for SlotStatus {
    fn default() -> Self {
        SlotStatus::NotReady
    }
}

impl fmt::Display for SlotStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SlotStatus::NoMap => write!(f, "No Map"),
            SlotStatus::NotReady => write!(f, "Not Ready"),
            SlotStatus::Ready => write!(f, "Ready"),
            SlotStatus::Playing => write!(f, "Playing"),
        }
    }
}

impl TryFrom<&str> for SlotStatus {
    type Error = ConversionError;
    fn try_from(s: &str) -> StdResult<Self, Self::Error> {
        match s {
            "No Map" => Ok(SlotStatus::NoMap),
            "Not Ready" => Ok(SlotStatus::NotReady),
            "Ready" => Ok(SlotStatus::Ready),
            "Playing" => Ok(SlotStatus::Playing),
            _ => Err(ConversionError::InvalidSlotStatus),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum GameMode {
    Standard,
    Taiko,
    CatchTheBeat,
    Mania,
}

impl Default for GameMode {
    fn default() -> Self {
        GameMode::Standard
    }
}

impl TryFrom<&str> for GameMode {
    type Error = ConversionError;
    fn try_from(s: &str) -> StdResult<Self, Self::Error> {
        match s {
            "Osu" => Ok(GameMode::Standard),
            "Taiko" => Ok(GameMode::Taiko),
            "CatchTheBeat" => Ok(GameMode::CatchTheBeat),
            "OsuMania" => Ok(GameMode::Mania),
            _ => Err(ConversionError::InvalidGameMode),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Event {
    PlayerJoined {
        user: User,
        slot: usize,
        team: Option<Team>,
    },
    PlayerMoved {
        user: User,
        slot: usize,
    },
    PlayerChangedTeam {
        user: User,
        team: Team,
    },
    PlayerLeft {
        user: User,
    },
    HostChanged {
        host: User,
    },
    HostChangingMap,
    MapChanged {
        map: Map,
    },
    PlayersReady,
    MatchStarted,
    MatchAborted,
    MatchFinished,
    MatchPlayerScore {
        user: User,
        score: u64,
        alive: bool,
    },
}

#[derive(Debug, Clone)]
pub struct GameResult {
    user: User,
    score: u64,
    /// game results "PASSED / FAILED"
    alive: bool,
}
