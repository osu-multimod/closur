use std::fmt;
use std::{borrow::Borrow, str::FromStr, time::Duration};

use super::{bot, Channel, Map, Mods, Operator, User, UserScore};

use crate::{error::ConversionError, StdResult};

#[derive(Debug)]
pub struct RangeError {
    number: usize,
    limit: usize,
}

impl RangeError {
    pub fn number(&self) -> usize {
        self.number
    }
    pub fn limit(&self) -> usize {
        self.limit
    }
}

impl fmt::Display for RangeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "number {} is out of range 0..={}",
            self.number, self.limit
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct SlotRanged<const MAX: usize>(u8);
impl<const MAX: usize> SlotRanged<MAX> {
    pub const MIN_USIZE: usize = 0;
    pub const MAX_USIZE: usize = MAX;
    pub const MIN: Self = Self(0);
    pub const MAX: Self = Self(MAX as u8);

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl AsRef<u8> for SlotRanged<MAX_SLOT_INDEX> {
    fn as_ref(&self) -> &u8 {
        &self.0
    }
}

impl<const MAX: usize> Into<usize> for SlotRanged<MAX> {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl<const MAX: usize> From<u8> for SlotRanged<MAX> {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl<const MAX: usize> From<u32> for SlotRanged<MAX> {
    fn from(value: u32) -> Self {
        Self(value as u8)
    }
}

impl<const MAX: usize> From<i32> for SlotRanged<MAX> {
    fn from(value: i32) -> Self {
        Self(value as u8)
    }
}

impl<const MAX: usize> From<usize> for SlotRanged<MAX> {
    fn from(value: usize) -> Self {
        Self(value as u8)
    }
}

impl<const MAX: usize> FromStr for SlotRanged<MAX> {
    type Err = core::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let index = s.parse::<u8>()?;
        Ok(Self(index))
    }
}

impl<const MAX: usize> fmt::Display for SlotRanged<MAX> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0 as usize)
    }
}

pub const MAX_SLOT_INDEX: usize = 15;
pub const MAX_SLOT_SIZE: usize = 16;

pub type SlotIndex = SlotRanged<MAX_SLOT_INDEX>;
pub type SlotSize = SlotRanged<MAX_SLOT_SIZE>;
pub type RawSlots = [Option<Slot>; SlotSize::MAX_USIZE];

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Slots(pub(super) [Option<Slot>; SlotSize::MAX_USIZE]);

impl Slots {
    pub fn as_slice(&self) -> &[Option<Slot>] {
        &self.0[..]
    }
    pub fn as_mut_slice(&mut self) -> &mut [Option<Slot>] {
        &mut self.0[..]
    }
    pub fn into_inner(self) -> [Option<Slot>; SlotSize::MAX_USIZE] {
        self.0
    }
    pub fn valid_slots(&self) -> impl Iterator<Item = (SlotIndex, &Slot)> {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(i, s)| s.as_ref().map(|s| (SlotIndex::from(i), s)))
    }
}

impl AsRef<[Option<Slot>]> for Slots {
    fn as_ref(&self) -> &[Option<Slot>] {
        &self.0[..]
    }
}

impl AsMut<[Option<Slot>]> for Slots {
    fn as_mut(&mut self) -> &mut [Option<Slot>] {
        &mut self.0[..]
    }
}

impl IntoIterator for Slots {
    type Item = Option<Slot>;
    type IntoIter = std::array::IntoIter<Option<Slot>, MAX_SLOT_SIZE>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

pub type MatchId = u64;
pub type MatchInternalId = u32;

#[derive(Debug)]
pub struct Match<'a> {
    pub(super) id: MatchId,
    /// internal match id, appeared in invite link e.g. `osump://123456`
    pub(super) internal_id: MatchInternalId,
    pub(super) operator: Operator<'a>,
}

impl<'a> Match<'a> {
    pub fn channel(&self) -> Channel {
        Channel::Multiplayer(self.id)
    }
    pub fn id(&self) -> MatchId {
        self.id
    }
    pub fn internal_id(&self) -> MatchInternalId {
        self.internal_id
    }
    pub fn invite_url(&self) -> String {
        format!("osump://{}/", self.internal_id)
    }
    pub fn invite_url_with_password(&self, password: impl AsRef<str>) -> String {
        let mut url = self.invite_url();
        url.push_str(password.as_ref());
        url
    }
    pub async fn send_bot_command<C: bot::Command>(
        &self,
        command: impl Borrow<C>,
    ) -> StdResult<bot::Response<C>, super::OperatorError<super::Action>> {
        self.operator
            .send_bot_command(Some(self.channel()), command)
            .await
    }
    pub async fn send_chat(
        &self,
        content: impl AsRef<str>,
    ) -> StdResult<(), super::OperatorError<super::Action>> {
        self.operator
            .send_channel_chat(self.channel(), content)
            .await
    }
    pub async fn send_chats<S: AsRef<str>>(
        &self,
        contents: impl AsRef<[S]>,
    ) -> StdResult<(), super::OperatorError<super::Action>> {
        self.operator
            .send_channel_chats(self.channel(), contents)
            .await
    }
    pub async fn operator(&self) -> &Operator {
        &self.operator
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Slot {
    pub(crate) user: User,
    pub(crate) host: bool,
    pub(crate) status: SlotStatus,
    pub(crate) team: Option<Team>,
    pub(crate) mods: Mods,
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
    pub fn team(&self) -> Option<&Team> {
        self.team.as_ref()
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

impl FromStr for Team {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

impl FromStr for TeamMode {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
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

impl FromStr for ScoreMode {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
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

impl FromStr for SlotStatus {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
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

impl FromStr for GameMode {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}

#[derive(Debug, Clone)]
pub struct Event {
    pub(super) channel: Channel,
    pub(super) match_id: MatchId,
    pub(super) match_internal_id: MatchInternalId,
    pub(super) kind: EventKind,
}

impl Event {
    pub fn match_id(&self) -> MatchId {
        self.match_id
    }
    pub fn match_internal_id(&self) -> MatchInternalId {
        self.match_internal_id
    }
    pub fn channel(&self) -> &Channel {
        &self.channel
    }
    pub fn kind(&self) -> &EventKind {
        &self.kind
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EventKind {
    PlayerJoined {
        user: User,
        slot: SlotIndex,
        team: Option<Team>,
    },
    PlayerMoved {
        user: User,
        slot: SlotIndex,
    },
    PlayerChangedTeam {
        user: User,
        team: Team,
    },
    PlayerLeft {
        user: User,
    },
    HostChanged {
        user: User,
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
        score: UserScore,
        alive: bool,
    },
    StartTimer {
        time: Duration,
    },
    StartTimerEnd,
    TimerTick {
        time: Duration,
    },
    TimerEnd,
}

impl EventKind {
    pub(super) fn from_bot(kind: bot::MessageKind) -> Self {
        match kind {
            bot::MessageKind::EventPlayerJoined { user, slot, team } => {
                Self::PlayerJoined { user, slot, team }
            }
            bot::MessageKind::EventPlayerMoved { user, slot } => Self::PlayerMoved { user, slot },
            bot::MessageKind::EventPlayerTeam { user, team } => {
                Self::PlayerChangedTeam { user, team }
            }
            bot::MessageKind::EventPlayerLeft { user } => Self::PlayerLeft { user },
            bot::MessageKind::EventHostChanged { user } => Self::HostChanged { user },
            bot::MessageKind::EventHostChangingMap => Self::HostChangingMap,
            bot::MessageKind::EventMapChanged { map } => Self::MapChanged { map },
            bot::MessageKind::EventPlayersReady => Self::PlayersReady,
            bot::MessageKind::EventMatchStarted => Self::MatchStarted,
            bot::MessageKind::MpAbortResponse => Self::MatchAborted,
            bot::MessageKind::EventMatchFinished => Self::MatchFinished,
            bot::MessageKind::EventMatchPlayerResult { user, score, alive } => {
                Self::MatchPlayerScore { user, score, alive }
            }
            bot::MessageKind::EventStartTimer { time } => Self::StartTimer { time },
            bot::MessageKind::EventStartTimerEndLuck => Self::StartTimerEnd,
            bot::MessageKind::EventTimer { time } => Self::TimerTick { time },
            bot::MessageKind::EventTimerEnd => Self::TimerEnd,
            _ => unreachable!(),
        }
    }
}
