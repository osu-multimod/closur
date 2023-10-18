use crate::bancho::{self, Channel};
use std::collections::HashMap;
use std::time::Duration;

use crate::bancho::multiplayer::MatchId;
use crate::bancho::multiplayer::{GameMode, ScoreMode, TeamMode};
use crate::bancho::multiplayer::{RawSlots, Slot, SlotIndex, SlotSize, SlotStatus, Slots, Team};
use crate::bancho::Mods;
use crate::bancho::{Map, MapId};
use crate::bancho::{
    User, UserAccuracy, UserId, UserLevel, UserPlayCount, UserRank, UserScore, UserStatus,
};
use crate::macros::regexset_enum;
use std::borrow::Borrow;

use regex::{Regex, RegexSet};

#[derive(Debug)]
pub struct MessageAssembler {
    matcher: RegexSetMatcher,
    needs_termination: bool,
    lines: Vec<Message>,
    #[allow(dead_code)]
    buckets: HashMap<String, Vec<Message>>,
}

impl MessageAssembler {
    pub fn new() -> Self {
        Self {
            matcher: RegexSetMatcher::new(MessageLine::patterns()),
            needs_termination: false,
            lines: Vec::new(),
            buckets: HashMap::new(),
        }
    }
    pub fn needs_termination(&self) -> bool {
        self.needs_termination
    }
    pub fn lines(&self) -> &[Message] {
        &self.lines
    }
    fn matches(
        &self,
        line: impl AsRef<str>,
    ) -> Result<Option<MessageLine>, Box<dyn std::error::Error + Send + Sync>> {
        self.matcher
            .matches(line.as_ref())
            .map(|(i, captures)| MessageLine::from_captures(i, captures))
            .transpose()
    }
    fn convert_kind_stateless(&self, line: MessageLine) -> MessageKind {
        match line {
            MessageLine::WhereResponse { user, location } => {
                MessageKind::WhereResponse { user, location }
            }
            MessageLine::WhereErrorOffline => MessageKind::WhereErrorOffline,
            MessageLine::WhereErrorUndetermined => MessageKind::WhereErrorUndetermined,

            MessageLine::StatsStatus { user, status } => MessageKind::StatsStatus { user, status },
            MessageLine::StatsScore { score, rank } => MessageKind::StatsScore { score, rank },
            MessageLine::StatsPlayCount { play_count, level } => {
                MessageKind::StatsPlayCount { play_count, level }
            }
            MessageLine::StatsAccuracy { accuracy } => MessageKind::StatsAccuracy { accuracy },

            MessageLine::RollResponse { user, value } => MessageKind::RollResponse { user, value },

            MessageLine::MpMakeResponse { id, name } => MessageKind::MpMakeResponse { id, name },
            MessageLine::MpMakeErrorLimited => MessageKind::MpMakeErrorLimited,
            MessageLine::MpMakeErrorNoName => MessageKind::MpMakeErrorNoName,

            MessageLine::MpCloseResponse => MessageKind::MpCloseResponse,

            MessageLine::MpNameResponse { name } => MessageKind::MpNameResponse { name },

            MessageLine::MpPasswordChanged => MessageKind::MpPasswordChanged,
            MessageLine::MpPasswordRemoved => MessageKind::MpPasswordRemoved,

            MessageLine::MpSizeResponse { size } => MessageKind::MpSizeResponse { size },
            MessageLine::MpSizeErrorInvalid => MessageKind::MpSizeErrorInvalid,

            MessageLine::MpSetResponse {
                size,
                team_mode,
                score_mode,
            } => MessageKind::MpSetResponse {
                size,
                team_mode,
                score_mode,
            },
            MessageLine::MpSetError => MessageKind::MpSetError,

            MessageLine::MpModsResponse { mods, freemod } => {
                MessageKind::MpModsResponse { mods, freemod }
            }

            MessageLine::MpMapResponseMap { map } => MessageKind::MpMapResponseMap { map },
            MessageLine::MpMapResponseGameMode { game_mode } => {
                MessageKind::MpMapResponseGameMode { game_mode }
            }
            MessageLine::MpMapErrorInvalid => MessageKind::MpMapErrorInvalid,

            MessageLine::MpHostResponse { user } => MessageKind::MpHostResponse { user },

            MessageLine::MpClearHostResponse => MessageKind::MpClearHostResponse,

            MessageLine::MpLockResponse => MessageKind::MpLockResponse,

            MessageLine::MpUnlockResponse => MessageKind::MpUnlockResponse,

            MessageLine::MpMoveResponse { user, slot } => {
                MessageKind::MpMoveResponse { user, slot }
            }
            MessageLine::MpMoveError { slot } => MessageKind::MpMoveError { slot },
            MessageLine::MpMoveErrorUserNotInMatch => MessageKind::MpMoveErrorUserNotInMatch,

            MessageLine::MpTeamResponse { user, team } => {
                MessageKind::MpTeamResponse { user, team }
            }
            MessageLine::MpTeamError => MessageKind::MpTeamError,

            MessageLine::MpSettingsRoom { name, id } => MessageKind::MpSettingsRoom { name, id },
            MessageLine::MpSettingsMap { map } => MessageKind::MpSettingsMap { map },
            MessageLine::MpSettingsTeamScoreModes {
                team_mode,
                score_mode,
            } => MessageKind::MpSettingsTeamScoreModes {
                team_mode,
                score_mode,
            },
            MessageLine::MpSettingsMods { mods, freemod } => {
                MessageKind::MpSettingsMods { mods, freemod }
            }
            MessageLine::MpSettingsSize { size } => MessageKind::MpSettingsSize { size },
            MessageLine::MpSettingsSlot {
                slot,
                status,
                user,
                mods,
                host: bool,
                team,
            } => MessageKind::MpSettingsSlot {
                slot,
                status,
                user,
                mods,
                host: bool,
                team,
            },

            MessageLine::MpStartResponse => MessageKind::MpStartResponse,
            MessageLine::MpStartTimer { time } => MessageKind::MpStartTimer { time },
            MessageLine::MpStartAlready => MessageKind::MpStartAlready,

            MessageLine::MpAbortTimerResponse => MessageKind::MpAbortTimerResponse,
            MessageLine::MpAbortTimerStart => MessageKind::MpAbortTimerStart,
            MessageLine::MpAbortResponse => MessageKind::MpAbortResponse,
            MessageLine::MpAbortErrorNotInProgress => MessageKind::MpAbortErrorNotInProgress,

            MessageLine::MpInviteResponse { user } => MessageKind::MpInviteResponse { user },
            MessageLine::MpInviteErrorAlreadyPresent => MessageKind::MpInviteErrorAlreadyPresent,

            MessageLine::MpKickResponse { user } => MessageKind::MpKickResponse { user },

            MessageLine::MpBanResponse { user } => MessageKind::MpBanResponse { user },

            MessageLine::MpAddRefResponse { user } => MessageKind::MpAddRefResponse { user },

            MessageLine::MpRemoveRefResponse { user } => MessageKind::MpRemoveRefResponse { user },
            MessageLine::MpRemoveRefErrorNotReferee { user } => {
                MessageKind::MpRemoveRefErrorNotReferee { user }
            }

            MessageLine::MpListRefsPrompt => MessageKind::MpListRefsPrompt,

            MessageLine::MpRefereeErrorUserNotFound { user } => {
                MessageKind::MpRefereeErrorUserNotFound { user }
            }

            MessageLine::ErrorUserNotFound => MessageKind::ErrorUserNotFound,
            MessageLine::ErrorUserUnspecified => MessageKind::ErrorUserUnspecified,
            MessageLine::MpErrorUserNotInMatch => MessageKind::MpErrorUserNotInMatch,
            MessageLine::MpErrorUserUnspecified => MessageKind::MpErrorUserUnspecified,

            MessageLine::EventPlayerJoined { user, slot, team } => {
                MessageKind::EventPlayerJoined { user, slot, team }
            }
            MessageLine::EventPlayerMoved { user, slot } => {
                MessageKind::EventPlayerMoved { user, slot }
            }
            MessageLine::EventPlayerTeam { user, team } => {
                MessageKind::EventPlayerTeam { user, team }
            }
            MessageLine::EventPlayerLeft { user } => MessageKind::EventPlayerLeft { user },
            MessageLine::EventHostChanged { user } => MessageKind::EventHostChanged { user },
            MessageLine::EventHostChangingMap => MessageKind::EventHostChangingMap,
            MessageLine::EventMapChanged { map } => MessageKind::EventMapChanged { map },
            MessageLine::EventPlayersReady => MessageKind::EventPlayersReady,
            MessageLine::EventMatchStarted => MessageKind::EventMatchStarted,
            MessageLine::EventMatchPlayerResult { user, score, alive } => {
                MessageKind::EventMatchPlayerResult { user, score, alive }
            }
            MessageLine::EventMatchFinished => MessageKind::EventMatchFinished,
            MessageLine::EventStartTimer { time } => MessageKind::EventStartTimer { time },
            MessageLine::EventStartTimerEndLuck => MessageKind::EventStartTimerEndLuck,
            MessageLine::EventTimer { time } => MessageKind::EventTimer { time },
            MessageLine::EventTimerEnd => MessageKind::EventTimerEnd,

            MessageLine::EventMaintenanceA
            | MessageLine::EventMaintenanceB
            | MessageLine::EventMaintenanceC
            | MessageLine::EventMaintenanceD => MessageKind::EventMaintenanceAlert,
            MessageLine::EventMaintenanceTimerMinutes { time }
            | MessageLine::EventMaintenanceTimerSeconds { time } => {
                MessageKind::EventMaintenanceTimer { time }
            }
            MessageLine::EventMaintenanceRightBack => MessageKind::EventMaintenanceRightBack,
        }
    }
    fn convert_fallback_stateful(&mut self, message: &bancho::Message) -> Option<Message> {
        match self.lines.last().map(|m| m.kind()) {
            Some(MessageKind::MpListRefsPrompt | MessageKind::MpListRefsLineUser { .. }) => {
                Some(Message {
                    channel: message.channel().cloned(),
                    kind: MessageKind::MpListRefsLineUser {
                        user: User::name_only(message.content()),
                    },
                })
            }
            _ => None,
        }
    }
    pub fn convert(&mut self, message: impl Borrow<bancho::Message>) -> Option<Message> {
        let message = message.borrow();
        let line = self.matches(message.content()).ok().and_then(|m| m);
        match line {
            Some(line) => {
                let kind = self.convert_kind_stateless(line);
                let message = Message {
                    channel: message.channel().cloned(),
                    kind,
                };
                Some(message)
            }
            None => self.convert_fallback_stateful(message),
        }
    }
    pub fn track(&mut self, message: impl Borrow<Message>) -> Option<Message> {
        let message = message.borrow();
        match (self.lines.last().map(|m| m.kind()), message.kind()) {
            (None, MessageKind::StatsStatus { .. })
            | (Some(MessageKind::StatsStatus { .. }), MessageKind::StatsScore { .. })
            | (Some(MessageKind::StatsScore { .. }), MessageKind::StatsPlayCount { .. })
            | (Some(MessageKind::StatsPlayCount { .. }), MessageKind::StatsAccuracy { .. }) => {
                self.lines.push(message.clone());
                if let MessageKind::StatsAccuracy { .. } = message.kind() {
                    let composed = Message::compose_stats(self.lines());
                    self.lines.clear();
                    Some(composed)
                } else {
                    None
                }
            }
            (None, MessageKind::MpSettingsRoom { .. })
            | (
                Some(MessageKind::MpSettingsRoom { .. }),
                MessageKind::MpSettingsMap { .. } | MessageKind::MpSettingsTeamScoreModes { .. },
            )
            | (
                Some(MessageKind::MpSettingsMap { .. }),
                MessageKind::MpSettingsTeamScoreModes { .. },
            )
            | (
                Some(MessageKind::MpSettingsTeamScoreModes { .. }),
                MessageKind::MpSettingsMods { .. } | MessageKind::MpSettingsSize { .. },
            )
            | (Some(MessageKind::MpSettingsMods { .. }), MessageKind::MpSettingsSize { .. })
            | (
                Some(MessageKind::MpSettingsSize { .. } | MessageKind::MpSettingsSlot { .. }),
                MessageKind::MpSettingsSlot { .. },
            ) => {
                self.lines.push(message.clone());
                let key_message = self.lines.iter().find(|m| match m.kind() {
                    MessageKind::MpSettingsSize { .. } => true,
                    _ => false,
                });
                if let Some(MessageKind::MpSettingsSize { size }) = key_message.map(|m| m.kind()) {
                    let present = self
                        .lines
                        .iter()
                        .filter(|m| match m.kind() {
                            MessageKind::MpSettingsSlot { .. } => true,
                            _ => false,
                        })
                        .count();
                    if present == size.as_usize() {
                        let composed = Message::compose_mp_settings(self.lines());
                        self.lines.clear();
                        Some(composed)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            (None, MessageKind::MpMapResponseMap { .. }) => {
                self.lines.push(message.clone());
                self.needs_termination = true;
                None
            }
            (
                Some(MessageKind::MpMapResponseMap { .. }),
                MessageKind::MpMapResponseGameMode { .. },
            ) => {
                self.lines.push(message.clone());
                let composed = Message::compose_mp_map(self.lines());
                self.lines.clear();
                Some(composed)
            }
            (None, MessageKind::MpListRefsPrompt)
            | (
                Some(MessageKind::MpListRefsPrompt | MessageKind::MpListRefsLineUser { .. }),
                MessageKind::MpListRefsLineUser { .. },
            ) => {
                self.lines.push(message.clone());
                if let MessageKind::MpListRefsLineUser { .. } = message.kind() {
                    self.needs_termination = true;
                }
                None
            }
            _ => {
                self.lines.clear();
                self.needs_termination = false;
                None
            }
        }
    }
    pub fn terminate(&mut self, message: Option<&Message>) -> Option<Message> {
        if self.needs_termination {
            match (
                self.lines.first().map(|m| m.kind()),
                message.map(|m| m.kind()),
            ) {
                (
                    Some(MessageKind::MpMapResponseMap { .. }),
                    Some(MessageKind::MpMapResponseGameMode { .. }),
                ) => None,
                (
                    Some(MessageKind::MpListRefsLineUser { .. }),
                    Some(MessageKind::MpListRefsLineUser { .. }),
                ) => None,

                (Some(MessageKind::MpMapResponseMap { .. }), _) => {
                    let composed = Message::compose_mp_map(self.lines());
                    self.lines.clear();
                    self.needs_termination = false;
                    Some(composed)
                }
                (Some(MessageKind::MpListRefsPrompt), _) => {
                    let composed = Message::compose_mp_listrefs(self.lines());
                    self.lines.clear();
                    self.needs_termination = false;
                    Some(composed)
                }
                _ => panic!("unexpected termination state"),
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Message {
    channel: Option<Channel>,
    kind: MessageKind,
}

impl Message {
    pub fn channel(&self) -> Option<&Channel> {
        self.channel.as_ref()
    }
    pub fn kind(&self) -> &MessageKind {
        &self.kind
    }
    pub fn is_stateful(&self) -> bool {
        self.kind.is_stateful()
    }
    pub fn is_multiplayer_event(&self) -> bool {
        self.kind.is_multiplayer_event()
    }
    pub fn is_maintenance(&self) -> bool {
        self.kind.is_maintenance_event()
    }
    pub fn maintenance(&self) -> Option<Duration> {
        self.kind.maintenance()
    }
}

impl Message {
    fn compose_stats(messages: &[Self]) -> Self {
        let mut composed_user = User::default();
        let mut composed_status = UserStatus::default();
        let mut composed_score = UserScore::default();
        let mut composed_rank = UserRank::default();
        let mut composed_play_count = UserPlayCount::default();
        let mut composed_level = UserLevel::default();
        let mut composed_accuracy = UserAccuracy::default();
        for message in messages {
            match message.kind() {
                MessageKind::StatsStatus { user, status } => {
                    composed_user = user.clone();
                    composed_status = *status;
                }
                MessageKind::StatsScore { score, rank } => {
                    composed_score = *score;
                    composed_rank = *rank;
                }
                MessageKind::StatsPlayCount { play_count, level } => {
                    composed_play_count = *play_count;
                    composed_level = *level;
                }
                MessageKind::StatsAccuracy { accuracy } => {
                    composed_accuracy = *accuracy;
                }
                _ => {}
            }
        }
        Self {
            channel: messages[0].channel().cloned(),
            kind: MessageKind::StatsResponse {
                user: composed_user,
                status: composed_status,
                score: composed_score,
                rank: composed_rank,
                play_count: composed_play_count,
                level: composed_level,
                accuracy: composed_accuracy,
            },
        }
    }
    fn compose_mp_settings(messages: &[Self]) -> Self {
        let mut composed_name = String::default();
        let mut composed_id = MatchId::default();
        let mut composed_map = None;
        let mut composed_team_mode = TeamMode::default();
        let mut composed_score_mode = ScoreMode::default();
        let mut composed_mods = Mods::default();
        let mut composed_freemod = bool::default();
        let mut composed_size = SlotSize::default();
        let mut slots = RawSlots::default();
        for message in messages {
            match message.kind() {
                MessageKind::MpSettingsRoom { name, id } => {
                    composed_name = name.clone();
                    composed_id = *id;
                }
                MessageKind::MpSettingsMap { map } => {
                    composed_map = Some(map.clone());
                }
                MessageKind::MpSettingsTeamScoreModes {
                    team_mode,
                    score_mode,
                } => {
                    composed_team_mode = *team_mode;
                    composed_score_mode = *score_mode;
                }
                MessageKind::MpSettingsMods { mods, freemod } => {
                    composed_mods = *mods;
                    composed_freemod = *freemod;
                }
                MessageKind::MpSettingsSize { size } => {
                    composed_size = *size;
                }
                MessageKind::MpSettingsSlot {
                    slot,
                    status,
                    user,
                    mods,
                    host,
                    team,
                } => {
                    slots[slot.as_usize()] = Some(Slot {
                        user: user.clone(),
                        host: *host,
                        status: *status,
                        team: *team,
                        mods: *mods,
                    });
                }
                _ => {}
            }
        }
        Self {
            channel: messages[0].channel().cloned(),
            kind: MessageKind::MpSettingsResponse {
                name: composed_name,
                id: composed_id,
                map: composed_map,
                team_mode: composed_team_mode,
                score_mode: composed_score_mode,
                mods: composed_mods,
                freemod: composed_freemod,
                size: composed_size,
                slots: Slots(slots),
            },
        }
    }
    fn compose_mp_map(messages: &[Self]) -> Self {
        let mut composed_map = Map::default();
        let mut composed_game_mode = None;
        for message in messages {
            match message.kind() {
                MessageKind::MpMapResponseMap { map } => {
                    composed_map = map.clone();
                }
                MessageKind::MpMapResponseGameMode { game_mode } => {
                    composed_game_mode = Some(*game_mode);
                }
                _ => {}
            }
        }
        Self {
            channel: messages[0].channel().cloned(),
            kind: MessageKind::MpMapResponse {
                map: composed_map,
                game_mode: composed_game_mode,
            },
        }
    }
    fn compose_mp_listrefs(messages: &[Self]) -> Self {
        let mut users = Vec::new();
        for message in messages {
            match message.kind() {
                MessageKind::MpListRefsLineUser { user } => {
                    users.push(user.clone());
                }
                _ => {}
            }
        }
        Self {
            channel: messages[0].channel().cloned(),
            kind: MessageKind::MpListRefsResponse { users },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MessageKind {
    WhereResponse {
        user: User,
        location: String,
    },
    WhereErrorOffline,
    WhereErrorUndetermined,

    StatsStatus {
        user: User,
        status: UserStatus,
    },
    StatsScore {
        score: UserScore,
        rank: UserRank,
    },
    StatsPlayCount {
        play_count: UserPlayCount,
        level: UserLevel,
    },
    StatsAccuracy {
        accuracy: UserAccuracy,
    },
    StatsResponse {
        user: User,
        status: UserStatus,
        score: UserScore,
        rank: UserRank,
        play_count: UserPlayCount,
        level: UserLevel,
        accuracy: UserAccuracy,
    },

    RollResponse {
        user: User,
        value: u32,
    },

    MpMakeResponse {
        id: MatchId,
        name: String,
    },
    MpMakeErrorLimited,
    MpMakeErrorNoName,

    MpCloseResponse,

    MpNameResponse {
        name: String,
    },

    MpPasswordChanged,
    MpPasswordRemoved,

    MpSizeResponse {
        size: SlotSize,
    },
    MpSizeErrorInvalid,

    MpSetResponse {
        size: Option<SlotSize>,
        team_mode: Option<TeamMode>,
        score_mode: Option<ScoreMode>,
    },
    MpSetError,

    MpModsResponse {
        mods: Mods,
        freemod: bool,
    },

    MpMapResponseMap {
        map: Map,
    },
    MpMapResponseGameMode {
        game_mode: GameMode,
    },
    MpMapResponse {
        map: Map,
        game_mode: Option<GameMode>,
    },
    MpMapErrorInvalid,

    MpHostResponse {
        user: User,
    },

    MpClearHostResponse,

    MpLockResponse,

    MpUnlockResponse,

    MpMoveResponse {
        user: User,
        slot: SlotIndex,
    },
    MpMoveError {
        slot: SlotIndex,
    },
    MpMoveErrorUserNotInMatch,

    MpTeamResponse {
        user: User,
        team: Team,
    },
    MpTeamError,

    MpSettingsRoom {
        name: String,
        id: MatchId,
    },
    MpSettingsMap {
        map: Map,
    },
    MpSettingsTeamScoreModes {
        team_mode: TeamMode,
        score_mode: ScoreMode,
    },
    MpSettingsMods {
        mods: Mods,
        freemod: bool,
    },
    MpSettingsSize {
        size: SlotSize,
    },
    MpSettingsSlot {
        slot: SlotIndex,
        status: SlotStatus,
        user: User,
        mods: Mods,
        host: bool,
        team: Option<Team>,
    },
    MpSettingsResponse {
        name: String,
        id: MatchId,
        map: Option<Map>,
        team_mode: TeamMode,
        score_mode: ScoreMode,
        mods: Mods,
        freemod: bool,
        size: SlotSize,
        slots: Slots,
    },

    MpStartResponse,
    MpStartTimer {
        time: Duration,
    },
    MpStartAlready,

    MpAbortTimerResponse,
    MpAbortTimerStart,
    MpAbortResponse,
    MpAbortErrorNotInProgress,

    MpInviteResponse {
        user: User,
    },
    MpInviteErrorAlreadyPresent,

    MpKickResponse {
        user: User,
    },

    MpBanResponse {
        user: User,
    },

    MpAddRefResponse {
        user: User,
    },

    MpRemoveRefResponse {
        user: User,
    },
    MpRemoveRefErrorNotReferee {
        user: User,
    },

    MpListRefsPrompt,
    MpListRefsLineUser {
        user: User,
    },
    MpListRefsResponse {
        users: Vec<User>,
    },

    MpRefereeErrorUserNotFound {
        user: User,
    },

    ErrorUserNotFound,
    ErrorUserUnspecified,
    MpErrorUserNotInMatch,
    MpErrorUserUnspecified,

    EventPlayerJoined {
        user: User,
        slot: SlotIndex,
        team: Option<Team>,
    },
    EventPlayerMoved {
        user: User,
        slot: SlotIndex,
    },
    EventPlayerTeam {
        user: User,
        team: Team,
    },
    EventPlayerLeft {
        user: User,
    },
    EventHostChanged {
        user: User,
    }, // host
    EventHostChangingMap, // host is changing map
    EventMapChanged {
        map: Map,
    },
    EventPlayersReady, // all players are ready
    EventMatchStarted, // match started
    EventMatchPlayerResult {
        user: User,
        score: UserScore,
        alive: bool,
    },
    EventMatchFinished, // match finished
    EventStartTimer {
        time: Duration,
    },
    EventStartTimerEndLuck,
    EventTimer {
        time: Duration,
    },
    EventTimerEnd,

    EventMaintenanceAlert,
    EventMaintenanceTimer {
        time: Duration,
    },
    EventMaintenanceRightBack,
}

impl MessageKind {
    pub fn is_stateful(&self) -> bool {
        match self {
            Self::StatsStatus { .. } => true,
            Self::StatsScore { .. } => true,
            Self::StatsPlayCount { .. } => true,
            Self::StatsAccuracy { .. } => true,

            Self::MpSettingsRoom { .. } => true,
            Self::MpSettingsMap { .. } => true,
            Self::MpSettingsTeamScoreModes { .. } => true,
            Self::MpSettingsMods { .. } => true,
            Self::MpSettingsSize { .. } => true,
            Self::MpSettingsSlot { .. } => true,

            Self::MpMapResponseMap { .. } => true,
            Self::MpMapResponseGameMode { .. } => true,

            Self::MpListRefsPrompt => true,
            Self::MpListRefsLineUser { .. } => true,

            _ => false,
        }
    }
    pub fn is_multiplayer_event(&self) -> bool {
        match self {
            Self::EventPlayerJoined { .. } => true,
            Self::EventPlayerMoved { .. } => true,
            Self::EventPlayerTeam { .. } => true,
            Self::EventPlayerLeft { .. } => true,
            Self::EventHostChanged { .. } => true,
            Self::EventHostChangingMap => true,
            Self::EventMapChanged { .. } => true,
            Self::EventPlayersReady => true,
            Self::EventMatchStarted => true,
            Self::EventMatchPlayerResult { .. } => true,

            // This is not a an actual event, but a response to `!mp abort`.
            // It is included here for convenience.
            Self::MpAbortResponse => true,

            Self::EventMatchFinished => true,
            Self::EventStartTimer { .. } => true,
            Self::EventStartTimerEndLuck => true,
            Self::EventTimer { .. } => true,
            Self::EventTimerEnd => true,
            _ => false,
        }
    }
    pub fn is_maintenance_event(&self) -> bool {
        match self {
            Self::EventMaintenanceAlert => true,
            Self::EventMaintenanceTimer { .. } => true,
            Self::EventMaintenanceRightBack => true,
            _ => false,
        }
    }
    pub fn maintenance(&self) -> Option<Duration> {
        match self {
            // The first two events do not contain timer information.
            Self::EventMaintenanceAlert => None,
            Self::EventMaintenanceTimer { time } => Some(time.clone()),
            Self::EventMaintenanceRightBack => Some(Duration::ZERO),
            _ => None,
        }
    }
}

/// This is a general matcher for a collection of regex patterns.
///
/// By design [`RegexSet`] is only for matching, not capturing. This matcher is
/// designed to locate the matched pattern via [`RegexSet`] and then capture
/// necessary information via [`Regex`]. This is quite useful for message
/// collections like [`MessageLine`].
#[derive(Debug)]
struct RegexSetMatcher {
    set: RegexSet,
    regexes: Vec<Regex>,
}

impl RegexSetMatcher {
    /// Creates a new matcher from a collection of patterns.
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
    /// Tests whether a string matches any of the patterns and returns the
    /// index of the matched pattern and captured information.
    fn matches<'a>(&'a self, s: &'a str) -> Option<(usize, regex::Captures)> {
        self.set
            .matches(s)
            .into_iter()
            .next()
            .map(|i| (i, self.regexes[i].captures(s).unwrap()))
    }
}

regexset_enum! {
    /// This is a collection of recognized BanchoBot message lines.
    ///
    /// It should be used with `RegexSetMatcher` to match against recognized
    /// messages.
    #[derive(Debug, Clone, PartialEq)]
    pub enum MessageLine {
        // standard commands
        // response to `!where`
        WhereResponse { user: User, location: String } => r"^(?P<user>.+) is in ?(?P<location>.*)$",
        WhereErrorOffline => r"^The user is currently not online.$",
        WhereErrorUndetermined => r"^The user's location could not be determined.$",
        // responses to `!stats`
        StatsStatus { user: User, status: UserStatus } [v, id: UserId, user: &str] => r"^Stats for \((?P<user>.+)\)\[https://osu.ppy.sh/u/(?P<id>\d+)\](?: is )?(?P<status>Afk|Idle|Playing|Lobby|Multiplayer|Multiplaying|Modding|Editing|Testing|Submitting|Watching)?:$" {
            Self::StatsStatus {
                user: User::new(id, user.to_string()),
                status,
            }
        },
        StatsScore { score: UserScore, rank: UserRank } [mstr] => r"^Score: *(?P<score>[\d,]+) \(#(?P<rank>\d+)\)$" {
            Self::StatsScore {
                score: score.replace(',', "").parse()?,
                rank: rank.parse()?,
            }
        },
        StatsPlayCount { play_count: UserPlayCount, level: UserLevel } => r"^Plays: *(?P<play_count>\d+) \(lv(?P<level>\d+)\)$",
        StatsAccuracy { accuracy: UserAccuracy } => r"^Accuracy: +(?P<accuracy>[\d.]+)%$",
        RollResponse { user: User, value: u32 } => r"^(?P<user>.+) rolls (?P<value>\d+) point\(s\)$",
        // create or close multiplayer games
        // responses to `!mp make` `!mp makeprivate` and `!mp close`
        MpMakeResponse { id: MatchId, name: String } => r"^Created the tournament match https://osu\.ppy\.sh/mp/(?P<id>\d+) (?P<name>.+)$",
        MpMakeErrorLimited => r"^You cannot create any more tournament matches. Please close any previous tournament matches you have open.$",
        MpMakeErrorNoName => r"^No name provided$",
        MpCloseResponse => r"^Closed the match$",
        // configure multiplayer game
        // response to `!mp name`
        MpNameResponse { name: String } => "^Room name updated to \"(?P<name>.+)\"$",
        // responses to `!mp password`
        MpPasswordChanged => r"^Changed the match password$",
        MpPasswordRemoved => r"^Removed the match password$", // when password is empty
        // response to `!mp size`
        MpSizeResponse { size: SlotSize } => r"^Changed match to size (?P<size>\d+)$",
        MpSizeErrorInvalid => r"^Invalid or no size provided$",
        // response to `!mp set`
        MpSetResponse { size: Option<SlotSize>, team_mode: Option<TeamMode>, score_mode: Option<ScoreMode> } => r"^Changed match settings to (?:(?P<size>\d+) slots)?(?:, *)?(?P<team_mode>HeadToHead|TagCoop|TeamVs|TagTeamVs)(?:, *)?(?P<score_mode>Score|Accuracy|Combo|ScoreV2)?$",
        MpSetError => r"^Invalid or no settings provided$",
        // response to `!mp mods`
        MpModsResponse { mods: Mods, freemod: bool } => r"^(Enabled (?P<mods>.+)|Disabled all mods), ((?P<freemod>enabled)|disabled) FreeMod$",
        // responses to `!mp map`
        MpMapResponseMap { map: Map } [id: MapId, name: &str] => r"^Changed beatmap to https://osu\.ppy\.sh/b/(?P<id>\d+) (?P<name>.+)$" {
            Self::MpMapResponseMap {
                map: Map { id, name: name.to_string() },
            }
        },
        MpMapResponseGameMode { game_mode: GameMode } => r"^Changed match mode to (?P<game_mode>Osu|Taiko|CatchTheBeat|OsuMania)$",
        MpMapErrorInvalid => r"^Invalid map ID provided$",
        // multiplayer slot-related commands
        // responses to `!mp host` and `!mp clearhost`
        MpHostResponse { user: User } => r"^Changed match host to (?P<user>.+)$",
        MpClearHostResponse => r"^Cleared match host$",
        // responses to `!mp lock` and `!mp unlock`
        MpLockResponse => r"^Locked the match$",
        MpUnlockResponse => r"^Unlocked the match$",
        // response to `!mp move`
        MpMoveResponse { user: User, slot: SlotIndex } [v, slot: usize] => r"^Moved (?P<user>.+) into slot (?P<slot>\d+)$" {
            Self::MpMoveResponse { user, slot: SlotIndex::try_from(slot - 1)? }
        },
        MpMoveError { slot: SlotIndex } [v, slot: usize] => r"^Failed to move player to slot (?P<slot>\d+)$" {
            Self::MpMoveError { slot: SlotIndex::try_from(slot - 1)? }
        },
        // same meaning as [`ErrorUserNotInMatch`], but for `!mp move`
        MpMoveErrorUserNotInMatch => r"^User is not in the match room$",
        // response to `!mp team`
        MpTeamResponse { user: User, team: Team } => r"^Moved (?P<user>.+) to team (?P<team>Red|Blue)$",
        MpTeamError => r"^Require a <username> <colour> combo$",
        // get multiplayer game settings
        // responses to `!mp settings`
        MpSettingsRoom { name: String, id: MatchId } => r"^Room name: (?P<name>.+), History: https://osu\.ppy\.sh/mp/(?P<id>\d+)$",
        MpSettingsMap { map: Map } [id: MapId, name: &str] => r"^Beatmap: https://osu\.ppy\.sh/b/(?P<id>\d+) (?P<name>.+)$" {
            Self::MpSettingsMap {
                map: Map { id, name: name.to_string() },
            }
        },
        MpSettingsTeamScoreModes { team_mode: TeamMode, score_mode: ScoreMode } => r"^Team mode: (?P<team_mode>.+), Win condition: (?P<score_mode>.+)$",
        MpSettingsMods { mods: Mods, freemod: bool } => r"^Active mods: (?P<mods>.+?)??(?P<freemod>(?:, )?Freemod)?$",
        MpSettingsSize { size: SlotSize } => r"^Players: (?P<size>\d+)$",
        MpSettingsSlot { slot: SlotIndex, status: SlotStatus, user: User, mods: Mods, host: bool, team: Option<Team> } [v, slot: usize, id: UserId, user: &str] => r"^Slot (?P<slot>\d+) +(?P<status>Not Ready|Ready|No Map) +https://osu\.ppy\.sh/u/(?P<id>\d+) (?P<user>.{16,}?) *(?:\[(?:(?P<host>Host)\s*/?\s*)?(?:Team\s*(?P<team>Red|Blue)\s*/?\s*)?(?P<mods>.+?)?\])?$" {
            Self::MpSettingsSlot {
                slot: SlotIndex::try_from(slot - 1)?,
                status,
                user: User::new(id, user.trim()),
                mods,
                host,
                team,
            }
        },
        // multiplayer match commands
        // responses to `!mp start`
        MpStartResponse => r"^Started the match$",
        MpStartTimer { time: Duration } [unit, major: u64, minor: u64] => r"^Queued the match to start in (?:(?P<major>\d+)(\s)*(?P<unit>second|minute)s?)?(?: and (?P<minor>\d+) seconds?)?$" {
            let unit = unit.map_or(0, |m| match m.as_str() { "second" => 1, "minute" => 60, _ => 0 });
            Self::MpStartTimer {
                time: Duration::from_secs(major * unit + minor),
            }
        },
        MpStartAlready => r"^The match has already been started$",
        // response to `!mp aborttimer`
        MpAbortTimerResponse => r"^Countdown aborted$",
        MpAbortTimerStart => r"^Aborted the match start timer$",
        // response to `!mp abort`
        MpAbortResponse => r"^Aborted the match$",
        MpAbortErrorNotInProgress => r"^The match is not in progress$",
        // multiplayer player management
        // response to `!mp invite`
        MpInviteResponse { user: User } => r"^Invited (?P<user>.+) to the room$",
        MpInviteErrorAlreadyPresent => r"^User is already in the room$",
        // responses to `!mp kick` and `!mp ban`
        MpKickResponse { user: User } => r"^Kicked (?P<user>.+) from the match$",
        MpBanResponse { user: User } => r"^Banned (?P<user>.+) from the match$",
        // responses to `!mp addref` and `!mp removeref`
        MpAddRefResponse { user: User } => r"^Added (?P<user>.+) to the match referees$",
        MpRemoveRefResponse { user: User } => r"^Removed (?P<user>.+) from the match referees$",
        MpRemoveRefErrorNotReferee { user: User } => r"^(?P<user>.+) is not a match referee$",
        MpListRefsPrompt => "^Match referees:$",
        // MpListRefsLineUser { user: User } => "(?!)",
        MpRefereeErrorUserNotFound { user: User } => r"^User not found: (?P<user>.+)$",
        // general errors
        // when parameter of a command is an invalid user, e.g. `!mp host` `!stats`
        ErrorUserNotFound => r"^User not found$",
        ErrorUserUnspecified => r"^No user specified$",
        // except `!mp host` which has **NO RESPONSE** when user is not present
        // when parameter of a command requires that the user is present in the match, e.g. `!mp team` `!mp ban`
        MpErrorUserNotInMatch => r"^User is not in the match!$",
        MpErrorUserUnspecified => r"^No username or #ID provided$",
        // multiplayer game events
        // slot-related events
        EventPlayerJoined { user: User, slot: SlotIndex, team: Option<Team> } [v, slot: usize] => r"^(?P<user>.+) joined in slot (?P<slot>\d+)(?: for team (?P<team>red|blue))?\.$" {
            Self::EventPlayerJoined {
                user,
                slot: SlotIndex::try_from(slot - 1)?,
                team,
            }
        },
        EventPlayerMoved { user: User, slot: SlotIndex } [v, slot: usize] => r"^(?P<user>.+) moved to slot (?P<slot>\d+)$" {
            Self::EventPlayerMoved { user, slot: SlotIndex::try_from(slot - 1)? }
        },
        EventPlayerTeam { user: User, team: Team } => r"^(?P<user>.+) changed to (?P<team>Blue|Red)$",
        EventPlayerLeft { user: User } => r"^(?P<user>.+) left the game\.$",
        // host-related events
        EventHostChanged { user: User } => r"^(?P<user>.+) became the host\.$", // host
        EventHostChangingMap => r"^Host is changing map\.\.\.$", // host is changing map
        EventMapChanged { map: Map } [id: MapId, name: &str] => r"^Beatmap changed to: (?P<name>.+) \(https://osu\.ppy\.sh/b/(?P<id>\d+)\)$" {
            Self::EventMapChanged {
                map: Map { id, name: name.to_string() },
            }
        },
        // match-related events
        EventPlayersReady => r"^All players are ready$", // all players are ready
        EventMatchStarted => r"^The match has started!$", // match started
        EventMatchPlayerResult { user: User, score: UserScore, alive: bool } => r"^(?P<user>.+) finished playing \(Score: (?P<score>\d+), (FAILED|(?P<alive>PASSED))\)\.$",
        EventMatchFinished => r"^The match has finished!$", // match finished
        // timer related events
        // Match starts in ((?P<s>\d+) seconds?|(?P<m>\d+) minutes?|(?P<s>\d+) minutes and 1 second)
        EventStartTimer { time: Duration } [unit, major: u64, minor: u64] => r"^Match starts in (?:(?P<major>\d+)(\s)*(?P<unit>second|minute)s?)?(?: and (?P<minor>\d+) seconds?)?$" {
            let unit = unit.map_or(0, |m| match m.as_str() { "second" => 1, "minute" => 60, _ => unreachable!() });
            Self::EventStartTimer {
                time: Duration::from_secs(major * unit + minor),
            }
        }, // `!mp start`
        EventStartTimerEndLuck => r"^Good luck, have fun!$",
        EventTimer { time: Duration } [unit, major: u64, minor: u64] => r"^Countdown ends in (?:(?P<major>\d+)(\s)*(?P<unit>second|minute)s?)?(?: and (?P<minor>\d+) seconds?)?$" {
            let unit = unit.map_or(0, |m| match m.as_str() { "second" => 1, "minute" => 60, _ => unreachable!() });
            Self::EventTimer {
                time: Duration::from_secs(major * unit + minor),
            }
        }, // `!mp timer`
        EventTimerEnd => r"^Countdown finished$",

        EventMaintenanceA => r"^Bancho will be restarting soon for routine maintenance. Please finish any multiplayer games to avoid interruption!$",
        EventMaintenanceB => r"^This maintenance does not affect leaderboards or score submission, only multiplayer and chat!$",
        EventMaintenanceC => r"^Maintenance usually take under one minute so you can continue playing almost immediately!$",
        EventMaintenanceD => r"^Multiplayer game creation has temporarily been disabled to allow for maintenance.$",
        EventMaintenanceTimerMinutes { time: Duration } [minutes: u64] => r"^Bancho will be restarting for maintenance in (?P<minutes>\d+) minutes?.$" {
            Self::EventMaintenanceTimerMinutes {
                time: Duration::from_secs(minutes * 60),
            }
        },
        EventMaintenanceTimerSeconds { time: Duration } [seconds: u64] => r"^(?P<seconds>10|\d)...$" {
            Self::EventMaintenanceTimerSeconds {
                time: Duration::from_secs(seconds),
            }
        },
        EventMaintenanceRightBack =>  r"^Bancho will be right back!$",
    }
}

#[cfg(test)]
mod tests {
    use crate::bancho::{
        irc,
        multiplayer::{ScoreMode, Slot, SlotStatus, Team, TeamMode},
        Map, Mods, User,
    };
    use std::io::BufRead;

    use super::*;

    fn assert_message_line_eq(iter: impl IntoIterator<Item = (&'static str, MessageLine)>) {
        let assembler = MessageAssembler::new();
        for (line, expected) in iter {
            let message = assembler
                .matches(line)
                .transpose()
                .unwrap_or_else(|| panic!("cannot match message: \"{}\"", line));
            match message {
                Ok(message) => {
                    assert_eq!(
                        message, expected,
                        "message \"{}\" parsed as {:?} is not equal to expected {:?}",
                        line, message, expected
                    );
                }
                Err(e) => {
                    panic!("cannot parse message \"{}\": {}", line, e);
                }
            }
        }
    }

    #[test]
    fn bot_message_line_tests() {
        assert_message_line_eq([
            ("Moved user1 to team Red", MessageLine::MpTeamResponse { user: User::name_only("user1"), team: Team::Red }),
            ("user2 changed to Red", MessageLine::EventPlayerTeam { user: User::name_only("user2"), team: Team::Red }),
            ("Active mods: DoubleTime, Nightcore, Freemod", MessageLine::MpSettingsMods { mods: Mods::DoubleTime | Mods::Nightcore, freemod: true }),
            ("Active mods: DoubleTime, Nightcore", MessageLine::MpSettingsMods { mods: Mods::DoubleTime | Mods::Nightcore, freemod: false }),
            ("Slot 12 Not Ready https://osu.ppy.sh/u/123 ABCDEF          [None]", MessageLine::MpSettingsSlot { slot: SlotIndex::from(11),                 status: SlotStatus::NotReady,                 user: User::new(123, "ABCDEF".to_string()),                 mods: Mods::empty(),                 host: false,                 team: None,             }),
            ("Slot 8  Not Ready https://osu.ppy.sh/u/123 ABCDEFGHIJK     [Host / Team Red ]", MessageLine::MpSettingsSlot {
                slot: SlotIndex::from(7),
                status: SlotStatus::NotReady,
                user: User::new(123, "ABCDEFGHIJK".to_string()),
                mods: Mods::empty(),
                host: true,
                team: Some(Team::Red),
            }),
            ("Slot 2  Not Ready https://osu.ppy.sh/u/123 ABCDEFGHI       [Team Red ]", MessageLine::MpSettingsSlot {
                slot: SlotIndex::from(1),
                status: SlotStatus::NotReady,
                user: User::new(123, "ABCDEFGHI".to_string()),
                mods: Mods::empty(),
                host: false,
                team: Some(Team::Red),
            }),
            ("Slot 12 Not Ready https://osu.ppy.sh/u/123 ABCDE1234       [Team Red  / Hidden, HardRock]", MessageLine::MpSettingsSlot {
                slot: SlotIndex::from(11),
                status: SlotStatus::NotReady,
                user: User::new(123, "ABCDE1234".to_string()),
                mods: Mods::Hidden | Mods::HardRock,
                host: false,
                team: Some(Team::Red),
            }),
            ("Slot 1  No Map    https://osu.ppy.sh/u/123 ABCDE           [Host / Team Blue / HardRock]", MessageLine::MpSettingsSlot {
                slot: SlotIndex::from(0),
                status: SlotStatus::NoMap,
                user: User::new(123, "ABCDE".to_string()),
                mods: Mods::HardRock,
                host: true,
                team: Some(Team::Blue),
            }),
            ("Countdown ends in 2 minutes and 2 seconds", MessageLine::EventTimer { time: Duration::from_secs(2 * 60 + 2) }),
            ("Countdown ends in 2 minutes and 1 second", MessageLine::EventTimer { time: Duration::from_secs(2 * 60 + 1) }),
            ("Countdown ends in 2 minutes", MessageLine::EventTimer { time: Duration::from_secs(2 * 60) }),
            ("Countdown ends in 1 minute and 39 seconds", MessageLine::EventTimer { time: Duration::from_secs(60 + 39) }),
            ("Countdown ends in 1 minute and 1 second", MessageLine::EventTimer { time: Duration::from_secs(60 + 1) }),
            ("Countdown ends in 1 minute", MessageLine::EventTimer { time: Duration::from_secs(60) }),
            ("Countdown ends in 5 seconds", MessageLine::EventTimer { time: Duration::from_secs(5) }),
            ("Countdown ends in 4 seconds", MessageLine::EventTimer { time: Duration::from_secs(4) }),
            ("Countdown ends in 3 seconds", MessageLine::EventTimer { time: Duration::from_secs(3) }),
            ("Countdown ends in 2 seconds", MessageLine::EventTimer { time: Duration::from_secs(2) }),
            ("Countdown ends in 1 second", MessageLine::EventTimer { time: Duration::from_secs(1) }),
            ("Countdown finished", MessageLine::EventTimerEnd),
            ("Countdown ends in ", MessageLine::EventTimer { time: Duration::from_secs(0) }),
        ]);
    }
    #[test]
    #[ignore]
    fn bot_message_real_input_test() {
        let mut assembler = MessageAssembler::new();
        let dir =
            std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("./resources/tests/banchobot");
        for entry in dir.read_dir().unwrap() {
            if let Ok(entry) = entry {
                let file = std::fs::File::open(entry.path()).unwrap();
                let reader = std::io::BufReader::new(file);
                for line in reader.lines() {
                    let line: String = line.unwrap();
                    let message = irc::Message::parse(line.as_bytes()).unwrap();
                    if let irc::Command::PRIVMSG { target, body } = &message.command {
                        let message = bancho::Message {
                            sender: User::name_only(message.prefix().unwrap().name()),
                            channel: target.parse().ok(),
                            content: body.to_string(),
                        };
                        match assembler.convert(message.clone()) {
                            None => {
                                println!("cannot convert message: {:?}", message);
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }
    #[test]
    fn bot_message_mp_settings() {
        let mut assembler = MessageAssembler::new();
        let messages = &[
            Message {
                channel: Some(Channel::Multiplayer(123)),
                kind: MessageKind::MpSettingsRoom {
                    name: "test".to_string(),
                    id: 0xdeadbeefu64,
                },
            },
            Message {
                channel: Some(Channel::Multiplayer(123)),
                kind: MessageKind::MpSettingsMap {
                    map: Map::new(0xbeefdeadu64, "map"),
                },
            },
            Message {
                channel: Some(Channel::Multiplayer(123)),
                kind: MessageKind::MpSettingsTeamScoreModes {
                    team_mode: TeamMode::TagTeam,
                    score_mode: ScoreMode::ScoreV2,
                },
            },
            Message {
                channel: Some(Channel::Multiplayer(123)),
                kind: MessageKind::MpSettingsMods {
                    mods: Mods::HardRock | Mods::DoubleTime,
                    freemod: false,
                },
            },
            Message {
                channel: Some(Channel::Multiplayer(123)),
                kind: MessageKind::MpSettingsSize {
                    size: SlotSize::from(3),
                },
            },
            Message {
                channel: Some(Channel::Multiplayer(123)),
                kind: MessageKind::MpSettingsSlot {
                    slot: SlotIndex::from(0),
                    status: SlotStatus::NotReady,
                    user: User::new(1, "user1"),
                    mods: Mods::empty(),
                    host: false,
                    team: Some(Team::Red),
                },
            },
            Message {
                channel: Some(Channel::Multiplayer(123)),
                kind: MessageKind::MpSettingsSlot {
                    slot: SlotIndex::from(1),
                    status: SlotStatus::NoMap,
                    user: User::new(4, "user2"),
                    mods: Mods::empty(),
                    host: false,
                    team: Some(Team::Red),
                },
            },
            Message {
                channel: Some(Channel::Multiplayer(123)),
                kind: MessageKind::MpSettingsSlot {
                    slot: SlotIndex::from(15),
                    status: SlotStatus::NoMap,
                    user: User::new(7, "user3"),
                    mods: Mods::empty(),
                    host: true,
                    team: Some(Team::Blue),
                },
            },
        ];
        for message in messages {
            match assembler.track(message) {
                Some(response) => {
                    assert_eq!(response.channel, Some(Channel::Multiplayer(123)));
                    match response.kind {
                        MessageKind::MpSettingsResponse {
                            name,
                            id,
                            map,
                            team_mode,
                            score_mode,
                            mods,
                            freemod,
                            size,
                            slots,
                        } => {
                            assert_eq!(name, "test");
                            assert_eq!(id, 0xdeadbeefu64);
                            assert_eq!(map, Some(Map::new(0xbeefdeadu64, "map")));
                            assert_eq!(mods, Mods::HardRock | Mods::DoubleTime);
                            assert_eq!(freemod, false);
                            assert_eq!(team_mode, TeamMode::TagTeam);
                            assert_eq!(score_mode, ScoreMode::ScoreV2);
                            assert_eq!(size, SlotSize::from(3));
                            assert_eq!(
                                slots.as_ref()[0],
                                Some(Slot {
                                    user: User::new(1, "user1"),
                                    status: SlotStatus::NotReady,
                                    mods: Mods::empty(),
                                    host: false,
                                    team: Some(Team::Red),
                                }),
                            );
                            assert_eq!(
                                slots.as_ref()[1],
                                Some(Slot {
                                    user: User::new(4, "user2"),
                                    status: SlotStatus::NoMap,
                                    mods: Mods::empty(),
                                    host: false,
                                    team: Some(Team::Red),
                                }),
                            );
                            assert_eq!(
                                slots.as_ref()[15],
                                Some(Slot {
                                    user: User::new(7, "user3"),
                                    status: SlotStatus::NoMap,
                                    mods: Mods::empty(),
                                    host: true,
                                    team: Some(Team::Blue),
                                })
                            );
                        }
                        _ => panic!("Unexpected response kind: {:?}", response),
                    }
                    return;
                }
                _ => {}
            }
        }
        panic!("Expected response not found");
    }
}
