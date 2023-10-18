use super::message::MessageKind;
use super::{CommandContext, TailableCommand, TrackVerdict};
use crate::bancho::multiplayer::Slots;
use crate::bancho::{
    bot,
    multiplayer::{GameMode, ScoreMode, SlotIndex, SlotSize, Team, TeamMode},
    Channel, Map, Mods, User,
};
use crate::bancho::{UserAccuracy, UserLevel, UserPlayCount, UserRank, UserScore, UserStatus};

use std::time::Duration;

/// Basic command `!where <user>`, which returns the geolocation of `<user>`.
///
/// This command can be sent both in all channels and directly to `BanchoBot`.
#[derive(Debug, Clone, PartialEq)]
pub struct Where(pub User);
impl bot::Command for Where {
    type Body = WhereResponse;
    type Error = WhereError;
    fn command_string(&self) -> String {
        format!("!where {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::WhereResponse { user, location } if user.eq(&self.0) => {
                    context.push(message.clone());
                    TrackVerdict::Body(WhereResponse { user, location })
                }
                MessageKind::WhereErrorOffline => {
                    context.push(message.clone());
                    TrackVerdict::Err(WhereError::Offline)
                }
                MessageKind::WhereErrorUndetermined => {
                    context.push(message.clone());
                    TrackVerdict::Err(WhereError::Undetermined)
                }
                MessageKind::ErrorUserUnspecified => {
                    context.push(message.clone());
                    TrackVerdict::Err(WhereError::Unspecified)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhereResponse {
    user: User,
    location: String,
}
impl WhereResponse {
    pub fn user(&self) -> &User {
        &self.user
    }
    pub fn location(&self) -> &str {
        &self.location
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum WhereError {
    /// This error indicates the user is not specified. (empty usernames)
    Unspecified,
    /// This error indicates the user is not online currently or not found.
    Offline,
    /// This error indicates the user's geolocation is not available.
    Undetermined,
}
// impl TailableCommand for Where {}

/// Basic command `!stats <user>`, which returns game statistics of `<user>`.
///
/// This command can be sent both in all channels and directly to `BanchoBot`.
#[derive(Debug, Clone, PartialEq)]
pub struct Stats(pub User);
impl bot::Command for Stats {
    type Body = StatsResponse;
    type Error = StatsError;
    fn command_string(&self) -> String {
        format!("!stats {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::StatsResponse {
                    user,
                    status,
                    score,
                    rank,
                    play_count,
                    level,
                    accuracy,
                } if user.eq(&self.0) => {
                    context.push(message.clone());
                    TrackVerdict::Body(StatsResponse {
                        user,
                        status,
                        score,
                        rank,
                        play_count,
                        level,
                        accuracy,
                    })
                }
                MessageKind::ErrorUserNotFound => {
                    context.push(message.clone());
                    TrackVerdict::Err(StatsError::NotFound)
                }
                MessageKind::ErrorUserUnspecified => {
                    context.push(message.clone());
                    TrackVerdict::Err(StatsError::Unspecified)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StatsResponse {
    user: User,
    status: UserStatus,
    score: UserScore,
    rank: UserRank,
    play_count: UserPlayCount,
    level: UserLevel,
    accuracy: UserAccuracy,
}
impl StatsResponse {
    pub fn user(&self) -> &User {
        &self.user
    }
    pub fn status(&self) -> UserStatus {
        self.status
    }
    pub fn score(&self) -> UserScore {
        self.score
    }
    pub fn rank(&self) -> UserRank {
        self.rank
    }
    pub fn play_count(&self) -> UserPlayCount {
        self.play_count
    }
    pub fn level(&self) -> UserLevel {
        self.level
    }
    pub fn accuracy(&self) -> UserAccuracy {
        self.accuracy
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum StatsError {
    /// This error indicates the user is not specified. (empty usernames)
    Unspecified,
    /// This error indicates the user is not found.
    NotFound,
}

/// Basic command `!roll [N]`, which returns a random number in a range. If the
/// range is not specified, the default range is [0, 100].
///
/// This command is tailable and can be sent both in all channels and directly
/// to `BanchoBot`.
#[derive(Debug, Clone, PartialEq)]
pub struct Roll(pub Option<u32>);
impl bot::Command for Roll {
    type Body = u32;
    type Error = ();
    fn command_string(&self) -> String {
        match self.0 {
            None => "!roll".to_string(),
            Some(n) => format!("!roll {}", n),
        }
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::RollResponse { user, value } if user.eq(context.issuer()) => {
                    context.push(message.clone());
                    TrackVerdict::Body(value)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for Roll {}

/// Multiplayer command `!mp make <room title>`, which creates a multiplayer
/// room with specific title.
///
/// This command can be sent both in all channels and directly to `BanchoBot`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpMake(pub String);
impl bot::Command for MpMake {
    type Body = MpMakeResponse;
    type Error = MpMakeError;
    fn sendable_in_channel(&self, _channel: &Channel) -> bool {
        false
    }
    fn command_string(&self) -> String {
        format!("!mp make {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        let kind = message.kind().clone();
        match kind {
            MessageKind::MpMakeResponse { id, name } => {
                context.push(message.clone());
                TrackVerdict::Body(MpMakeResponse { id, name })
            }
            MessageKind::MpMakeErrorLimited => {
                context.push(message.clone());
                TrackVerdict::Err(MpMakeError::LimitExceeded)
            }
            MessageKind::MpMakeErrorNoName => {
                context.push(message.clone());
                TrackVerdict::Err(MpMakeError::NoName)
            }
            _ => TrackVerdict::Skip,
        }
    }
}

/// Multiplayer command `!mp makeprivate <room title>`, which creates a
/// multiplayer room with specific title and *private* match history.
///
/// This command can be sent both in all channels and directly to `BanchoBot`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpMakePrivate(pub String);
impl bot::Command for MpMakePrivate {
    type Body = MpMakeResponse;
    type Error = MpMakeError;
    fn sendable_in_channel(&self, _channel: &Channel) -> bool {
        false
    }
    fn command_string(&self) -> String {
        format!("!mp makeprivate {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        let kind = message.kind().clone();
        match kind {
            MessageKind::MpMakeResponse { id, name } => {
                context.push(message.clone());
                TrackVerdict::Body(MpMakeResponse { id, name })
            }
            MessageKind::MpMakeErrorLimited => {
                context.push(message.clone());
                TrackVerdict::Err(MpMakeError::LimitExceeded)
            }
            MessageKind::MpMakeErrorNoName => {
                context.push(message.clone());
                TrackVerdict::Err(MpMakeError::NoName)
            }
            _ => TrackVerdict::Skip,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpMakeResponse {
    id: u64,
    name: String,
}
impl MpMakeResponse {
    pub fn id(&self) -> u64 {
        self.id
    }
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpMakeError {
    /// This error indicates the user has already reached the limit of creating
    /// multiplayer rooms.
    LimitExceeded,
    /// This error indicates the room name is empty.
    NoName,
}

/// Multiplayer command `!mp close`, which closes a multiplayer room.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpClose;
impl bot::Command for MpClose {
    type Body = ();
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        "!mp close".to_string()
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpCloseResponse => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpClose {}

/// Multiplayer command `!mp name <room title>`, which modifies the title of a
/// multiplayer room.
///
/// This command can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpName(pub String);
impl bot::Command for MpName {
    type Body = MpNameResponse;
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        format!("!mp name {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpNameResponse { name } => {
                    context.push(message.clone());
                    TrackVerdict::Body(MpNameResponse { name })
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpNameResponse {
    name: String,
}
impl MpNameResponse {
    pub fn name(&self) -> &str {
        &self.name
    }
}

/// Multiplayer command `!mp password <room title>`, which sets / clears the
/// password of a multiplayer room.
///
/// This command can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpPassword(pub Option<String>);
impl bot::Command for MpPassword {
    type Body = ();
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        match &self.0 {
            None => "!mp password".to_string(),
            Some(password) => format!("!mp password {}", password),
        }
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpPasswordChanged | MessageKind::MpPasswordRemoved => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}

/// Multiplayer command `!mp size <slot number>`, which limits the number of
/// slots in a multiplayer room.
///
/// This command is tailable can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpSize(pub SlotSize);
impl bot::Command for MpSize {
    type Body = MpSizeResponse;
    type Error = MpSizeError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        format!("!mp size {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpSizeResponse { size } => {
                    context.push(message.clone());
                    TrackVerdict::Body(MpSizeResponse { size })
                }
                MessageKind::MpSizeErrorInvalid => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpSizeError::Invalid)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpSize {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpSizeResponse {
    size: SlotSize,
}
impl MpSizeResponse {
    pub fn size(&self) -> SlotSize {
        self.size
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpSizeError {
    /// This error indicates an invalid size.
    Invalid,
}

/// Multiplayer command `!mp set <team mode> [score mode] [slot number]`, which
/// sets modes of a multiplayer room.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpSet(
    pub Option<TeamMode>,
    pub Option<ScoreMode>,
    pub Option<SlotSize>,
);
impl MpSet {
    fn segments(&self) -> Vec<Option<String>> {
        [
            self.0.map(|t| (t as usize).to_string()),
            self.1.map(|s| (s as usize).to_string()),
            self.2.map(|n| n.to_string()),
        ]
        .to_vec()
    }
}
impl bot::Command for MpSet {
    type Body = MpSetResponse;
    type Error = MpSetError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        let _placeholder = "_".to_string();

        self.segments()
            .iter()
            .rev()
            .skip_while(|o| o.is_none())
            .map(|o| o.as_ref().map(|s| s.as_str()).unwrap_or("_"))
            .fold("!mp set".to_string(), |acc, item| {
                format!("{} {}", acc, item)
            })
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpSetResponse {
                    size,
                    team_mode,
                    score_mode,
                } if self.0 == team_mode && self.1 == score_mode && self.2 == size => {
                    context.push(message.clone());
                    TrackVerdict::Body(MpSetResponse {
                        team_mode,
                        score_mode,
                        size,
                    })
                }
                MessageKind::MpSetError => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpSetError::Invalid)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpSet {
    fn tailed_command_string(&self, tail: &str) -> String {
        let mut s = self
            .segments()
            .iter()
            .map(|o| o.as_ref().map(|s| s.as_str()).unwrap_or("_"))
            .fold("!mp set".to_string(), |acc, item| {
                format!("{} {}", acc, item)
            });
        s.push_str(&tail);
        s
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpSetResponse {
    team_mode: Option<TeamMode>,
    score_mode: Option<ScoreMode>,
    size: Option<SlotSize>,
}
impl MpSetResponse {
    pub fn team_mode(&self) -> Option<TeamMode> {
        self.team_mode
    }
    pub fn score_mode(&self) -> Option<ScoreMode> {
        self.score_mode
    }
    pub fn size(&self) -> Option<SlotSize> {
        self.size
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpSetError {
    /// This error indicates the settings provided is invalid.
    Invalid,
}

/// Multiplayer command `!mp mods <mod> [mod ...]`, which sets the game mods of
/// a multiplayer room.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpMods {
    pub mods: Mods,
    pub freemod: bool,
}
impl bot::Command for MpMods {
    type Body = MpModsResponse;
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        if self.freemod {
            format!("!mp mods {} Freemod", self.mods.bits())
        } else {
            format!("!mp mods {}", self.mods.bits())
        }
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpModsResponse { mods, freemod } => {
                    context.push(message.clone());
                    TrackVerdict::Body(MpModsResponse { mods, freemod })
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpMods {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpModsResponse {
    mods: Mods,
    freemod: bool,
}
impl MpModsResponse {
    pub fn mods(&self) -> Mods {
        self.mods
    }
    pub fn freemod(&self) -> bool {
        self.freemod
    }
}

/// Multiplayer command `!mp map <map id> [mode]`, which sets the map and mode
/// of a multiplayer room.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpMap(pub Map, pub Option<GameMode>);
impl bot::Command for MpMap {
    type Body = MpMapResponse;
    type Error = MpMapError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        match self.1 {
            None => format!("!mp map {}", self.0.id()),
            Some(mode) => {
                format!("!mp map {} {}", self.0.id(), mode as usize)
            }
        }
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpMapResponse { map, game_mode } => {
                    context.push(message.clone());
                    TrackVerdict::Body(MpMapResponse { map, game_mode })
                }
                MessageKind::MpMapErrorInvalid => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpMapError::Invalid)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpMap {
    fn tailed_command_string(&self, tail: &str) -> String {
        let mut s = match self.1 {
            None => format!("!mp map {} _", self.0.id()),
            Some(mode) => {
                format!("!mp map {} {}", self.0.id(), mode as usize)
            }
        };
        s.push_str(&tail);
        s
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MpMapResponse {
    map: Map,
    game_mode: Option<GameMode>,
}
impl MpMapResponse {
    pub fn map(&self) -> &Map {
        &self.map
    }
    pub fn game_mode(&self) -> Option<GameMode> {
        self.game_mode
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpMapError {
    /// This error indicates an invalid map ID.
    Invalid,
}

/// Multiplayer command `!mp host <user>`, which assigns a new host.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpHost(pub User);
impl bot::Command for MpHost {
    type Body = MpHostResponse;
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        format!("!mp host {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpHostResponse { user } if user.eq(&self.0) => {
                    context.push(message.clone());
                    TrackVerdict::Body(MpHostResponse(user))
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpHost {}

#[derive(Debug, Clone, PartialEq)]
pub struct MpHostResponse(User);
impl MpHostResponse {
    pub fn user(&self) -> &User {
        &self.0
    }
}

/// Multiplayer command `!mp clearhost`, which clears the host.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpClearHost;
impl bot::Command for MpClearHost {
    type Body = ();
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        "!mp clearhost".to_string()
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpClearHostResponse => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpClearHost {}

/// Multiplayer command `!mp lock`, which locks the state of a multiplayer room.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpLock;
impl bot::Command for MpLock {
    type Body = ();
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        "!mp lock".to_string()
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpLockResponse => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpLock {}

/// Multiplayer command `!mp unlock`, which unlocks the state of a multiplayer
/// room.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpUnlock;
impl bot::Command for MpUnlock {
    type Body = ();
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        "!mp unlock".to_string()
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpUnlockResponse => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpUnlock {}

/// Multiplayer command `!mp move <user> <slot index>`, which moves a user to a
/// specific slot.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpMove(pub User, pub SlotIndex);
impl bot::Command for MpMove {
    type Body = ();
    type Error = MpMoveError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        format!("!mp move {} {}", self.0, self.1.as_usize() + 1)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpMoveResponse { user, slot } if user == self.0 && slot == self.1 => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                MessageKind::MpMoveError { slot } if slot == self.1 => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpMoveError::OperationFailure)
                }
                MessageKind::MpMoveErrorUserNotInMatch => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpMoveError::NotPresent)
                }
                MessageKind::ErrorUserNotFound => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpMoveError::NotFound)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpMove {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpMoveError {
    /// This error indicates the user is invalid.
    NotFound,
    /// This error indicates the user is found but without presence in the room.
    NotPresent,
    /// This error indicates a move operation failure. This usually implies that
    /// the target slot is occupied.
    OperationFailure,
}

/// Multiplayer command `!mp team <user> <team index>`, which assigns a user to
/// the specific team.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpTeam(pub User, pub Team);
impl bot::Command for MpTeam {
    type Body = ();
    type Error = MpTeamError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        format!("!mp team {} {}", self.0, self.1 as usize)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpTeamResponse { user, team } if user == self.0 && team == self.1 => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                MessageKind::MpErrorUserNotInMatch => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpTeamError::NotPresent)
                }
                MessageKind::ErrorUserNotFound => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpTeamError::NotFound)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpTeam {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpTeamError {
    /// This error indicates the user is invalid.
    NotFound,
    /// This error indicates the user is found but without presence in the room.
    NotPresent,
}

/// Multiplayer command `!mp settings`, which returns modes, status, players of
/// a multiplayer room.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpSettings;
impl bot::Command for MpSettings {
    type Body = MpSettingsResponse;
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        "!mp settings".to_string()
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
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
                    context.push(message.clone());
                    TrackVerdict::Body(MpSettingsResponse {
                        name,
                        id,
                        map,
                        team_mode,
                        score_mode,
                        mods,
                        freemod,
                        size,
                        slots,
                    })
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpSettings {}

#[derive(Debug, Clone, PartialEq)]
pub struct MpSettingsResponse {
    name: String,
    id: u64,
    map: Option<Map>,
    team_mode: TeamMode,
    score_mode: ScoreMode,
    mods: Mods,
    freemod: bool,
    size: SlotSize,
    slots: Slots,
}
impl MpSettingsResponse {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn id(&self) -> u64 {
        self.id
    }
    pub fn map(&self) -> Option<&Map> {
        self.map.as_ref()
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
        self.size.as_usize()
    }
    pub fn slots(&self) -> &Slots {
        &self.slots
    }
}

/// Multiplayer command `!mp start [duration]`, which starts the game (with
/// optional timer) in a multiplayer room.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpStart(pub Option<Duration>);
impl bot::Command for MpStart {
    type Body = ();
    type Error = MpStartError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        match self.0 {
            None => "!mp start".to_string(),
            Some(delay) => format!("!mp start {}", delay.as_secs()),
        }
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpStartResponse => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                MessageKind::MpStartTimer { time } if self.0.map_or(false, |d| d == time) => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                MessageKind::MpStartAlready => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpStartError::AlreadyStarted)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpStart {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpStartError {
    /// This error indicates the game has been already started.
    AlreadyStarted,
}

/// Multiplayer command `!mp timer [duration]`, which starts a timer in a
/// multiplayer room.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpTimer(pub Option<Duration>);
impl bot::Command for MpTimer {
    type Body = ();
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        match self.0 {
            None => "!mp timer".to_string(),
            Some(delay) => format!("!mp timer {}", delay.as_secs()),
        }
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::EventTimer { time }
                    if time == self.0.unwrap_or(Duration::from_secs(30)) =>
                {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpTimer {}

/// Multiplayer command `!mp aborttimer`, which aborts a timer initiated by
/// `!mp timer <duration>` or a start timer initiated by `!mp start <duration>`.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpAbortTimer;
impl bot::Command for MpAbortTimer {
    type Body = ();
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        "!mp aborttimer".to_string()
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpAbortTimerResponse => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpAbortTimer {}

/// Multiplayer command `!mp abort`, which aborts a started match or a start
/// timer initiated by `!mp start <duration>`.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MpAbort;
impl bot::Command for MpAbort {
    type Body = ();
    type Error = MpAbortError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        "!mp abort".to_string()
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpAbortResponse => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                MessageKind::MpAbortErrorNotInProgress => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpAbortError::NotInProgress)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpAbort {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpAbortError {
    /// This error indicates the game is not in progress.
    NotInProgress,
}

/// Multiplayer command `!mp invite <user>`, which invites a specific player to
/// a multiplayer room.
///
/// This command can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpInvite(pub User);
impl bot::Command for MpInvite {
    type Body = ();
    type Error = MpInviteError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        format!("!mp invite {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpInviteResponse { user } if user == self.0 => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                MessageKind::MpInviteErrorAlreadyPresent => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpInviteError::AlreadyPresent)
                }
                MessageKind::ErrorUserNotFound => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpInviteError::NotFound)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpInvite {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpInviteError {
    /// This error indicates the user is invalid.
    NotFound,
    /// This error indicates the user is found but already present in the room.
    AlreadyPresent,
}

/// Multiplayer command `!mp kick <user>`, which kicks a specific player from a
/// multiplayer room.
///
/// This command can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpKick(pub User);
impl bot::Command for MpKick {
    type Body = ();
    type Error = MpKickError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        format!("!mp kick {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpKickResponse { user } if user == self.0 => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                MessageKind::MpErrorUserNotInMatch => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpKickError::NotPresent)
                }
                MessageKind::ErrorUserNotFound => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpKickError::NotFound)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpKick {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpKickError {
    /// This error indicates the user is invalid.
    NotFound,
    /// This error indicates the user is found but without presence in the room.
    NotPresent,
}

/// Multiplayer command `!mp ban <user>`, which bans a specific player from a
/// multiplayer room.
///
/// This command can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpBan(pub User);
impl bot::Command for MpBan {
    type Body = ();
    type Error = MpBanError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        format!("!mp ban {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpBanResponse { user } if user == self.0 => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                MessageKind::MpErrorUserNotInMatch => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpBanError::NotPresent)
                }
                MessageKind::ErrorUserNotFound => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpBanError::NotFound)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpBan {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpBanError {
    /// This error indicates the user is invalid.
    NotFound,
    /// This error indicates the user is found but without presence in the room.
    NotPresent,
}

/// Multiplayer command `!mp addref <user>`, which assigns a specific user as a
/// referee.
///
/// This command can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpAddRef(pub User);
impl bot::Command for MpAddRef {
    type Body = ();
    type Error = MpAddRefError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        format!("!mp addref {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpAddRefResponse { user } if user == self.0 => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                MessageKind::MpRefereeErrorUserNotFound { user } if user == self.0 => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpAddRefError::NotFound)
                }
                MessageKind::MpErrorUserUnspecified => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpAddRefError::Unspecified)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpAddRefError {
    /// This error indicates the user is not specified. (empty usernames)
    Unspecified,
    /// This error indicates the user is invalid.
    NotFound,
}

/// Multiplayer command `!mp removeref <user>`, which assigns a specific user as
/// a referee.
///
/// This command can be sent only in multiplayer channels.
#[derive(Debug, Clone, PartialEq)]
pub struct MpRemoveRef(pub User);
impl bot::Command for MpRemoveRef {
    type Body = ();
    type Error = MpRemoveRefError;
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        format!("!mp removeref {}", self.0)
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpRemoveRefResponse { user } if user == self.0 => {
                    context.push(message.clone());
                    TrackVerdict::Body(())
                }
                MessageKind::MpRemoveRefErrorNotReferee { user } if user == self.0 => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpRemoveRefError::NotReferee)
                }
                MessageKind::MpRefereeErrorUserNotFound { user } if user == self.0 => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpRemoveRefError::NotFound)
                }
                MessageKind::MpErrorUserUnspecified => {
                    context.push(message.clone());
                    TrackVerdict::Err(MpRemoveRefError::Unspecified)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MpRemoveRefError {
    /// This error indicates the user is not specified. (empty usernames)
    Unspecified,
    /// This error indicates the user is invalid.
    NotFound,
    /// This error indicates the user is not a referee.
    NotReferee,
}

/// Multiplayer command `!mp listrefs`, which gives a list of referees in a
/// multiplayer room.
///
/// This command is tailable and can be sent only in multiplayer channels.
#[derive(Debug, Clone)]
pub struct MpListRefs;
impl bot::Command for MpListRefs {
    type Body = Vec<User>;
    type Error = ();
    fn sendable_to_user(&self, _user: &User) -> bool {
        false
    }
    fn sendable_in_channel(&self, channel: &Channel) -> bool {
        channel.is_multiplayer()
    }
    fn command_string(&self) -> String {
        "!mp listrefs".to_string()
    }
    fn tracks_message(
        &self,
        context: &mut CommandContext,
        message: bot::Message,
    ) -> TrackVerdict<Self::Body, Self::Error> {
        if message.channel() == context.target().channel() {
            let kind = message.kind().clone();
            match kind {
                MessageKind::MpListRefsResponse { users } => {
                    context.push(message.clone());
                    TrackVerdict::Body(users)
                }
                _ => TrackVerdict::Skip,
            }
        } else {
            TrackVerdict::Skip
        }
    }
}
impl TailableCommand for MpListRefs {}
