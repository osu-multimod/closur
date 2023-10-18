pub use std::error::Error as StdError;
use std::fmt;

#[derive(Debug)]
pub enum ConversionError {
    ChannelTypeMismatch,

    InvalidMatchInnerId,
    InvalidUser,
    InvalidChannel,
    InvalidUserStatus,
    InvalidModName,

    InvalidSlotIndex,
    InvalidTeam,
    InvalidTeamMode,
    InvalidSlotStatus,
    InvalidGameMode,
    InvalidScoreMode,
}

impl StdError for ConversionError {}
impl fmt::Display for ConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConversionError::ChannelTypeMismatch => write!(f, "Channel type mismatch"),

            ConversionError::InvalidMatchInnerId => write!(f, "Invalid match inner id"),
            ConversionError::InvalidUser => write!(f, "Invalid user"),
            ConversionError::InvalidChannel => write!(f, "Invalid channel"),
            ConversionError::InvalidUserStatus => write!(f, "Invalid user status"),
            ConversionError::InvalidModName => write!(f, "Invalid mod name"),

            ConversionError::InvalidSlotIndex => write!(f, "Invalid slot index"),
            ConversionError::InvalidTeam => write!(f, "Invalid team"),
            ConversionError::InvalidTeamMode => write!(f, "Invalid team mode"),
            ConversionError::InvalidSlotStatus => write!(f, "Invalid slot status"),
            ConversionError::InvalidGameMode => write!(f, "Invalid game mode"),
            ConversionError::InvalidScoreMode => write!(f, "Invalid score mode"),
        }
    }
}
