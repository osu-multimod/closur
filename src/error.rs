use std::fmt;
pub use std::error::Error as StdError;

#[derive(Debug)]
pub enum ConversionError {
    InvalidUser,
    InvalidChannel,
    InvalidUserStatus,
    InvalidModName,

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
            ConversionError::InvalidUser => write!(f, "Invalid user"),
            ConversionError::InvalidChannel => write!(f, "Invalid channel"),
            ConversionError::InvalidUserStatus => write!(f, "Invalid user status"),
            ConversionError::InvalidModName => write!(f, "Invalid mod name"),
            ConversionError::InvalidTeam => write!(f, "Invalid team"),
            ConversionError::InvalidTeamMode => write!(f, "Invalid team mode"),
            ConversionError::InvalidSlotStatus => write!(f, "Invalid slot status"),
            ConversionError::InvalidGameMode => write!(f, "Invalid game mode"),
            ConversionError::InvalidScoreMode => write!(f, "Invalid score mode"),
        }
    }
}
