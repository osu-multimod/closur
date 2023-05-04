pub mod bancho;
mod error;

use std::result::Result as StdResult;

pub(crate) type BoxError = Box<dyn error::StdError + Send + Sync>;
pub(crate) type Result<T> = StdResult<T, BoxError>;

pub type Error = BoxError;
