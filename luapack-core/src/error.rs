pub type Result<T, E = Error> = core::result::Result<T, E>;

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Parse(Box<full_moon::Error>),
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

impl From<full_moon::Error> for Error {
    fn from(value: full_moon::Error) -> Self {
        Self::Parse(Box::new(value))
    }
}
