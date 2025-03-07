pub type Result<T, E = Error> = core::result::Result<T, E>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{0:?}")]
    Parse(Vec<full_moon::Error>),
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

impl From<full_moon::Error> for Error {
    #[inline]
    fn from(value: full_moon::Error) -> Self {
        Self::Parse(vec![value])
    }
}

impl From<Vec<full_moon::Error>> for Error {
    #[inline]
    fn from(value: Vec<full_moon::Error>) -> Self {
        Self::Parse(value)
    }
}
