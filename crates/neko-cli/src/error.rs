#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("I/O Error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Runtime Error: {0}")]
    RuntimeError(#[from] neko_lang::error::Error),
}
