#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Parse Error: {0}")]
    ParseError(#[from] neko_lang::parser::ParseError),
    #[error("Runtime Error: {0}")]
    RuntimeError(#[from] neko_lang::interpreter::RuntimeError),
}
