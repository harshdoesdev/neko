extern crate self as neko_cli;

pub mod error;

pub type Result<T> = std::result::Result<T, error::Error>;

pub mod cli_parser;
pub mod commands;
pub mod runner;
