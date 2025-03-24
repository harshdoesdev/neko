use clap::Parser;

use neko_cli::{
    cli_parser::{Cli, Commands},
    commands,
};

pub fn run() -> neko_cli::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { script_path } => Ok(commands::run::handle(script_path)?),
    }
}
