use std::path::PathBuf;

pub fn handle(script_path: PathBuf) -> neko_cli::Result<()> {
    let script = std::fs::read_to_string(script_path)?;
    let _value = neko_lang::utils::run_source(&script)?;
    Ok(())
}
