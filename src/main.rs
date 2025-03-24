pub fn main() {
    if let Err(e) = neko_cli::runner::run() {
        eprintln!("Error: {:#?}", e);
        std::process::exit(1);
    }
}
