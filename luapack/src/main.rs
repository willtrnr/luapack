use clap::Parser;
use luapack_core::{Packer, SearchPaths};
use std::{
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
};

#[derive(Parser, Clone, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(short, long, env = "LUA_PATH")]
    path: Vec<String>,
    entry: PathBuf,
}

pub fn main() -> eyre::Result<()> {
    let args = Args::parse();

    env_logger::builder()
        .filter_level(args.verbose.log_level_filter())
        .init();

    let mut packer = Packer::new().add_searcher(SearchPaths::from_iter(
        args.path
            .iter()
            .flat_map(|p| p.split(';'))
            .filter(|p| !p.is_empty()),
    ));

    let result = packer.pack_to_string(args.entry.as_path())?;

    let file = args
        .output
        .unwrap_or_else(|| args.entry.with_extension("bundle.lua"));
    if file.as_os_str() == "-" {
        std::io::stdout().write_all(result.as_bytes())?;
    } else {
        BufWriter::new(File::create(file)?).write_all(result.as_bytes())?;
    }

    Ok(())
}
