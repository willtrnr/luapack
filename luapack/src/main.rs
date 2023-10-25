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
    #[arg(long, value_parser = parse_preload)]
    preload: Vec<(String, PathBuf)>,
    #[arg(long)]
    exclude: Vec<String>,
    entry: PathBuf,
}

fn parse_preload(value: &str) -> eyre::Result<(String, PathBuf)> {
    if let Some((k, v)) = value.split_once('=') {
        Ok((k.to_string(), PathBuf::from(v)))
    } else {
        let p = PathBuf::from(value);
        Ok((
            p.with_extension("")
                .to_string_lossy()
                .replace(['/', '\\'], "."),
            p,
        ))
    }
}

pub fn main() -> eyre::Result<()> {
    let args = Args::parse();

    env_logger::builder()
        .filter_level(args.verbose.log_level_filter())
        .init();

    let mut packer = Packer::new()
        .add_searcher(SearchPaths::from_iter(
            args.path
                .iter()
                .flat_map(|p| p.split(';'))
                .filter(|p| !p.is_empty()),
        ))
        .add_excludes(args.exclude)
        .add_preloads(args.preload)?;

    let result = packer.pack_to_string(args.entry.as_path())?;

    if let Some(p) = args.output {
        if p.as_os_str() != "-" {
            BufWriter::new(File::create(p)?).write_all(result.as_bytes())?;
            return Ok(());
        }
    }

    std::io::stdout().write_all(result.as_bytes())?;

    Ok(())
}
