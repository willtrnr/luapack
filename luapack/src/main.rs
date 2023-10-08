use clap::Parser;
use luapack_core::packer::SearchPaths;
use std::{fs::File, io::BufWriter, path::PathBuf};

#[derive(Parser, Clone, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(short = 'I', long)]
    include: Vec<PathBuf>,
    entry: PathBuf,
}

pub fn main() -> eyre::Result<()> {
    env_logger::init();

    let args = Args::parse();

    let packer =
        luapack_core::packer::Packer::new().add_searcher(SearchPaths::from_iter(args.include));

    let _ = packer.pack(args.entry.as_path())?;

    let _output = BufWriter::<Box<dyn std::io::Write>>::new({
        let file = args
            .output
            .unwrap_or_else(|| args.entry.with_extension("bundle.lua"));
        if file.as_os_str() == "-" {
            Box::new(std::io::stdout())
        } else {
            Box::new(File::create(file)?)
        }
    });

    Ok(())
}
