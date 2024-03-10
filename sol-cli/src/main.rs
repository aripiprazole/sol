use clap::*;
use itertools::Itertools;
use sol_driver::RootDb;
use sol_eyre::eyre;

use crate::build::Manifest;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand, Debug, Clone)]
pub enum Command {
    Js {
        #[clap(short, long)]
        watch: bool,

        #[clap(short, long)]
        package: String,
    },
    TypeCheck,
}

pub mod build;

fn main() -> sol_eyre::Result<()> {
    let cli = Cli::parse();
    let db = RootDb::default();

    match cli.command {
        Command::Js { package, .. } => {
            let mut manifest = Manifest::load_in_folder(&db, std::env::current_dir()?)?;
            manifest.register_packages()?;

            let source_map = manifest.resolve_all_files()?;
            if manifest.diagnostics.is_empty() {
                let current_source = source_map
                    .get_in_db(manifest.db, package)
                    .ok_or_else(|| eyre!("could not locate the package"))?;

                todo!()
            }

            sol_ariadne::AriadneReport::default()
                .expand(manifest.diagnostics.into_iter().collect_vec())
                .eprint()?;
        }
        Command::TypeCheck => todo!(),
    }
    Ok(())
}
