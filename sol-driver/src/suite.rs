use std::{collections::HashMap, io::Write, sync::Arc};

use itertools::Itertools;
use owo_colors::{
    colors::{Green, Red, White},
    OwoColorize,
};
use salsa_2022::DebugWithDb;
use similar::{ChangeTag, TextDiff};
use sol_diagnostic::Diagnostic;
use sol_hir::{fmt::HirFormatter, source::HirElement};
use sol_typer::TypeTable;

use crate::RootDb;

#[macro_export]
macro_rules! make_test {
    ($category: expr, $name:ident, $run:expr) => {
        #[test]
        fn $name() {
            let source_code = std::fs::read_to_string(concat!(
                "suite/",
                stringify!($category),
                "/",
                stringify!($name),
                ".sol"
            ))
            .expect("can't find the source code");
            let expect = std::fs::read_to_string(concat!(
                "suite/",
                stringify!($category),
                "/",
                stringify!($name),
                ".expect"
            ))
            .unwrap_or_default();
            $crate::suite::run_test_suite(
                stringify!($category),
                stringify!($name),
                &source_code,
                &expect,
                $run,
            );
        }
    };
    ($category: expr, $name:ident, $file:expr, $run:expr) => {
        #[test]
        fn $name() {
            let source_code = std::fs::read_to_string(concat!(
                "suite/",
                stringify!($category),
                "/",
                $file,
                ".sol"
            ))
            .unwrap();
            let expect = std::fs::read_to_string(concat!(
                "suite/",
                stringify!($category),
                "/",
                $file,
                ".expect"
            ))
            .unwrap_or_default();
            $crate::suite::run_test_suite(
                stringify!($category),
                $file,
                &source_code,
                &expect,
                $run,
            );
        }
    };
}

#[macro_export]
macro_rules! make_test_suite {
  (tests $e:ident {$($name:ident)*} run $run:expr) => {
    $($crate::make_test!($e, $name, $run);)*
  };
}

type SourceCode = String;
type Expect<'a> = &'a mut dyn Write;

/// Runs a test suite, with the given `name` and `f`.
pub fn run_test_suite(
    category: &str,
    file: &str,
    source_code: &str,
    expect: &str,
    f: impl FnOnce(RootDb, SourceCode, Expect) -> sol_eyre::Result<()>,
) {
    bupropion::install(bupropion::BupropionHandlerOpts::new).unwrap();

    env_logger::builder()
        .is_test(true)
        .filter_level(log::LevelFilter::Debug)
        .filter_module("salsa_2022", log::LevelFilter::Off)
        .try_init()
        .unwrap();

    let db = RootDb::default();
    let mut output = Vec::new();
    if let Err(err) = f(db, source_code.into(), &mut output) {
        panic!("{}", err);
    }

    let output = strip_ansi_escapes::strip_str(String::from_utf8_lossy(&output));
    if expect.is_empty() {
        std::fs::write(format!("suite/{category}/{file}.expect"), output).unwrap();
        return;
    }
    if output != expect {
        let diff = TextDiff::from_lines(expect, &output);
        for change in diff.iter_all_changes() {
            let sign = match change.tag() {
                ChangeTag::Delete => format!("- {change}").fg::<Red>().to_string(),
                ChangeTag::Insert => format!("+ {change}").fg::<Green>().to_string(),
                ChangeTag::Equal => format!("  {change}").fg::<White>().to_string(),
            };
            print!("{}", sign);
        }
        println!();
        panic!("The expected output does not match the actual output.");
    }
}

/// Groups the errors by file.
pub fn push_fancy_errors(output: Expect, outputs: &[Vec<Diagnostic>]) -> sol_eyre::Result<()> {
    writeln!(output, "Errors:")?;
    for error in outputs.iter().flatten() {
        writeln!(output, "{error:?}")?;
    }
    Ok(())
}

/// Prints a debug report of the given `type_table`.
pub fn debug_type_table(
    expect: Expect,
    db: &RootDb,
    type_table: TypeTable,
) -> sol_eyre::Result<()> {
    for (name, (term, type_rep)) in type_table {}

    Ok(())
}
