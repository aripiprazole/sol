// use sol_diagnostic::Diagnostics;
// use sol_driver::{make_test_suite, suite::*};
// use sol_eval::{hir_eval, stack::Stack};
// use sol_hir_lowering::hir_lower;
// use sol_syntax::parse;
// use sol_tt::Env;
// use sol_typer::table::infer_type_table;
// use sol_vfs::SourceFile;
// use utils::create_package;

pub mod utils;

// make_test_suite! {
//   tests ("type inference") {
//     typeclasses "typeclasses"
//     generalization "generalization"
//   }
//   run |db, source, output| {
//     let file = SourceFile::new(&db, "repl".into(), "Repl".into(), source);

//     let src = parse(&db, file);
//     let local = create_package(&db, src, "local");
//     let hir = hir_lower(&db, local, src);
//     let table = infer_type_table(&db, hir);

//     debug_type_table(output, &db, table)?;

//     // Concats the diagnostics of the various passes and prints them.
//     //
//     // Using the aridane crate, we can print the diagnostics in a nice way,
//     // with colors and all.
//     push_ariadne_errors(output, &[
//       sol_syntax::parse::accumulated::<Diagnostics>(&db, file),
//       hir_lower::accumulated::<Diagnostics>(&db, local, src),
//       infer_type_table::accumulated::<Diagnostics>(&db, hir),
//     ])?;

//     Ok(())
//   }
// }

// make_test_suite! {
//   tests ("evaluation") {
//     eval "eval"
//   }
//   run |db, source, output| {
//     let file = SourceFile::new(&db, "repl".into(), "Repl".into(), source);

//     let src = parse(&db, file);
//     let local = create_package(&db, src, "local");
//     let hir = hir_lower(&db, local, src);
//     let value = hir_eval(&db, Stack::default(), Env::default(), hir);

//     writeln!(output, "{:#?}", value)?;

//     Ok(())
//   }
// }
