#![feature(trait_alias)]
#![feature(trait_upcasting)]
#![feature(fn_traits)]
#![feature(stmt_expr_attributes)]
#![feature(box_patterns)]

use std::{
    collections::VecDeque,
    panic::{catch_unwind, AssertUnwindSafe},
    sync::Arc,
};

use salsa::DbWithJar;
use sol_diagnostic::{report_error, TextSource, UnwrapOrReport};
use sol_hir::{
    solver::Definition,
    source::{declaration::Declaration, top_level::TopLevel, HirSource},
    HirDb,
};
use sol_thir::{
    shared::{Context, Env, GlobalEnv, MetaVar},
    source::Term,
    value::Type,
    ThirDb,
};

extern crate salsa_2022 as salsa;

pub type TypeTable = im::HashMap<Definition, (Term, Type)>;

#[salsa::jar(db = TyperDb)]
pub struct Jar(infer_type_table);

/// The database that Typer uses internally. This is a trait so that we can
/// mock it during testing.
pub trait TyperDb: ThirDb + HirDb + DbWithJar<Jar> {}

impl<DB: ThirDb> TyperDb for DB where DB: ?Sized + HirDb + salsa::DbWithJar<Jar> {}

pub mod options;
pub mod utils;

fn check_top_level(
    db: &dyn TyperDb,
    ctx: Context,
    item: TopLevel,
    table: &mut TypeTable,
) -> std::thread::Result<()> {
    use sol_hir::source::top_level::TopLevel::*;

    catch_unwind(AssertUnwindSafe(|| {
        match item {
            Error(_) | Inductive(_) | Using(_) | Command(_) => todo!("handle: error"),
            BindingGroup(group) => {
                let actual_type = match group.signature(db).type_rep(db) {
                    Some(value) => {
                        let term = db
                            .thir_check(ctx, *value.expr, Type::U)
                            .unwrap_or_report(db);

                        db.thir_eval(Env::new(db, VecDeque::new()), term)
                            .unwrap_or_report(db)
                    }
                    None => Type::Flexible(MetaVar::new(None), vec![]),
                };

                match group.clauses(db).len() {
                    0 => todo!("handle: error"),
                    1 => {
                        let clause = group.clauses(db).into_iter().next().unwrap();
                        let term = db
                            .thir_check(ctx, clause.value(db), actual_type.clone())
                            .unwrap_or_report(db);
                        table.insert(clause.name(db), (term, actual_type));
                    }
                    _ => todo!("handle: different error"),
                }
            }
        };
    }))
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("type checker panicked, please report an error: {message}")]
#[diagnostic(code(sol::typer::type_checker_panic))]
pub struct TyperPanicError {
    pub message: String,

    #[source_code]
    pub source_code: TextSource,
}

#[salsa::tracked]
pub fn infer_type_table(db: &dyn TyperDb, global_env: GlobalEnv, source: HirSource) -> TypeTable {
    let mut table = TypeTable::new();
    let ctx = Context::default_with_env(db, global_env);
    let text_source = TextSource::new(
        source.source(db).file_path(db).to_string_lossy(),
        Arc::new(source.source(db).source_text(db).to_string()),
    );

    for item in source.contents(db).iter() {
        if let Err(panic_error) = check_top_level(db, ctx, item.clone(), &mut table) {
            report_error(db, TyperPanicError {
                source_code: text_source.clone(),
                message: panic_error
                    .downcast::<String>()
                    .map(|e| e.to_string())
                    .unwrap_or_else(|_| "unknown panic error".to_string()),
            })
        }
    }

    table
}
