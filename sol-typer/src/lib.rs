#![feature(trait_alias)]
#![feature(trait_upcasting)]
#![feature(fn_traits)]
#![feature(stmt_expr_attributes)]
#![feature(box_patterns)]

use std::collections::VecDeque;

use salsa::DbWithJar;
use sol_hir::{
    solver::Definition,
    source::{declaration::Declaration, HirSource},
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
pub struct Jar(create_type_table);

/// The database that Typer uses internally. This is a trait so that we can
/// mock it during testing.
pub trait TyperDb: ThirDb + HirDb + DbWithJar<Jar> {}

impl<DB: ThirDb> TyperDb for DB where DB: ?Sized + HirDb + salsa::DbWithJar<Jar> {}

pub mod options;
pub mod utils;

#[salsa::tracked]
pub fn create_type_table(db: &dyn TyperDb, global_env: GlobalEnv, source: HirSource) -> TypeTable {
    let mut table = TypeTable::new();
    let ctx = Context::default_with_env(db, global_env);

    for item in source.contents(db).iter() {
        use sol_hir::source::top_level::TopLevel::*;
        match item {
            Error(_) | Inductive(_) | Using(_) | Command(_) => todo!("handle: error"),
            BindingGroup(group) => {
                let actual_type = match group.signature(db).type_rep(db) {
                    Some(value) => {
                        let term = db.thir_check(ctx, *value.expr, Type::U);
                        db.thir_eval(Env::new(db, VecDeque::new()), term)
                    }
                    None => Type::Flexible(MetaVar::new(None), vec![]),
                };

                match group.clauses(db).len() {
                    0 => todo!("handle: error"),
                    1 => {
                        let clause = group.clauses(db).into_iter().next().unwrap();
                        let term = db.thir_check(ctx, clause.value(db), actual_type.clone());
                        table.insert(clause.name(db), (term, actual_type));
                    }
                    _ => todo!("handle: different error"),
                }
            }
        }
    }

    table
}
