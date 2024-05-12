//! Type terms to type infer and check the sol programs, they are the simpliest
//! normal-form terms.

#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(trait_upcasting)]

use salsa::DbWithJar;
use sol_hir::{
    primitives::PrimitiveProvider,
    solver::{Definition, Reference},
    source::{
        expr::Expr, literal::Literal, pattern::Pattern, type_rep::TypeRep, HirElement, HirError,
        Location,
    },
    HirDb,
};
use sol_thir::{
    debruijin::Level,
    shared::{Context, Env},
    source::Term,
    value::{Type, Value},
    ThirDb,
};

extern crate salsa_2022 as salsa;

#[salsa::jar(db = ThirLoweringDb)]
pub struct Jar(thir_eval, thir_infer, thir_check, thir_quote);

/// The database that stores all the information about the source code. It is
/// implemented using the [`salsa`] crate, and it's used by the [`sol-driver`] crate.
pub trait ThirLoweringDb: ThirDb + DbWithJar<Jar> {}

impl<T> ThirLoweringDb for T where T: ThirDb + DbWithJar<Jar> {}

#[salsa::tracked]
fn thir_eval(db: &dyn ThirLoweringDb, env: Env, term: Term) -> Value {
    todo!()
}

/// The quoting function to convert the value back to the term.
#[salsa::tracked]
fn thir_quote(db: &dyn ThirLoweringDb, lvl: Level, value: Value) -> Term {
    todo!()
}

/// The infer function to infer the type of the term.
#[salsa::tracked]
fn thir_infer(db: &dyn ThirLoweringDb, ctx: Context, expr: Expr) -> (Term, Type) {
    ctx.location(db).update(expr.location(db));

    todo!()
}

/// The check function to check the type of the term.
#[salsa::tracked]
fn thir_check(db: &dyn ThirLoweringDb, ctx: Context, expr: Expr, type_repr: Type) -> Term {
    todo!()
}
