//! Type terms to type infer and check the sol programs, they are the simpliest
//! normal-form terms.

#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(trait_upcasting)]

use salsa::DbWithJar;
use sol_diagnostic::{Diagnostic, DiagnosticDb, ErrorId, ErrorKind};
use sol_hir::{
    debruijin::{Index, Level},
    lowering::HirLowering,
    package::HasManifest,
    primitives::PrimitiveProvider,
    solver::{Definition, Reference},
    source::{
        expr::{Expr, Meta},
        literal::Literal,
        pattern::Pattern,
        HirElement, HirError, Location,
    },
    HirDb,
};
use sol_syntax::ParseDb;

#[salsa::jar(db = ThirLoweringDb)]
pub struct Jar(thir_lower, thir_eval, thir_quote, thir_infer, thir_quote);

/// The database that stores all the information about the source code. It is
/// implemented using the [`salsa`] crate, and it's used by the [`sol-driver`] crate.
pub trait ThirLoweringDb: ThirDb + DbWithJar<Jar> {}

impl<T> ThirLoweringDb for T where T: HirDb + DbWithJar<Jar> {}

/// The quoting function to convert the value back to the term.
#[salsa::tracked]
fn thir_quote(&self, db: &dyn ThirLoweringDb, value: Value) -> Term {
    todo!()
}

/// The infer function to infer the type of the term.
#[salsa::tracked]
fn thir_infer(&self, db: &dyn ThirLoweringDb, env: Env, term: Term) -> (Term, Type) {
    todo!()
}

/// The check function to check the type of the term.
#[salsa::tracked]
fn thir_check(&self, db: &dyn ThirLoweringDb, env: Env, term: Term, type_repr: Type) -> Term {
    todo!()
}

#[salsa::tracked]
fn thir_lower(&self, db: &dyn ThirLoweringDb, expr: Expr) -> Term {
    todo!()
}

#[salsa::tracked]
fn thir_eval(&self, db: &dyn ThirLoweringDb, env: Env, term: Term) -> Value {
    todo!()
}
