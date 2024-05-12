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
    shared::{ConstructorKind, Context, Env},
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
    let (location, value) = value.force(db);

    location
        .map(|location| {
            let value = thir_quote_impl(db, Some(location.clone()), lvl, value.clone());

            Term::Location(location.clone(), value.into())
        })
        .unwrap_or_else(|| thir_quote_impl(db, None, lvl, value))
}

/// Implementation of [`thir_quote`] with the correct parameters and forced value, with unboxed
/// rigid structures.
fn thir_quote_impl(
    db: &dyn ThirLoweringDb,
    location: Option<sol_hir::source::Location>,
    lvl: Level,
    value: Value,
) -> Term {
    use sol_thir::value::Value::*;

    match value {
        U => Term::U,
        Constructor(constructor) => Term::Constructor(constructor),
        Flexible(meta, spine) => spine.into_iter().rev().fold(Term::Meta(meta), |acc, next| {
            Term::App(acc.into(), thir_quote(db, lvl, next).into())
        }),
        Rigid(x, spine) => spine
            .into_iter()
            .rev()
            .fold(Term::Var(lvl.as_idx(db, x).unwrap(), None), |acc, next| {
                Term::App(acc.into(), thir_quote(db, lvl, next).into())
            }),
        Pi(pi) => {
            // Pi (quote lvl pi.type_rep) (quote (lvl + 1) (pi.closure $$ (Var lvl pi.name)))
            let name = create_reference_of(db, pi.name, location.clone());
            let domain = thir_quote(db, lvl, *pi.type_rep.clone());
            let codomain = thir_quote(db, lvl.increase(db), Value::new_var(lvl, name));

            Term::Pi(
                pi.name.into(),
                pi.implicitness,
                domain.into(),
                codomain.into(),
            )
        }
        Lam(name, implicitness, closure) => {
            // Lam (quote (lvl + 1) (closure $$ (Var lvl name)))
            let argument = Value::new_var(lvl, create_reference_of(db, name, location.clone()));
            let closure = thir_quote(db, lvl.increase(db), closure.apply(db, argument));

            Term::Lam(name, implicitness, closure.into())
        }
        Location(location, term) => Term::Location(location, thir_quote(db, lvl, *term).into()),
    }
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

/// Create a reference to a definition if it's a reference or use stub value to location
/// if it's a call site.
fn create_reference_of(
    db: &dyn ThirLoweringDb,
    definition: Definition,
    maybe_location: Option<Location>,
) -> Option<Reference> {
    maybe_location.map(|location| Reference::new(db, definition, location))
}
