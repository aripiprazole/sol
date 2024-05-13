//! Type terms to type infer and check the sol programs, they are the simpliest
//! normal-form terms.

#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(trait_upcasting)]
#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]

use salsa::DbWithJar;
use sol_hir::{
    solver::{Definition, Reference},
    source::{
        expr::{AbsExpr, Expr},
        type_rep::TypeRep,
        HirElement, Location,
    },
};
use sol_thir::{
    debruijin::Level,
    shared::{Context, Env, Implicitness, MetaVar},
    source::Term,
    value::{Closure, Pi, Type, Value},
    ThirDb,
};

extern crate salsa_2022 as salsa;

mod elaboration;

#[salsa::jar(db = ThirLoweringDb)]
pub struct Jar(
    thir_eval,
    thir_infer,
    thir_check,
    thir_quote,
    elaboration::unify_catch,
);

/// The database that stores all the information about the source code. It is
/// implemented using the [`salsa`] crate, and it's used by the [`sol-driver`] crate.
pub trait ThirLoweringDb: ThirDb + DbWithJar<Jar> {}

impl<T> ThirLoweringDb for T where T: ThirDb + DbWithJar<Jar> {}

#[salsa::tracked]
pub fn thir_eval(db: &dyn ThirLoweringDb, env: Env, term: Term) -> Value {
    match term {
        Term::U => Value::U,
        Term::Var(idx, _) => env.get(db, idx),
        Term::Lam(name, implicitness, value) => {
            Value::Lam(name, implicitness, Closure { env, expr: *value })
        }
        Term::App(callee, argument) => db
            .thir_eval(env, *callee)
            .apply_to_spine(db.thir_eval(env, *argument)),
        Term::Pi(name, implicitness, domain, codomain) => {
            let domain = db.thir_eval(env, *domain);

            Value::Pi(Pi {
                name,
                implicitness,
                type_rep: Box::new(domain),
                closure: Closure {
                    env,
                    expr: *codomain,
                },
            })
        }
        Term::Constructor(constructor) => Value::Constructor(constructor),
        Term::Ann(value, _) => db.thir_eval(env, *value),
        Term::InsertedMeta(meta) => meta.get().unwrap_or_else(|| Value::Flexible(meta, vec![])),
        Term::Location(location, term) => Value::located(location, db.thir_eval(env, *term)),
        Term::Sorry(_, _) => panic!("sorry :("),
    }
}

/// The quoting function to convert the value back to the term.
#[salsa::tracked]
pub fn thir_quote(db: &dyn ThirLoweringDb, lvl: Level, value: Value) -> Term {
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
            Flexible(meta, spine) => spine
                .into_iter()
                .rev()
                .fold(Term::InsertedMeta(meta), |acc, next| {
                    Term::App(acc.into(), db.thir_quote(lvl, next).into())
                }),
            Rigid(x, spine) => spine
                .into_iter()
                .rev()
                .fold(Term::Var(lvl.as_idx(db, x).unwrap(), None), |acc, next| {
                    Term::App(acc.into(), db.thir_quote(lvl, next).into())
                }),
            Pi(pi) => {
                // Pi (quote lvl pi.type_rep) (quote (lvl + 1) (pi.closure $$ (Var lvl pi.name)))
                let name = create_reference_of(db, pi.name, location.clone());
                let domain = db.thir_quote(lvl, *pi.type_rep.clone());
                let codomain = db.thir_quote(lvl.increase(db), Value::new_var(lvl, name));

                Term::Pi(pi.name, pi.implicitness, domain.into(), codomain.into())
            }
            Lam(name, implicitness, closure) => {
                // Lam (quote (lvl + 1) (closure $$ (Var lvl name)))
                let argument =
                    Value::new_var(lvl, create_reference_of(db, name.into(), location.clone()));
                let closure = db.thir_quote(lvl.increase(db), closure.apply(db, argument));

                Term::Lam(name, implicitness, closure.into())
            }
            Location(location, term) => Term::Location(location, db.thir_quote(lvl, *term).into()),
        }
    }

    let (location, value) = value.force(db);

    location
        .map(|location| {
            let value = thir_quote_impl(db, Some(location.clone()), lvl, value.clone());

            Term::Location(location.clone(), value.into())
        })
        .unwrap_or_else(|| thir_quote_impl(db, None, lvl, value))
}

/// The infer function to infer the type of the term.
#[salsa::tracked]
pub fn thir_infer(db: &dyn ThirLoweringDb, ctx: Context, expr: Expr) -> (Term, Type) {
    use Expr::*;

    ctx.location(db).update(expr.location(db));

    match expr {
        Empty => todo!(),
        Error(_) => todo!(),
        Path(_) => todo!(),
        Literal(_) => todo!(),
        Call(_) => todo!(),
        Ann(_) => todo!(),
        Abs(_) => todo!(),
        Match(_) => todo!(),
        Upgrade(box TypeRep::App(_)) => todo!(),
        Upgrade(box TypeRep::Hole) => todo!(),
        Upgrade(box TypeRep::SelfType) => todo!(),
        Upgrade(box TypeRep::Unit) => todo!(),
        Upgrade(box TypeRep::Type) => todo!(),
        Upgrade(box TypeRep::Pi(_)) => todo!(),
        Upgrade(box TypeRep::Path(_, _)) => todo!(),
        Upgrade(box TypeRep::Error(_)) => todo!(),
        Upgrade(box TypeRep::Downgrade(box expr)) => db.thir_infer(ctx, expr),
    }
}

/// The check function to check the type of the term.
#[salsa::tracked]
pub fn thir_check(db: &dyn ThirLoweringDb, ctx: Context, expr: Expr, type_repr: Type) -> Term {
    ctx.location(db).update(expr.location(db));

    fn abs_pi(db: &dyn ThirLoweringDb, ctx: Context, abs: AbsExpr, pi: Pi) -> Term {
        todo!()
    }

    fn implicit_fun_eta(db: &dyn ThirLoweringDb, value: Expr, pi: Pi) -> Term {
        todo!()
    }

    fn type_hole() -> Term {
        Term::InsertedMeta(MetaVar::new(None))
    }

    fn term_equality(db: &dyn ThirLoweringDb, ctx: Context, expr: Expr, expected: Type) -> Term {
        let (term, type_repr) = db.thir_infer(ctx, expr);
        let (term, inferred_type) = elaboration::insert(ctx, term, type_repr);
        elaboration::unify_catch(db, ctx, expected, inferred_type);
        term
    }

    #[rustfmt::skip]
    match (expr, type_repr) {
        (Expr::Abs(lam), Type::Pi(pi)) => abs_pi(db, ctx, lam, pi),
        (value, Type::Pi(pi @ Pi { implicitness: Implicitness::Implicit, .. })) => implicit_fun_eta(db, value, pi),
        (Expr::Upgrade(box TypeRep::Hole), _) => type_hole(),
        (value, expected) => term_equality(db, ctx, value, expected)
    }
}

/// Create a reference to a definition if it's a reference or use stub value to location
/// if it's a call site.
fn create_reference_of(
    db: &dyn ThirLoweringDb,
    maybe_definition: Option<Definition>,
    maybe_location: Option<Location>,
) -> Option<Reference> {
    maybe_definition
        .zip(maybe_location)
        .map(|(definition, location)| Reference::new(db, definition, location))
}
