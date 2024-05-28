//! Type terms to type infer and check the sol programs, they are the simpliest
//! normal-form terms.

#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(trait_upcasting)]
#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]

use salsa::DbWithJar;
use sol_diagnostic::IntoSolDiagnostic;
use sol_hir::{
    solver::{Definition, DefinitionId, DefinitionKind::Variable, Reference},
    source::{
        expr::{Expr, LamExpr},
        pattern::Pattern,
        HirElement, HirPath, Location,
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

pub mod check;
pub mod elaboration;
pub mod infer;

#[salsa::jar(db = ThirLoweringDb)]
pub struct Jar(
    thir_eval,
    infer::thir_infer,
    check::thir_check,
    thir_quote,
    elaboration::unify_catch,
);

/// The database that stores all the information about the source code. It is
/// implemented using the [`salsa`] crate, and it's used by the [`sol-driver`] crate.
pub trait ThirLoweringDb: ThirDb + DbWithJar<Jar> {}

impl<T> ThirLoweringDb for T where T: ThirDb + DbWithJar<Jar> {}

#[salsa::tracked]
pub fn thir_eval(db: &dyn ThirLoweringDb, env: Env, term: Term) -> sol_diagnostic::Result<Value> {
    Ok(match term {
        Term::U => Value::U,
        Term::Var(idx, _) => env.get(db, idx),
        Term::Lam(name, implicitness, value) => {
            Value::Lam(name, implicitness, Closure { env, expr: *value })
        }
        Term::App(callee, argument) => db
            .thir_eval(env, *callee)?
            .apply_to_spine(db.thir_eval(env, *argument)?),
        Term::Pi(name, implicitness, domain, codomain) => Value::Pi(Pi {
            name,
            implicitness,
            type_repr: Box::new(db.thir_eval(env, *domain)?),
            closure: Closure {
                env,
                expr: *codomain,
            },
        }),
        Term::Constructor(constructor) => Value::Constructor(constructor),
        Term::Ann(value, _) => db.thir_eval(env, *value)?,
        Term::InsertedMeta(meta) => meta.get().unwrap_or_else(|| Value::Flexible(meta, vec![])),
        Term::Location(location, term) => Value::located(location, db.thir_eval(env, *term)?),
        Term::Sorry(_, _) => panic!("sorry :("),
    })
}

/// The quoting function to convert the value back to the term.
#[salsa::tracked]
pub fn thir_quote(
    db: &dyn ThirLoweringDb,
    lvl: Level,
    value: Value,
) -> sol_diagnostic::Result<Term> {
    /// Implementation of [`thir_quote`] with the correct parameters and forced value, with unboxed
    /// rigid structures.
    fn thir_quote_impl(
        db: &dyn ThirLoweringDb,
        location: Option<sol_hir::source::Location>,
        lvl: Level,
        value: Value,
    ) -> sol_eyre::Result<Term> {
        use sol_thir::value::Value::*;

        Ok(match value {
            U => Term::U,
            Constructor(constructor) => Term::Constructor(constructor),
            Flexible(meta, spine) => {
                let default = Ok(Term::InsertedMeta(meta));
                return spine.into_iter().rev().fold(default, |acc, next| {
                    let next = db.thir_quote(lvl, next)?;
                    Ok(Term::App(acc?.into(), next.into()))
                });
            }
            Rigid(x, spine) => {
                let default = Ok(Term::Var(lvl.as_idx(db, x).unwrap(), None));
                return spine.into_iter().rev().fold(default, |acc, next| {
                    let next = db.thir_quote(lvl, next)?;
                    Ok(Term::App(acc?.into(), next.into()))
                });
            }
            Pi(pi) => {
                // Pi (quote lvl pi.type_rep) (quote (lvl + 1) (pi.closure $$ (Var lvl pi.name)))
                let name = create_reference_of(db, pi.name, location.clone());
                let domain = db.thir_quote(lvl, *pi.type_repr.clone())?;
                let codomain = db.thir_quote(lvl.increase(db), Value::new_var(lvl, name))?;

                Term::Pi(pi.name, pi.implicitness, domain.into(), codomain.into())
            }
            Lam(name, implicitness, closure) => {
                // Lam (quote (lvl + 1) (closure $$ (Var lvl name)))
                let argument =
                    Value::new_var(lvl, create_reference_of(db, name.into(), location.clone()));
                let closure = db.thir_quote(lvl.increase(db), closure.apply(db, argument)?)?;

                Term::Lam(name, implicitness, closure.into())
            }
            Location(location, term) => Term::Location(location, db.thir_quote(lvl, *term)?.into()),
        })
    }

    let (location, value) = value.force(db);

    location
        .map(|location| {
            let value = thir_quote_impl(db, Some(location.clone()), lvl, value.clone())?;

            Ok(Term::Location(location.clone(), value.into()))
        })
        .unwrap_or_else(|| thir_quote_impl(db, None, lvl, value))
        .into_sol_diagnostic()
}

pub fn extract_parameter_definition(db: &dyn ThirLoweringDb, pattern: Pattern) -> Definition {
    let location = pattern.location(db);
    let hole = HirPath::create(db, "_");
    let fallback = Definition::new(
        db,
        /* id = */ DefinitionId::new(db, location, hole.to_string(db)),
        /* kind = */ Variable,
        /* name = */ hole,
    );

    match pattern {
        Pattern::Hole | Pattern::Error(_) | Pattern::Wildcard(_) => fallback,
        Pattern::Literal(_) | Pattern::Constructor(_) => todo!("handle: unsuporrted pattern"),
        Pattern::Rest(_) => todo!("rest must be used inside a pattern"),
        Pattern::Binding(binding) => binding.name(db),
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
