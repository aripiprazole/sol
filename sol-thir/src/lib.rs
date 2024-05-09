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

extern crate salsa_2022 as salsa;

#[salsa::jar(db = ThirDb)]
pub struct Jar(Env, eval);

pub trait ThirDb:
    PrimitiveProvider + HirDb + HirLowering + HasManifest + ParseDb + DiagnosticDb + DbWithJar<Jar>
{
}

impl<DB: HasManifest + HirDb + HirLowering + PrimitiveProvider> ThirDb for DB where
    DB: ?Sized + ParseDb + DiagnosticDb + salsa::DbWithJar<Jar>
{
}

/// Represents the diagnostic for High-Level Intermediate Representation. It's intended to be used
/// to report errors to the diagnostic database, by this crate, only.
#[derive(Debug)]
pub struct ThirDiagnostic {
    pub location: Location,
    pub kind: ThirError,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum ThirError {
    HirError(HirError),
    UpgradeNotSupported,
    MatchNotSupported,
}

impl Diagnostic for ThirDiagnostic {
    type TextRange = Location;

    const KIND: ErrorKind = ErrorKind::LoweringError;

    fn text(&self) -> Vec<sol_diagnostic::ErrorText> {
        todo!()
    }

    fn location(&self) -> Option<Self::TextRange> {
        Some(self.location.clone())
    }

    fn error_id(&self) -> ErrorId {
        todo!()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ConstructorKind {
    True,
    False,
    Definition(Definition),
    Int(isize),
    String(String),
}

/// Constant, or primitive value that has no subterms
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Constructor {
    pub kind: ConstructorKind,
    pub location: Location,
}

pub type Type = Term;

#[salsa::tracked]
pub struct Env {
    pub values: Vec<Value>,
}

/// Value that can have a type associated with it.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Value {
    Type(Term),
    Runtime(Term, Type),
}

/// It does represent a type level function stores the environment and can
/// take environments to evaluate the quoted expression.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Closure {
    pub env: Env,
    pub expr: Expr,
}

/// Implicitness of a term.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Implicitness {
    Implicit,
    Explicit,
}

impl From<bool> for Implicitness {
    fn from(value: bool) -> Self {
        if value {
            Implicitness::Implicit
        } else {
            Implicitness::Explicit
        }
    }
}

/// Dependent function type, it's a type-level function
/// that depends on a value.
///
/// It allows we to construct every dependent-type features.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Pi {
    pub name: Definition,
    pub implicitness: Implicitness,
    pub type_rep: Box<Type>,
    pub closure: Closure,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Object {
    Tuple(Vec<Value>),
    Record(im::HashMap<Definition, Value>),
}

/// Basic normalized expression, it has the term's NFE.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Term {
    Var(Index, Option<Reference>),
    Object(Object),
    Constructor(Constructor),
    Flexible(Meta, Vec<Value>),
    Rigid(Level, Vec<Value>),
    Pi(Pi),
    Lam(Definition, Implicitness, Closure),
    Type,
    Location(Location, Box<Term>),
    Sorry(Location, Option<ThirError>),
}

impl Term {
    pub fn located(location: Location, value: Term) -> Term {
        Term::Location(location, Box::new(value))
    }

    pub fn sorry(location: Location, error: ThirError) -> Term {
        Term::Sorry(location, Some(error))
    }

    pub fn sorry_but_no(location: Location) -> Term {
        Term::Sorry(location, None)
    }

    pub fn no() -> Term {
        Term::Sorry(Location::CallSite, None)
    }
}

pub fn literal_to_constructor_kind(literal: Literal) -> ConstructorKind {
    match literal {
        Literal::Empty => todo!(),
        Literal::Int8(_) => todo!(),
        Literal::UInt8(_) => todo!(),
        Literal::Int16(_) => todo!(),
        Literal::UInt16(_) => todo!(),
        Literal::Int32(_) => todo!(),
        Literal::UInt32(_) => todo!(),
        Literal::Int64(_) => todo!(),
        Literal::UInt64(_) => todo!(),
        Literal::String(_) => todo!(),
        Literal::Boolean(_) => todo!(),
        Literal::Char(_) => todo!(),
    }
}

pub fn get_definition(db: &dyn crate::ThirDb, pattern: Pattern) -> Definition {
    match pattern {
        Pattern::Hole => todo!(),
        Pattern::Literal(_) => todo!(),
        Pattern::Wildcard(_) => todo!(),
        Pattern::Rest(_) => todo!(),
        Pattern::Error(_) => todo!(),
        Pattern::Constructor(_) => todo!(),
        Pattern::Binding(binding) => binding.name,
    }
}

#[salsa::tracked]
pub fn eval(db: &dyn crate::ThirDb, env: Env, expr: Expr) -> Term {
    Term::located(expr.location(db), match expr {
        Expr::Meta(Meta(location, value)) => Term::sorry_but_no(value),
        Expr::Literal(ref literal) => Term::Constructor(Constructor {
            location: literal.location.clone().unwrap_or(expr.location(db)),
            kind: literal_to_constructor_kind(literal.value.clone()),
        }),
        Expr::Path(path) => Term::Constructor(Constructor {
            location: path.location(db),
            kind: ConstructorKind::Definition(path.definition(db)),
        }),
        Expr::Call(call) => todo!(),
        Expr::Ann(ann) => todo!(),
        Expr::Abs(abs) => abs
            .parameters
            .into_iter()
            .fold(Err(*abs.value), |acc, param| {
                let location = param.location(db);
                let definition = get_definition(db, param);
                let term = Term::Lam(definition, Implicitness::Explicit, Closure {
                    env,
                    expr: match acc {
                        Ok(value) => quote(db, value),
                        Err(expr) => expr,
                    },
                });

                Ok(Term::located(location, term))
            })
            .expect("no first parameter"),
        Expr::Match(expr) => Term::sorry(expr.location(db), ThirError::MatchNotSupported),
        Expr::Upgrade(upgrade) => Term::sorry(upgrade.location(db), ThirError::UpgradeNotSupported),
        Expr::Error(error) => Term::sorry(error.location(db), ThirError::HirError(error)),
        Expr::Empty => Term::no(),
    })
}

pub fn quote(db: &dyn crate::ThirDb, term: Term) -> Expr {
    todo!()
}

pub fn infer(db: &dyn crate::ThirDb, env: Env, expr: Expr) -> (Expr, Type) {
    todo!()
}
