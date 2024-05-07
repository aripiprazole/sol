//! Type terms to type infer and check the sol programs, they are the simpliest
//! normal-form terms.

#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(trait_upcasting)]

use salsa::DbWithJar;
use sol_diagnostic::{DiagnosticDb, ErrorKind};
use sol_hir::debruijin::{Index, Level};
use sol_hir::lowering::HirLowering;
use sol_hir::package::HasManifest;
use sol_hir::primitives::PrimitiveProvider;
use sol_hir::solver::{Definition, Reference};
use sol_hir::source::expr::{Expr, Meta};
use sol_hir::source::{HirError, Location};
use sol_syntax::ParseDb;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = ThirDb)]
pub struct Jar();

pub trait ThirDb:
    PrimitiveProvider + HirLowering + HasManifest + ParseDb + DiagnosticDb + DbWithJar<Jar>
{
}

impl<DB: HasManifest + HirLowering + PrimitiveProvider> ThirDb for DB where
    DB: ?Sized + ParseDb + DiagnosticDb + salsa::DbWithJar<Jar>
{
}

pub mod desugar;

/// Represents the diagnostic for High-Level Intermediate Representation. It's intended to be used
/// to report errors to the diagnostic database, by this crate, only.
#[derive(Debug)]
pub struct ThirDiagnostic {
    pub location: Location,
    pub kind: ThirError,
}

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
        self.id
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ConstructorKind {
    True,
    False,
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
pub type Env = Vec<Value>;

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
    Impl,
    Expl,
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
    Lam(Closure),
    Type,
    Location(Location, Box<Term>),
    Sorry(Location, Option<ThirError>),
}

impl Term {
    pub fn located(location: Location, value: Term) -> Term {
        Term::Location(location, Box::new(value))
    }

    pub fn sorry(location: Location, error: ThirError) -> Term {
        Term::Sorry(location, Some(p))
    }

    pub fn no() -> Term {
        Term::Sorry(Location::CallSite, None)
    }
}
