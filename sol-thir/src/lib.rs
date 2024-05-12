//! Type terms to type infer and check the sol programs, they are the simpliest
//! normal-form terms.

#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(trait_upcasting)]

use salsa::DbWithJar;
use shared::Env;
use sol_diagnostic::{Diagnostic, DiagnosticDb, ErrorId, ErrorKind};
use sol_hir::{
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

use crate::{
    source::Term,
    value::{Type, Value},
};

extern crate salsa_2022 as salsa;

pub mod shared;
pub mod source;
pub mod value;

#[salsa::jar(db = ThirDb)]
pub struct Jar(shared::Env);

pub trait ThirDb:
    PrimitiveProvider
    + HirDb
    + ThirLowering
    + ThirTyping
    + HirLowering
    + HasManifest
    + ParseDb
    + DiagnosticDb
    + DbWithJar<Jar>
{
}

impl ThirDb for DB where
    DB: ?Sized
        + ParseDb
        + DiagnosticDb
        + HasManifest
        + HirDb
        + ThirLowering
        + ThirTyping
        + HirLowering
        + PrimitiveProvider
        + salsa::DbWithJar<Jar>
{
}

/// Represents the lowering functions for Low-Level Intermediate Representation.
pub trait ThirLowering {
    fn thir_lower(&self, db: &dyn ThirDb, expr: Expr) -> Term;

    fn thir_eval(&self, db: &dyn ThirDb, env: Env, term: Term) -> Value;
}

/// Represents the typing functions for Typed High-Level Intermediate Representation.
pub trait ThirTyping {
    /// The quoting function to convert the value back to the term.
    fn thir_quote(&self, db: &dyn ThirDb, value: Value) -> Term;

    /// The infer function to infer the type of the term.
    fn thir_infer(&self, db: &dyn ThirDb, env: Env, term: Term) -> (Term, Type);

    /// The check function to check the type of the term.
    fn thir_check(&self, db: &dyn ThirDb, env: Env, term: Term, type_repr: Type) -> Term;
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
