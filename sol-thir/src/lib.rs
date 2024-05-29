//! Type terms to type infer and check the sol programs, they are the simpliest
//! normal-form terms.

#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(trait_upcasting)]

use debruijin::Level;
use salsa::DbWithJar;
use shared::{Context, Env};
use sol_diagnostic::DiagnosticDb;
use sol_hir::{
    lowering::HirLowering,
    package::HasManifest,
    primitives::PrimitiveProvider,
    solver::{Definition, Reference},
    source::{expr::Expr, literal::Literal, Location},
    HirDb,
};
use sol_syntax::ParseDb;

use crate::{
    source::Term,
    value::{Type, Value},
};

extern crate salsa_2022 as salsa;

pub mod debruijin;
pub mod sexpr;
pub mod shared;
pub mod source;
pub mod unification;
pub mod value;

#[salsa::jar(db = ThirDb)]
pub struct Jar(
    shared::GlobalEnv,
    shared::Env,
    shared::Env_get,
    shared::Env_push,
    shared::Env_len,
    shared::Context,
    shared::Context_create_new_value,
    shared::Context_insert_new_binder,
    debruijin::Indices,
    debruijin::Level,
    debruijin::Level_as_idx,
    debruijin::Level_increase,
);

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

impl<DB> ThirDb for DB where
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
    fn thir_eval(&self, env: Env, term: Term) -> sol_diagnostic::Result<Value>;

    fn thir_quote(&self, lvl: Level, value: Value) -> sol_diagnostic::Result<Term>;
}

/// Represents the typing functions for Typed High-Level Intermediate Representation.
pub trait ThirTyping {
    /// The infer function to infer the type of the term.
    fn thir_infer(&self, ctx: Context, expr: Expr) -> sol_diagnostic::Result<ElaboratedTerm>;

    /// The check function to check the type of the term.
    fn thir_check(&self, ctx: Context, expr: Expr, type_repr: Type)
        -> sol_diagnostic::Result<Term>;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ElaboratedTerm(pub Term, pub Type);

impl From<(Term, Type)> for ElaboratedTerm {
    fn from((term, ty): (Term, Type)) -> Self {
        Self(term, ty)
    }
}

impl Default for ElaboratedTerm {
    fn default() -> Self {
        todo!()
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, thiserror::Error, miette::Diagnostic)]
#[error("elaboration error")]
#[diagnostic(code(solc::thir::error), url(docsrs))]
pub struct ThirError {
    #[source_code]
    pub source_code: Location,

    #[label = "here"]
    pub label: miette::SourceSpan,

    #[source]
    #[diagnostic_source]
    pub kind: ThirErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error, miette::Diagnostic)]
pub enum ThirErrorKind {
    #[error("unexpected no expression")]
    #[diagnostic(code(solc::thir::empty), url(docsrs))]
    Empty,

    #[error("could not find {0} definition: {1}")]
    #[diagnostic(code(solc::thir::unresolved_definition), url(docsrs))]
    UnresolvedDefinition(String, String),

    #[error("incorrect kind: {0}")]
    #[diagnostic(code(solc::thir::incorrect_kind), url(docsrs))]
    IncorrectKind(String),
}
