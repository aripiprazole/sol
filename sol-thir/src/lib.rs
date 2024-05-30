//! Type terms to type infer and check the sol programs, they are the simpliest
//! normal-form terms.

#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(trait_upcasting)]
#![feature(box_patterns)]

use salsa::DbWithJar;
use sol_diagnostic::{fail, DiagnosticDb};
use sol_hir::{
    lowering::HirLowering,
    package::HasManifest,
    primitives::PrimitiveProvider,
    solver::{Definition, Reference},
    source::{expr::Expr, literal::Literal, HirSource, Location},
    HirDb,
};
use sol_syntax::ParseDb;

use crate::{
    debruijin::Level,
    shared::{Constructor, ConstructorKind, Context, Env},
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
    shared::Context_increase_level,
    find_reference_type,
    debruijin::Indices,
    debruijin::Level,
    debruijin::Level_as_idx,
    debruijin::Level_increase,
);

pub trait ThirDb:
    PrimitiveProvider
    + Typer
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
        + Typer
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

pub type TypeTable = im::HashMap<Definition, (Term, Type)>;

pub trait Typer {
    fn infer_type_table(&self, source: HirSource) -> sol_diagnostic::Result<TypeTable>;
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

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("could not find the source code location")]
#[diagnostic(code(sol::thir::could_not_find_location_source))]
pub struct CouldNotFindLocationSourceError {
    #[source_code]
    #[label = "here"]
    pub location: Location,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("could not find the type of the definition: {name}")]
#[diagnostic(code(sol::thir::could_not_find_type_of_definition))]
pub struct CouldNotFindTypeOfDefinitionError {
    pub name: String,

    #[source_code]
    #[label = "here"]
    pub location: Location,
}

pub fn infer_constructor(
    db: &dyn ThirDb,
    ctx: Context,
    constructor: Constructor,
) -> sol_diagnostic::Result<Type> {
    Ok(match constructor.kind {
        ConstructorKind::UnitType
        | ConstructorKind::BooleanType
        | ConstructorKind::StringType
        | ConstructorKind::NatType
        | ConstructorKind::IntType(_, _) => Type::U,
        ConstructorKind::Unit => Type::Constructor(Constructor {
            kind: ConstructorKind::UnitType,
            location: constructor.location,
        }),
        ConstructorKind::True | ConstructorKind::False => Type::Constructor(Constructor {
            kind: ConstructorKind::BooleanType,
            location: constructor.location,
        }),
        ConstructorKind::String(_) => Type::Constructor(Constructor {
            kind: ConstructorKind::StringType,
            location: constructor.location,
        }),
        ConstructorKind::Int(_) => Type::Constructor(Constructor {
            kind: ConstructorKind::IntType(true, 32),
            location: constructor.location,
        }),
        ConstructorKind::Reference(reference) => find_reference_type(db, ctx, reference)?.1,
    })
}

#[salsa::tracked]
pub fn find_reference_type(
    db: &dyn ThirDb,
    ctx: Context,
    reference: Reference,
) -> sol_diagnostic::Result<(Term, Value)> {
    let definition = reference.definition(db);
    let Some(src) = definition.location(db).source() else {
        return fail(CouldNotFindLocationSourceError {
            location: reference.location(db),
        });
    };

    let type_table =
        if definition.location(db).text_source() == reference.location(db).text_source() {
            ctx.env(db).definitions(db)
        } else {
            let hir_src = db.hir_lower(ctx.pkg(db), src);
            db.infer_type_table(hir_src)?
        };

    let Some(elaborated_type) = type_table.get(&definition).cloned() else {
        return fail(CouldNotFindTypeOfDefinitionError {
            name: definition.name(db).to_string(db).unwrap(),
            location: reference.location(db),
        });
    };

    Ok(elaborated_type)
}
