use miette::SourceSpan;

use crate::source::Location;

#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error, miette::Diagnostic)]
pub enum HirErrorKind {
    /// An error occurred while parsing the source code.
    ///
    /// This error is used when the parser encounters an unexpected token.
    #[error("unexpected no expression")]
    #[diagnostic(code(solc::hir_internal_error), url(docsrs))]
    Empty,

    /// Could not find a definition in source code. This is used when the solver
    /// cannot find a definition in the source code.
    ///
    /// ```
    /// A = b
    /// ```
    ///
    /// And b is free bound.
    #[error("could not find {0} definition: {1}")]
    #[diagnostic(code(solc::hir_unresolved_definition), url(docsrs))]
    UnresolvedDefinition(String, String),

    /// An error occurred while parsing the source code.
    #[error("incorrect kind: {0}")]
    IncorrectKind(String),

    /// Return outside do notation.
    #[error("return outside do notation")]
    #[diagnostic(code(solc::hir_return_outside_do_notation), url(docsrs))]
    ReturnOutsideDoNotation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error, miette::Diagnostic)]
#[diagnostic(code(solc::hir_error), url(docsrs))]
#[error("hir error: {kind}")]
pub struct HirError {
    #[source_code]
    pub source_code: Location,

    #[label = "here"]
    pub label: SourceSpan,

    #[source]
    #[diagnostic_source]
    pub kind: HirErrorKind,
}
