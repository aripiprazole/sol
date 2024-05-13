use crate::{
    shared::{self, Implicitness},
    source::Term,
    value::Value,
};

#[derive(miette::Diagnostic, thiserror::Error, Debug, Clone, PartialEq)]
#[diagnostic()]
pub enum UnifyError {
    /// Int value mismatch between two values,
    #[error("expected int value: {0}, and got: {1}")]
    #[diagnostic(url(docsrs), code(unify::int_mismatch))]
    MismatchBetweenInts(isize, isize),

    /// String value mismatch between two values,
    #[error("expected string value: \"{0}\", and got: \"{1}\"")]
    #[diagnostic(url(docsrs), code(unify::str_mismatch))]
    MismatchBetweenStrs(String, String),

    /// Icit mismatch between two values,
    #[error("expected a value with implicitness: {0:?} of `{2}`, and got: {1:?} of `{3}`")]
    #[diagnostic(url(docsrs), code(unify::icit_mismatch))]
    IcitMismatch(Implicitness, Implicitness, Term, Term),

    /// Unification error between two types
    #[error("expected type: `{0}`, got the type: `{1}`")]
    #[diagnostic(url(docsrs), code(unify::cant_unify))]
    CantUnify(Term, Term),
}

impl Value {
    /// Performs unification between two values, its a
    /// equality relation between two values.
    ///
    /// It does closes some holes.
    ///
    /// # Parameters
    ///
    /// - `self` - The left hand side of the unification
    /// - `rhs`  - The right hand side of the unification
    /// - `ctx`  - The context where the unification is happening
    ///            right now
    ///
    /// # Returns
    ///
    /// It does returns an error if the unification fails. And a
    /// unit if the unification succeeds.
    pub fn unify(self, rhs: Self, ctx: shared::Context) -> miette::Result<(), UnifyError> {
        todo!()
    }
}
