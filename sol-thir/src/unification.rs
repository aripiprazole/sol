use sol_diagnostic::fail;

use crate::{
    shared::{self, Context, Implicitness},
    source::Term,
    value::Value,
    ThirDb,
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

    #[error("could not apply the value: `{0}` to the value: `{1}`")]
    #[diagnostic(url(docsrs), code(unify::could_not_apply))]
    CouldNotApply(Term, Term),
}

/// Unifies a spine of applications, it does unifies two list of applications
/// that are spines.
///
/// It requires that the spines have the same length, and it does unifies
/// the spines.
fn unify_sp(
    db: &dyn ThirDb,
    sp_a: Vec<Value>,
    sp_b: Vec<Value>,
    ctx: Context,
) -> sol_diagnostic::Result<()> {
    assert!(sp_a.len() == sp_b.len(), "spines must have the same length");

    for (lhs, rhs) in sp_a.into_iter().zip(sp_b) {
        lhs.unify(db, ctx, rhs)?;
    }

    Ok(())
}

/// Pattern unification module
pub mod pattern {
    use shared::MetaVar;

    use super::*;
    use crate::debruijin::Level;

    /// Partial renaming from Γ to Δ. It does renames the variables
    /// from Γ to Δ.
    ///
    /// It does returns a list of pairs of the variables that were
    /// renamed.
    pub struct PartialRenaming {
        /// The size of domain Γ.
        pub domain: Level,

        /// The size of codomain Δ, that will be mapped from Γ.
        pub codomain: Level,

        /// The renaming function, that maps the variables from Γ to Δ.
        pub renames: im::HashMap<Level, Level>,
    }

    impl PartialRenaming {
        /// Lifts a partial renaming over an extra bound variable.
        ///
        /// Given (σ : PartialRenaming Γ Δ),
        ///   (lift σ : PartialRenaming (Γ, x : A[σ]) (Δ, x : A)).
        pub fn lift(mut self, db: &dyn ThirDb) -> Self {
            self.domain = self.domain.increase(db);
            self.codomain = self.codomain.increase(db);
            self.renames.insert(self.codomain, self.domain);
            self
        }

        /// invert : (Γ : Cxt) → (spine : Sub Δ Γ) → PartialRenaming Γ Δ@
        pub fn invert(_: &dyn ThirDb, _: Level, _: Vec<Value>) -> sol_diagnostic::Result<Value> {
            todo!()
        }

        /// Perform partial renaming on right-most term while searching the occurrences of
        /// the variable to rename.
        pub fn rename(self, _: &dyn ThirDb, _: MetaVar, _: Value) -> sol_diagnostic::Result<Term> {
            todo!()
        }
    }

    /// Solves the unification of meta variables, it does unifies the meta variables
    /// that are present in the context.
    ///
    /// It does require a solver function.
    pub fn solve(
        _db: &dyn ThirDb,
        _lvl: Level,
        m: shared::MetaVar,
        t: Value,
        _spine: Vec<Value>,
    ) -> sol_diagnostic::Result<()> {
        m.update(Some(t));
        Ok(())
    }
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
    #[rustfmt::skip]
    pub fn unify(self, db: &dyn crate::ThirDb, ctx: Context, rhs: Self) -> sol_diagnostic::Result<()> {
        use crate::value::Pi as VPi;
        use Value::*;

        match (self.force(db)?.1, rhs.force(db)?.1) {
            (U                                 , U)                                 => Ok(()),
            (Constructor(lhs)                  , Constructor(rhs))    if lhs == rhs => Ok(()),

            // Unification of application spines or meta variables, it does unifies
            // flexibles, rigids and meta variable's spines.
            //
            // It does unifies the spines of the applications.
            (Flexible(lhs, sp_a)               , Flexible(rhs, sp_b)) if lhs == rhs => {
                unify_sp(db, sp_a, sp_b, ctx)
            }
            (Rigid(lhs, sp_a)                  , Rigid(rhs, sp_b))    if lhs == rhs => {
                unify_sp(db, sp_a, sp_b, ctx)
            }

            // Lambda unification, that applies closures and pi types
            // using the spine of applications.
            //
            // It does unifies the closures and the pi types.
            (ref lhs @ Lam(_, icit_a, _)       , ref rhs @ Lam(_, icit_b, _)) if icit_a != icit_b => {
                let lhs = db.thir_quote(ctx.lvl(db), lhs.clone())?;
                let rhs = db.thir_quote(ctx.lvl(db), rhs.clone())?;
                fail(UnifyError::IcitMismatch(icit_a, icit_b, lhs, rhs))
            }
            (Lam(_, _, clos_a)                 , Lam(_, _, clos_b)) => {
                clos_a.apply(db, Value::new_var(ctx.lvl(db), None))?
                    .unify(db, ctx.increase_level(db), clos_b.apply(db, Value::new_var(ctx.lvl(db), None))?)
            }
            (Lam(_, _, clos_a)                 , rhs) => {
                clos_a.apply(db, Value::new_var(ctx.lvl(db), None))?
                    .unify(db, ctx.increase_level(db), rhs.apply_to_spine(db, Value::new_var(ctx.lvl(db), None))?)
            }
            (lhs                               , Lam(_, _, clos_b)) => {
                lhs.apply_to_spine(db, Value::new_var(ctx.lvl(db), None))?
                    .unify(db, ctx.increase_level(db), clos_b.apply(db, Value::new_var(ctx.lvl(db), None))?)
            }

            // Pi type unification, it does unifies the domain and the codomain
            // of the pi types.
            //
            // NOTE: cod stands for codomain, and dom stands for domain.
            (ref lhs @ Pi(VPi { implicitness: icit_a, .. })      , ref rhs @ Pi(VPi { implicitness: icit_b, .. })) if icit_a != icit_b => {
                let lhs = db.thir_quote(ctx.lvl(db), lhs.clone())?;
                let rhs = db.thir_quote(ctx.lvl(db), rhs.clone())?;
                fail(UnifyError::IcitMismatch(icit_a, icit_b, lhs, rhs))
            }
            (Pi(VPi { type_repr: box dom_a, closure: cod_a, .. }), Pi(VPi { type_repr: box dom_b, closure: cod_b, .. })) => {
                dom_a.unify(db, ctx, dom_b)?;
                cod_a.apply(db, Value::new_var(ctx.lvl(db), None))?
                    .unify(db, ctx.increase_level(db), cod_b.apply(db, Value::new_var(ctx.lvl(db), None))?)?;

                Ok(())
            }

            // Unification of meta variables, it does unifies meta variables that
            // are present in the context.
            //
            // It does require a solver function.
            //
            // TODO: Solve
            (Flexible(m, sp), t) | (t, Flexible(m, sp)) => pattern::solve(db, ctx.lvl(db), m, t, sp),

            // Fallback case, it does fail with the unification error.
            (lhs, rhs) => {
                let lhs = db.thir_quote(ctx.lvl(db), lhs)?;
                let rhs = db.thir_quote(ctx.lvl(db), rhs)?;
                fail(UnifyError::CantUnify(lhs, rhs))
            }

        }
    }
}
