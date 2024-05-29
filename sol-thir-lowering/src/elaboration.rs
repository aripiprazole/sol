use sol_diagnostic::Diagnostics;
use sol_thir::ElaboratedTerm;

use super::*;

#[salsa::tracked]
pub fn unify_catch(db: &dyn ThirLoweringDb, ctx: Context, lhs: Value, rhs: Value) {
    if let Err(diagnostic) = lhs.unify(db, ctx, rhs) {
        Diagnostics::push(db, diagnostic);
    }
}

/// Insert fresh implicit applications to a term which is not
/// an implicit lambda (i.e. neutral).
pub fn insert(ctx: Context, term: Term, type_repr: Value) -> ElaboratedTerm {
    todo!()
}
