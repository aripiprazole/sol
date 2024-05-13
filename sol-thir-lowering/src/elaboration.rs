use sol_thir::InferResult;

use super::*;

#[salsa::tracked]
pub fn unify_catch(db: &dyn ThirLoweringDb, ctx: Context, lhs: Value, rhs: Value) {
    todo!()
}

/// Insert fresh implicit applications to a term which is not
/// an implicit lambda (i.e. neutral).
pub fn insert(ctx: Context, term: Term, type_repr: Value) -> InferResult {
    todo!()
}
