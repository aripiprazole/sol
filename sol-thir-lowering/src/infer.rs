use sol_thir::{shared::Constructor, InferResult};

use super::*;

/// The infer function to infer the type of the term.
#[salsa::tracked]
pub fn thir_infer(db: &dyn ThirLoweringDb, ctx: Context, expr: Expr) -> InferResult {
    use Expr::*;

    InferResult::from(match expr {
        Empty | Error(_) | Match(_) => todo!(),
        Path(_) => todo!(),
        Literal(literal) => {
            let constructor = Constructor {
                location: literal.location(db),
                kind: literal.value.into(),
            };
            (Term::Constructor(constructor.clone()), constructor.infer())
        }
        Call(_) => todo!(),
        Ann(_) => todo!(),
        Abs(_) => todo!(),
        Type(_, _) => todo!(),
        Pi(_) => todo!(),
        Sigma(_) => todo!(),
        Hole(_) => todo!(),
    })
}
