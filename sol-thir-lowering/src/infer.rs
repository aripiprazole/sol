use sol_thir::{shared::Constructor, InferResult};

use super::*;

/// The infer function to infer the type of the term.
#[salsa::tracked]
pub fn thir_infer(db: &dyn ThirLoweringDb, ctx: Context, expr: Expr) -> InferResult {
    use Expr::*;

    ctx.location(db).update(expr.location(db));

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
        Upgrade(box TypeRep::App(_)) => todo!(),
        Upgrade(box TypeRep::Hole) => todo!(),
        Upgrade(box TypeRep::Unit) => todo!(),
        Upgrade(box TypeRep::Type) => todo!(),
        Upgrade(box TypeRep::Pi(_)) => todo!(),
        Upgrade(box TypeRep::Path(_, _)) => todo!(),
        Upgrade(box TypeRep::Error(_) | box TypeRep::SelfType) => todo!("unsuporrted type rep"),
        Upgrade(box TypeRep::Downgrade(box expr)) => return db.thir_infer(ctx, expr),
    })
}
