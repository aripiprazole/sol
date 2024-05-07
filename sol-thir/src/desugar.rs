pub use sol_hir::source::expr::{Expr, Meta};
pub use sol_hir::source::Location;
use sol_hir::source::{literal::Literal, HirElement};

pub use crate::*;

pub fn literal_to_constructor_kind(literal: Literal) -> ConstructorKind {
    match literal {
        Literal::Empty => todo!(),
        Literal::Int8(_) => todo!(),
        Literal::UInt8(_) => todo!(),
        Literal::Int16(_) => todo!(),
        Literal::UInt16(_) => todo!(),
        Literal::Int32(_) => todo!(),
        Literal::UInt32(_) => todo!(),
        Literal::Int64(_) => todo!(),
        Literal::UInt64(_) => todo!(),
        Literal::String(_) => todo!(),
        Literal::Boolean(_) => todo!(),
        Literal::Char(_) => todo!(),
    }
}

pub fn desugar_expr(db: &dyn crate::ThirDb, expr: Expr) -> Term {
    Term::located(expr.location(db), match expr {
        Expr::Meta(Meta(location, value)) => Term::sorry_but_no(value),
        Expr::Literal(ref literal) => Term::Constructor(Constructor {
            location: literal.location.clone().unwrap_or(expr.location(db)),
            kind: literal_to_constructor_kind(literal.value.clone()),
        }),
        Expr::Path(path) => todo!(),
        Expr::Call(call) => todo!(),
        Expr::Ann(ann) => todo!(),
        Expr::Abs(abs) => todo!(),
        Expr::Match(expr) => Term::sorry(expr.location(db), ThirError::MatchNotSupported),
        Expr::Upgrade(upgrade) => Term::sorry(upgrade.location(db), ThirError::UpgradeNotSupported),
        Expr::Error(error) => Term::sorry(error.location(db), ThirError::HirError(error)),
        Expr::Empty => Term::no(),
    })
}
