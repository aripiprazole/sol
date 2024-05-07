use sol_diagnostic::ErrorKind;
pub use sol_hir::source::expr::{Expr, Meta};
use sol_hir::source::HirElement;
pub use sol_hir::source::Location;

pub use crate::*;

pub fn desugar_expr(db: &dyn crate::ThirDb, expr: Expr) -> Term {
    Term::located(expr.location(db), match expr {
        Expr::Meta(Meta(location, value)) => Term::Sorry,
        Expr::Path(path) => todo!(),
        Expr::Literal(literal) => Expr::Literal(literal.desugar(db)),
        Expr::Call(call) => todo!(),
        Expr::Ann(ann) => todo!(),
        Expr::Abs(abs) => todo!(),
        Expr::Match(expr) => Term::sorry(expr.location(db), ThirError::MatchNotSupported),
        Expr::Upgrade(upgrade) => Term::sorry(upgrade.location(db), ThirError::UpgradeNotSupported),
        Expr::Error(error) => Term::sorry(error.location(db), ThirError::HirError(error)),
        Expr::Empty => Term::no(),
    })
}
