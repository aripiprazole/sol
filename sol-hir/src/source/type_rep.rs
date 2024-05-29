//! Defines a kind of terms. It does define type representations that can be used in the type level
//! of the language. These are the base of the language grammar and semantics.
use super::*;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeRep {
    pub expr: Box<expr::Expr>,
}

impl TypeRep {
    pub fn downgrade(&self) -> expr::Expr {
        *self.expr.clone()
    }
}

impl Default for TypeRep {
    fn default() -> Self {
        Self {
            expr: Box::new(expr::Expr::Empty),
        }
    }
}

impl HirElement for TypeRep {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        self.expr.location(db)
    }
}

impl walking::Walker for TypeRep {
    fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.expr.accept(db, listener);
    }
}
