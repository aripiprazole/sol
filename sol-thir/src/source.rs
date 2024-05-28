use std::fmt::Display;

use super::*;

pub type Expr = Box<Term>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Term {
    U,
    Var(debruijin::Index, Option<Reference>),
    Lam(Definition, shared::Implicitness, Expr),
    App(Expr, Expr),
    Pi(Option<Definition>, shared::Implicitness, Expr, Expr),
    Constructor(shared::Constructor),
    Ann(Expr, Expr),
    Location(Location, Expr),
    Sorry(Location, Option<ThirError>),
    InsertedMeta(shared::MetaVar),
}

impl Default for Term {
    fn default() -> Self {
        Term::U
    }
}

impl Term {
    pub fn normalise(self, db: &dyn ThirDb, env: shared::Env) -> sol_eyre::Result<Term> {
        db.thir_quote(Level::new(db, env.len(db)), db.thir_eval(env, self)?)
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
