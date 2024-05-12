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
    Meta(usize),
    Location(Location, Expr),
    Sorry(Location, Option<ThirError>),
}

impl Term {
    pub fn normalise(self, db: &dyn ThirDb, env: shared::Env) -> Term {
        db.thir_quote(Level::new(db, env.len(db)), db.thir_eval(env, self))
    }
}
