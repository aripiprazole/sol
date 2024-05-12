use super::*;

pub type Expr = Box<Term>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Term {
    Var(Index, Option<Reference>),
    Lam(Definition, shared::Implicitness, Expr),
    App(Expr, Expr),
    Pi(Option<Definition>, shared::Implicitness, Expr, Expr),
    Ann(Expr, Expr),
    Meta(usize),
    Location(Location, Expr),
    Sorry(Location, Option<ThirError>),
}
