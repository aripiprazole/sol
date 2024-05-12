use super::*;

pub type Type = ValueKind;

/// Value that can have a type associated with it.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Value {
    Type(ValueKind),
    Runtime(ValueKind, Type),
}

/// It does represent a type level function stores the environment and can
/// take environments to evaluate the quoted expression.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Closure {
    pub env: shared::Env,
    pub expr: Expr,
}

/// Dependent function type, it's a type-level function
/// that depends on a value.
///
/// It allows we to construct every dependent-type features.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Pi {
    pub name: Definition,
    pub implicitness: shared::Implicitness,
    pub type_rep: Box<Type>,
    pub closure: Closure,
}

/// Basic normalized expression, it has the term's NFE.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ValueKind {
    U,
    Var(debruijin::Index, Option<Reference>),
    Constructor(shared::Constructor),
    Flexible(shared::Meta, Vec<Value>),
    Rigid(debruijin::Level, Vec<Value>),
    Pi(Pi),
    Lam(Definition, shared::Implicitness, Closure),
    Location(Location, Box<ValueKind>),
}

impl ValueKind {
    pub fn located(location: Location, value: ValueKind) -> ValueKind {
        ValueKind::Location(location, Box::new(value))
    }
}
