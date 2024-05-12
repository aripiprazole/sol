use super::*;

pub type Type = Value;

/// Basic normalized expression, it has the term's NFE.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Value {
    U,
    Constructor(shared::Constructor),
    Flexible(shared::MetaVar, Vec<Value>),
    Rigid(debruijin::Level, Vec<Value>),
    Pi(Pi),
    Lam(Definition, shared::Implicitness, Closure),
    Location(Location, Box<Value>),
}

impl Value {
    pub fn new_var(lvl: debruijin::Level, _reference: Option<Reference>) -> Value {
        Value::Rigid(lvl, vec![])
    }

    pub fn force(self, db: &dyn ThirDb) -> (Option<Location>, Value) {
        // TODO: use db
        let _ = db;

        match self {
            Value::Flexible(ref m, ref spine) => (None, match m.get() {
                Some(value) => value.apply_with_spine(spine.clone()),
                None => self,
            }),
            Value::Location(ref location, _) => (Some(location.clone()), self.force(db).1),
            _ => (None, self),
        }
    }

    pub fn apply_with_spine(self, spine: Vec<Value>) -> Value {
        spine.into_iter().rev().fold(self, Value::apply_to_spine)
    }

    pub fn apply_to_spine(self, argument: Value) -> Value {
        match self {
            Value::Flexible(meta, mut spine) => {
                spine.push(argument);
                Value::Flexible(meta, spine)
            }
            Value::Rigid(lvl, mut spine) => {
                spine.push(argument);
                Value::Rigid(lvl, spine)
            }
            _ => panic!("vapp: can't apply non-function value"),
        }
    }

    pub fn located(location: Location, value: Value) -> Value {
        Value::Location(location, Box::new(value))
    }
}

/// It does represent a type level function stores the environment and can
/// take environments to evaluate the quoted expression.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Closure {
    pub env: shared::Env,
    pub expr: Term,
}

impl Closure {
    /// Apply the closure to the value. It does apply as as snoc list in the environment
    /// to be the first to be applied.
    pub fn apply(self, db: &dyn ThirDb, value: Value) -> Value {
        let closure_env = self.env.push(db, value);

        db.thir_eval(closure_env, self.expr)
    }
}

/// Dependent function type, it's a type-level function
/// that depends on a value.
///
/// It allows we to construct every dependent-type features.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Pi {
    pub name: Option<Definition>,
    pub implicitness: shared::Implicitness,
    pub type_rep: Box<Type>,
    pub closure: Closure,
}
