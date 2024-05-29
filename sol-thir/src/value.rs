use shared::MetaVar;

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

impl Default for Value {
    fn default() -> Self {
        Value::Flexible(MetaVar::new(None), vec![])
    }
}

impl Value {
    pub fn new_var(lvl: debruijin::Level, _reference: Option<Reference>) -> Value {
        Value::Rigid(lvl, vec![])
    }

    pub fn force(self, db: &dyn ThirDb) -> sol_diagnostic::Result<(Option<Location>, Value)> {
        // TODO: use db
        let _ = db;

        match self {
            Value::Flexible(ref m, ref spine) => Ok((None, match m.get() {
                Some(value) => value.apply_with_spine(db, spine.clone())?,
                None => self,
            })),
            Value::Location(ref location, _) => Ok((Some(location.clone()), self.force(db)?.1)),
            _ => Ok((None, self)),
        }
    }

    pub fn apply_with_spine(
        self,
        db: &dyn ThirDb,
        spine: Vec<Value>,
    ) -> sol_diagnostic::Result<Value> {
        spine
            .into_iter()
            .rev()
            .try_fold(self, |acc, next| acc.apply_to_spine(db, next))
    }

    pub fn apply_to_spine(self, db: &dyn ThirDb, argument: Value) -> sol_diagnostic::Result<Value> {
        match self {
            Value::Lam(_, _, closure) => closure.apply(db, argument),
            Value::Flexible(meta, mut spine) => {
                spine.push(argument);
                Ok(Value::Flexible(meta, spine))
            }
            Value::Rigid(lvl, mut spine) => {
                spine.push(argument);
                Ok(Value::Rigid(lvl, spine))
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
    pub fn apply(self, db: &dyn ThirDb, value: Value) -> sol_diagnostic::Result<Value> {
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
    pub type_repr: Box<Type>,
    pub closure: Closure,
}
