use std::{
    collections::VecDeque,
    hash::Hash,
    sync::{Arc, Mutex},
};

use super::*;

/// Constant, or primitive value that has no subterms
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Constructor {
    pub kind: ConstructorKind,
    pub location: Location,
}

#[salsa::tracked]
pub struct Context {
    pub location: CurrentLocation,
}

pub struct CurrentLocation(Arc<Mutex<Location>>);

impl CurrentLocation {
    pub fn new(location: Location) -> Self {
        Self(Arc::new(Mutex::new(location)))
    }

    pub fn get(&self) -> Location {
        self.0.lock().unwrap().clone()
    }

    pub fn update(&self, location: Location) {
        *self.0.lock().unwrap() = location;
    }
}

impl Clone for CurrentLocation {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl std::fmt::Debug for CurrentLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.lock().unwrap().fmt(f)
    }
}

impl Hash for CurrentLocation {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.lock().unwrap().hash(state)
    }
}

impl PartialEq for CurrentLocation {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for CurrentLocation {}

#[salsa::tracked]
pub struct Env {
    pub values: VecDeque<value::Value>,
}

#[salsa::tracked]
impl Env {
    #[salsa::tracked]
    pub fn push(self, db: &dyn ThirDb, value: value::Value) -> Env {
        let mut values = self.values(db);
        values.push_front(value);
        Env::new(db, values)
    }

    #[salsa::tracked]
    pub fn len(self, db: &dyn ThirDb) -> usize {
        self.values(db).len()
    }
}

pub type Meta = usize;

/// Implicitness of a term.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Implicitness {
    Implicit,
    Explicit,
}

impl From<bool> for Implicitness {
    fn from(value: bool) -> Self {
        if value {
            Implicitness::Implicit
        } else {
            Implicitness::Explicit
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ConstructorKind {
    Unit,
    True,
    False,
    Definition(Definition),
    Int(isize),
    String(String),
}

impl From<Literal> for ConstructorKind {
    fn from(literal: Literal) -> Self {
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
}
