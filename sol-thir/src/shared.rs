use std::sync::{Arc, Mutex};

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

#[derive(Debug, Clone, Hash, Eq)]
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

impl PartialEq for CurrentLocation {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[salsa::tracked]
pub struct Env {
    pub values: Vec<value::Value>,
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
