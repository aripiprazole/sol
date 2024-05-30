use std::{
    collections::VecDeque,
    hash::Hash,
    sync::{Arc, Mutex},
};

use sol_hir::package::Package;

use self::debruijin::Index;
use super::*;

/// Constant, or primitive value that has no subterms
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Constructor {
    pub kind: ConstructorKind,
    pub location: Location,
}

#[salsa::input]
pub struct GlobalEnv {
    pub definitions: im::HashMap<Definition, (Term, Type)>,
}

#[salsa::input]
pub struct Context {
    pub lvl: Level,
    pub locals: Env,
    pub env: GlobalEnv,
    pub pkg: Package,
}

impl Context {
    pub fn default_with_env(db: &dyn ThirDb, env: GlobalEnv, pkg: Package) -> Self {
        Self::new(
            db,
            Level::new(db, 0),
            Env::new(db, VecDeque::new()),
            env,
            pkg,
        )
    }
}

#[salsa::tracked]
impl Context {
    #[salsa::tracked]
    pub fn increase_level(self, db: &dyn ThirDb) -> Context {
        let lvl = self.lvl(db).increase(db);
        Context::new(db, lvl, self.locals(db), self.env(db), self.pkg(db))
    }

    #[salsa::tracked]
    pub fn create_new_value(self, db: &dyn ThirDb, name: Definition, value: Value) -> Context {
        todo!()
    }

    #[salsa::tracked]
    pub fn insert_new_binder(self, db: &dyn ThirDb, name: Definition, value: Value) -> Context {
        todo!()
    }
}

#[salsa::input]
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
    pub fn get(self, db: &dyn ThirDb, idx: Index) -> Value {
        let Index(idx) = idx;
        self.values(db).get(idx).unwrap().clone()
    }

    #[salsa::tracked]
    pub fn len(self, db: &dyn ThirDb) -> usize {
        self.values(db).len()
    }
}

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
    UnitType,
    True,
    False,
    BooleanType,
    NatType,
    Reference(Reference),
    IntType(bool, isize),
    Int(isize),
    StringType,
    String(String),
}

impl From<Literal> for ConstructorKind {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Empty => ConstructorKind::Unit,
            Literal::Int8(value) => ConstructorKind::Int(value as _),
            Literal::UInt8(value) => ConstructorKind::Int(value as _),
            Literal::Int16(value) => ConstructorKind::Int(value as _),
            Literal::UInt16(value) => ConstructorKind::Int(value as _),
            Literal::Int32(value) => ConstructorKind::Int(value as _),
            Literal::UInt32(value) => ConstructorKind::Int(value as _),
            Literal::Int64(value) => ConstructorKind::Int(value as _),
            Literal::UInt64(value) => ConstructorKind::Int(value as _),
            Literal::String(string) => ConstructorKind::String(string),
            Literal::Boolean(true) => ConstructorKind::True,
            Literal::Boolean(false) => ConstructorKind::False,
            Literal::Char(char) => ConstructorKind::Int(char as _),
        }
    }
}

macro_rules! mutable_reference {
    (struct $name:ident = $type:ty) => {
        pub struct $name(Arc<Mutex<$type>>);

        impl $name {
            pub fn new(value: $type) -> Self {
                Self(Arc::new(Mutex::new(value)))
            }

            pub fn get(&self) -> $type {
                self.0.lock().unwrap().clone()
            }

            pub fn update(&self, value: $type) {
                *self.0.lock().unwrap() = value;
            }
        }

        impl Clone for $name {
            fn clone(&self) -> Self {
                Self(self.0.clone())
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.lock().unwrap().fmt(f)
            }
        }

        impl Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.lock().unwrap().hash(state)
            }
        }

        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                Arc::ptr_eq(&self.0, &other.0)
            }
        }

        impl Eq for $name {}
    };
}

mutable_reference! {
  struct MetaVar = Option<Value>
}
