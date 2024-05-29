//! Defines a kind of terms. It does define patterns that can be used in clauses and pattern
//! matching agains't a value.
//!
//! It can be known as the name of eliminating a value.

use std::fmt::Formatter;

use super::*;
use crate::{
    errors::HirError,
    solver::{Definition, Reference},
    walking::HirListener,
};

/// The constructor kind. It's used to define the kind of a constructor, and to improve the
/// type checking of the constructors.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Constructor {
    Array,
    Tuple,
    Unit,
    Path(Reference),
}

impl walking::Walker for Constructor {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        match self {
            Self::Array => {}
            Self::Tuple => {}
            Self::Unit => {}
            Self::Path(path) => {
                path.accept(db, listener);
            }
        }
    }
}

/// Defines a constructor pattern. It's a pattern that can be used to match agains't a
/// constructor. It's matching agains't a constructor, and it's arguments.
///
/// The constructor can have different kinds, like a tuple, a unit, a path, or a array, but
/// the signature is the same.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ConstructorPattern {
    pub name: Constructor,
    pub arguments: Vec<Pattern>,
    pub location: Location,
}

impl walking::Walker for ConstructorPattern {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        listener.enter_constructor_pattern(self.clone());
        self.name.clone().accept(db, listener);
        self.arguments.clone().accept(db, listener);
        self.location(db).accept(db, listener);
        listener.exit_constructor_pattern(self);
    }
}

impl HirElement for ConstructorPattern {
    fn location(&self, _: &dyn crate::HirDb) -> Location {
        self.location.clone()
    }
}

impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for ConstructorPattern {
    fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Defines a binding pattern. It's a pattern that can be used to bind a value to a name. It's
/// matching agains't any value, but commiting a name to it's value.
///
/// The semantics are like a wildcard pattern, but it's not a wildcard pattern, because it
/// defines a name.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BindingPattern {
    pub name: Definition,
    pub location: Location,
}

impl walking::Walker for BindingPattern {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        listener.enter_binding_pattern(self.clone());
        self.name.accept(db, listener);
        self.location(db).accept(db, listener);
        listener.exit_binding_pattern(self);
    }
}

impl HirElement for BindingPattern {
    fn location(&self, _: &dyn crate::HirDb) -> Location {
        self.location.clone()
    }
}

impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for BindingPattern {
    fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Defines the pattern element in the HIR.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Pattern {
    /// The empty parameter should work as a wildcard, it's just like a `_` pattern. But it's
    /// splitted into a different variant to make it easier to work with, when we don't have
    /// an actual pattern.
    ///
    /// It's useful to do code generation. This pattern matches agains't every value, just like
    /// a wildcard.
    ///
    /// Actually, in this compiler, we have a few another patterns that are just like a wildcard
    /// pattern, but they are not represented as a wildcard pattern, because they are actual
    /// patterns, these are:
    /// - [`Pattern::Error`]
    /// - [`Pattern::Binding`]
    Hole,
    Literal(Spanned<literal::Literal>),
    Wildcard(Location),
    Rest(Location),
    Error(HirError),
    Constructor(ConstructorPattern),
    Binding(BindingPattern),
}

impl DefaultWithDb for Pattern {
    fn default_with_db(_db: &dyn crate::HirDb) -> Self {
        Self::Hole
    }

    fn error(_db: &dyn crate::HirDb, error: HirError) -> Self {
        Self::Error(error)
    }
}

impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for Pattern {
    fn fmt(&self, f: &mut Formatter<'_>, db: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
        match self {
            Pattern::Hole => write!(f, "Empty"),
            Pattern::Literal(literal) => write!(f, "Literal({literal:?})"),
            Pattern::Wildcard(wildcard) => write!(f, "Wildcard(location: {wildcard:?})"),
            Pattern::Rest(rest) => write!(f, "Rest(location: {rest:?})"),
            Pattern::Error(error) => write!(f, "Error({error:?})"),
            Pattern::Constructor(constructor) => constructor.debug_all(db).fmt(f),
            Pattern::Binding(binding) => binding.debug_all(db).fmt(f),
        }
    }
}

impl walking::Walker for Pattern {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        match self {
            Pattern::Hole => listener.visit_empty_pattern(),
            Pattern::Constructor(constructor) => constructor.accept(db, listener),
            Pattern::Binding(binding) => binding.accept(db, listener),
            Pattern::Literal(literal) => {
                listener.enter_literal_pattern(literal.clone());
                literal.clone().accept(db, listener);
                listener.exit_literal_pattern(literal);
            }
            Pattern::Wildcard(location) => {
                listener.enter_wildcard_pattern(location.clone());
                location.clone().accept(db, listener);
                listener.exit_wildcard_pattern(location);
            }
            Pattern::Rest(location) => {
                listener.enter_rest_pattern(location.clone());
                location.clone().accept(db, listener);
                listener.exit_rest_pattern(location);
            }
            Pattern::Error(_) => {}
        }
    }
}

impl HirElement for Pattern {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        match self {
            Self::Hole => Location::call_site(db),
            Self::Literal(literal) => literal.location.clone().unwrap(),
            Self::Wildcard(location) => location.clone(),
            Self::Rest(location) => location.clone(),
            Self::Constructor(downcast) => downcast.location(db),
            Self::Binding(downcast) => downcast.location(db),
            Self::Error(downcast) => downcast.label.clone(),
        }
    }
}
