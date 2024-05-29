//! Defines a kind of definition. The declarations are definitions that can have a name, and can be
//! referenced by other definitions.
//!
//! The other definitions are just like declarations, but they can't be referenced by other.

use fxhash::FxBuildHasher;

use self::expr::{Expr, Type};
use super::*;
use crate::solver::{Definition, HirLevel};

/// Represents a declaration in the HIR. It's a definition that can be referenced by other
/// definitions.
pub trait Declaration: HirElement {
    /// Returns the attributes of the declaration. It does rule how the declaration will be
    /// interpreted/generated, after the resolution step.
    ///
    /// TODO: mark reflection as an idea.
    fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<Attribute, FxBuildHasher>;

    /// Returns the visibility of the declaration. It does rule "who" can access this
    /// declaration.
    fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<Vis>;

    /// Returns the documentation of the declaration. It does rule how the declaration will be
    /// documented, and presented to others in their IDEs.
    fn docs(&self, db: &dyn crate::HirDb) -> Vec<DocString>;

    /// Returns the name of the declaration.
    fn name(&self, db: &dyn crate::HirDb) -> Definition;

    /// Returns the parameters of the declaration. It's not obligatory to have parameters, but
    /// it's obligatory to return a vector of parameters.
    ///
    /// If the declaration doesn't have parameters, it should return an empty vector.
    fn parameters(&self, db: &dyn crate::HirDb) -> Vec<Parameter>;

    /// Returns the type representation of the declaration. It's not obligatory to have a type
    /// representation, but it's obligatory to return an [`Option`] of type representation.
    ///
    /// The type representation is used to type check the declaration, and to infer the type of
    /// the declaration.
    fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep>;

    /// Upcasts the declaration to a [`DeclDescriptor`]. It's used to create a descriptor of
    /// the declaration, that can be used to create a [`Definition`].
    ///
    /// TODO: remove this method, and use the [`Definition`] directly.
    #[deprecated]
    fn upcast(&self, db: &dyn crate::HirDb) -> top_level::DeclDescriptor;
}

/// Defines an attribute for a declaration. It's used to rule how the declaration will be
/// interpreted/generated, after the resolution step.
///
/// TODO: mark reflection as an idea.
///
/// It does have a name, arguments, and a location.
#[salsa::tracked]
pub struct Attribute {
    pub name: HirPath,
    pub arguments: Vec<expr::Expr>,
    pub location: Location,
}

impl walking::Walker for Attribute {
    fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.name(db).accept(db, listener);
        self.arguments(db).accept(db, listener);
        self.location(db).accept(db, listener);
    }
}

impl DefaultWithDb for Attribute {
    fn default_with_db(db: &dyn crate::HirDb) -> Self {
        let name = default_with_db(db);

        Attribute::new(db, name, vec![], Location::call_site(db))
    }
}

/// Defines a documentation string for a declaration. It's used to rule how the declaration
/// will be documented, and presented to others in their IDEs.
///
/// It does have a range, and a location.
#[salsa::tracked]
pub struct DocString {
    pub range: Location,
}

impl walking::Walker for DocString {
    fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.range(db).accept(db, listener);
    }
}

/// Defines a visibility for a declaration. It's used to rule "who" can access this
/// declaration.
#[derive(Default, Clone, Hash, PartialEq, Eq, Debug)]
pub enum Vis {
    /// The declaration is public, and can be accessed by anyone.
    #[default]
    Public,

    /// The declaration is public, but if it's a trait, it can only be implemented by the
    /// package that defines it.
    Sealed,

    /// The declaration is private, and can only be accessed by the file that defines it.
    Private,

    /// The declaration is private to the package that defines it, and can only be accessed by
    /// the package that defines it.
    Internal,
}

impl walking::Walker for Spanned<Vis> {
    fn accept<T: walking::HirListener>(self, _db: &dyn crate::HirDb, _listener: &mut T) {}
}

/// Defines a parameter for a declaration. It's a function parameter, and it's used to type
/// a binding, and a type representation for the parameter.
///
/// It can be either implicit or explicit, and rigid or unrigid.
#[salsa::tracked]
pub struct Parameter {
    pub binding: pattern::Pattern,
    pub parameter_type: type_rep::TypeRep,

    /// Whether this parameter is implicit, i.e. it's a `forall` parameter.
    pub is_implicit: bool,

    /// Whether this parameter is rigid, if it's not rigid, it can be transformed into a
    /// an implicit parameter.
    pub rigid: bool,

    /// The level of the parameter. It's used to type check the parameter.
    pub level: HirLevel,

    pub location: Location,
}

impl Parameter {
    /// Creates a new explicit parameter with the given [`binding`], [`type_rep`], that
    /// is rigid.
    pub fn explicit(
        db: &dyn crate::HirDb,
        binding: pattern::Pattern,
        type_rep: type_rep::TypeRep,
        location: Location,
    ) -> Self {
        Self::new(db, binding, type_rep, false, true, HirLevel::Expr, location)
    }

    /// Creates a new implicit parameter with the given [`binding`], [`type_rep`], that
    /// is rigid.
    pub fn implicit(
        db: &dyn crate::HirDb,
        binding: pattern::Pattern,
        type_rep: type_rep::TypeRep,
        location: Location,
    ) -> Self {
        Self::new(db, binding, type_rep, true, true, HirLevel::Expr, location)
    }

    /// Creates a new explicit parameter with the given [`binding`], [`type_rep`], that
    /// is unrigid. It can be transformed into an implicit parameter, if the type signature
    /// requires it.
    pub fn unrigid(
        db: &dyn crate::HirDb,
        binding: pattern::Pattern,
        type_rep: type_rep::TypeRep,
        location: Location,
    ) -> Self {
        Self::new(
            db,
            /* binding     = */ binding,
            /* type_rep    = */ type_rep,
            /* is_implicit = */ false,
            /* rigid       = */ false,
            /* level       = */ HirLevel::Expr,
            /* location    = */ location,
        )
    }

    /// Creates a new unnamed and explicit parameter, it does have an empty binding, that is
    /// just like an *wildcard* pattern.
    ///
    /// This is useful for constructing parameters with just the types, and not the bindings.
    pub fn unnamed(db: &dyn crate::HirDb, type_rep: type_rep::TypeRep) -> Self {
        Self::new(
            db,
            /* binding     = */ pattern::Pattern::Hole,
            /* type_rep    = */ type_rep.clone(),
            /* is_implicit = */ false,
            /* rigid       = */ true,
            /* level       = */ HirLevel::Expr,
            /* location    = */ type_rep.location(db),
        )
    }
}

impl walking::Walker for Parameter {
    fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.binding(db).accept(db, listener);
        self.parameter_type(db).accept(db, listener);
        self.location(db).accept(db, listener);
    }
}

impl DefaultWithDb for Parameter {
    /// Creates a new unit parameter. Just like `(): ()`, for default and error recovery
    /// purposes.
    fn default_with_db(db: &dyn crate::HirDb) -> Self {
        let binding = pattern::Pattern::Hole;
        let type_rep = Expr::Type(Type::Unit, Location::CallSite).upgrade(db);
        let level = HirLevel::Expr;

        Self::new(
            db,
            /* binding     = */ binding,
            /* type_rep    = */ type_rep,
            /* is_implicit = */ false,
            /* rigid       = */ false,
            /* level       = */ level,
            /* location    = */ Location::call_site(db),
        )
    }
}
