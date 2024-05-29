//! Defines a kind of definition. The top level declarations are like statements for a file in HIR,
//! and they can maybe be referenced by other definitions.
//!
//! The other definitions are just like declarations, but they can't be referenced by other.

use core::panic;
use std::{fmt::Formatter, sync::Arc};

use fxhash::FxBuildHasher;

use super::*;
use crate::{
    errors::HirError,
    solver::{Definition, Reference},
    walking::HirListener,
};

/// Defines a top level declaration signature declaration. It's a declaration that can be
/// referenced by other definitions. And it can have clauses, that are the implementations of
/// the signature.
///
/// It does have attributes, visibility, documentation, name, parameters, return type, and
///
/// ## Examples
///
/// ```hs
/// f : String -> String
/// f x = x
/// ```
///
/// In this example, `f : ...` is a signature declaration, and `f x = x` is a clause.
#[salsa::tracked]
pub struct Signature {
    pub attributes: HashSet<declaration::Attribute, FxBuildHasher>,
    pub docs: Vec<declaration::DocString>,
    pub visibility: Spanned<declaration::Vis>,
    pub name: Definition,
    pub parameters: Vec<declaration::Parameter>,
    pub return_type: type_rep::TypeRep,
    pub location: Location,
}

impl walking::Walker for Signature {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.attributes(db).accept(db, listener);
        self.docs(db).accept(db, listener);
        self.visibility(db).accept(db, listener);
        self.name(db).accept(db, listener);
        self.parameters(db).accept(db, listener);
        self.return_type(db).accept(db, listener);
        self.location(db).accept(db, listener);
    }
}

impl declaration::Declaration for Signature {
    fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute, FxBuildHasher> {
        Self::attributes(*self, db)
    }

    fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
        Self::visibility(*self, db)
    }

    fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
        Self::docs(*self, db)
    }

    fn name(&self, db: &dyn crate::HirDb) -> Definition {
        Self::name(*self, db)
    }

    fn parameters(&self, db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
        Self::parameters(*self, db)
    }

    fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
        Self::return_type(*self, db).into()
    }

    fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
        panic!("Signature can't be upcasted")
    }
}

impl HirElement for Signature {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        Self::location(*self, db)
    }
}

/// Defines a top level declaration binding group. It's a declaration that can be referenced by
/// other definitions. And it can have a single signature and multiple clauses, that are the
/// implementations of the signature.
///
/// It does have attributes, visibility, documentation, name, parameters, return type, and
/// clauses.
///
/// ## Examples
/// ```hs
/// f x = x
/// ```
///
/// This is a clause, of a signature declaration. And the scope is the block scope
#[salsa::tracked]
pub struct Clause {
    pub name: Definition,
    pub arguments: Vec<pattern::Pattern>,
    pub value: expr::Expr,
    pub location: Location,
}

impl walking::Walker for Clause {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.name(db).accept(db, listener);
        self.arguments(db).accept(db, listener);
        self.value(db).accept(db, listener);
        self.location(db).accept(db, listener);
    }
}

impl HirElement for Clause {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        Self::location(*self, db)
    }
}

/// A Binding Group is the combination of a single signature and multiple clauses. It's a
/// declaration that can be referenced by other definitions.
///
/// It does have attributes, visibility, documentation, name, parameters, return type, and
/// clauses, that references the signature's ones.
///
/// ## Examples
/// ```hs
/// f : String -> String
/// f x = x
/// ```
#[salsa::tracked]
pub struct BindingGroup {
    pub signature: Signature,
    pub clauses: HashSet<Clause, FxBuildHasher>,
}

impl walking::Walker for BindingGroup {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        listener.enter_binding_top_level(self);
        self.signature(db).accept(db, listener);
        self.clauses(db).accept(db, listener);
        listener.exit_binding_top_level(self);
    }
}

impl declaration::Declaration for BindingGroup {
    fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute, FxBuildHasher> {
        self.signature(db).attributes(db)
    }

    fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
        self.signature(db).visibility(db)
    }

    fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
        self.signature(db).docs(db)
    }

    fn name(&self, db: &dyn crate::HirDb) -> Definition {
        self.signature(db).name(db)
    }

    fn parameters(&self, db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
        self.signature(db).parameters(db)
    }

    fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
        self.signature(db).type_rep(db)
    }

    fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
        top_level::DeclDescriptor::BindingGroup(*self)
    }
}

impl HirElement for BindingGroup {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        self.signature(db).location(db)
    }
}

/// Defines a "command-like" top level declaration. It does import a module, or use a top level
/// definition, into the file scope.
///
/// It does have location, and receives a [`Definition`] and a [`Location`].
#[salsa::tracked]
pub struct UsingTopLevel {
    pub path: Reference,
    pub location: Location,
}

impl walking::Walker for UsingTopLevel {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        listener.enter_using_top_level(self);
        self.path(db).accept(db, listener);
        self.location(db).accept(db, listener);
        listener.exit_using_top_level(self);
    }
}

impl HirElement for UsingTopLevel {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        Self::location(*self, db)
    }
}

/// Defines a "command-like" top level declaration. It does literally send a command to the
/// compiler, it's used to generate code, and to do other things that are not related to the
/// main language semantics.
///
/// It does have location, and receives a [`Definition`] and a [`Location`].
#[salsa::tracked]
pub struct CommandTopLevel {
    pub path: Definition,
    pub arguments: Vec<expr::Expr>,
    pub location: Location,
}

impl walking::Walker for CommandTopLevel {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        listener.enter_command_top_level(self);
        self.path(db).accept(db, listener);
        self.arguments(db).accept(db, listener);
        self.location(db).accept(db, listener);
        listener.exit_command_top_level(self);
    }
}

impl HirElement for CommandTopLevel {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        Self::location(*self, db)
    }
}

#[salsa::tracked]
pub struct Inductive {
    pub attributes: HashSet<declaration::Attribute, FxBuildHasher>,
    pub docs: Vec<declaration::DocString>,
    pub visibility: Spanned<declaration::Vis>,
    pub name: Definition,
    pub parameters: Vec<declaration::Parameter>,
    pub return_type: type_rep::TypeRep,
    pub variants: Vec<Constructor>,
    pub location: Location,
    pub scope: Arc<Scope>,
}

impl walking::Walker for Inductive {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        listener.enter_inductive_top_level(self);
        self.attributes(db).accept(db, listener);
        self.docs(db).accept(db, listener);
        self.visibility(db).accept(db, listener);
        self.name(db).accept(db, listener);
        self.parameters(db).accept(db, listener);
        self.return_type(db).accept(db, listener);
        self.variants(db).accept(db, listener);
        self.location(db).accept(db, listener);
        listener.exit_inductive_top_level(self);
    }
}

impl declaration::Declaration for Inductive {
    fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute, FxBuildHasher> {
        Self::attributes(*self, db)
    }

    fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
        Self::visibility(*self, db)
    }

    fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
        Self::docs(*self, db)
    }

    fn name(&self, db: &dyn crate::HirDb) -> Definition {
        Self::name(*self, db)
    }

    fn parameters(&self, db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
        Self::parameters(*self, db)
    }

    fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
        Self::return_type(*self, db).into()
    }

    fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
        top_level::DeclDescriptor::Inductive(*self)
    }
}

impl HirElement for Inductive {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        Self::location(*self, db)
    }
}

#[salsa::tracked]
pub struct Constructor {
    pub kind: ConstructorKind,
    pub attributes: HashSet<declaration::Attribute, FxBuildHasher>,
    pub docs: Vec<declaration::DocString>,
    pub name: Definition,
    pub return_type: type_rep::TypeRep,
    pub location: Location,
}

impl walking::Walker for Constructor {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.attributes(db).accept(db, listener);
        self.docs(db).accept(db, listener);
        self.name(db).accept(db, listener);
        self.return_type(db).accept(db, listener);
        self.location(db).accept(db, listener);
    }
}

impl declaration::Declaration for Constructor {
    fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute, FxBuildHasher> {
        Self::attributes(*self, db)
    }

    fn visibility(&self, _db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
        Spanned::on_call_site(declaration::Vis::Public)
    }

    fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
        Self::docs(*self, db)
    }

    fn name(&self, db: &dyn crate::HirDb) -> Definition {
        Self::name(*self, db)
    }

    fn parameters(&self, _db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
        Vec::new()
    }

    fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
        Self::return_type(*self, db).into()
    }

    fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
        panic!("Constructors can't be upcasted")
    }
}

impl HirElement for Constructor {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        Self::location(*self, db)
    }
}

/// Defines the style of a constructor. It can be either a function, or a Generalized Algebraic
/// Data Type. It's used to improve the type checking of the constructors.
#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum ConstructorKind {
    Function,

    /// Generalized Algebraic Data Type. It can be just like haskell GADTs:
    ///
    /// ```hs
    /// data Expr (a) {
    ///   Lit : (value: Int) -> Expr Int,
    ///   Add : (lhs: Expr Int) -> (rhs: Expr Int) -> Expr Int;
    /// }
    /// ```
    Gadt,
}

/// Defines a top level declaration. It can be either a command to the compiler or a
/// declaration, that holds a [`Definition`].
///
/// It can have recovery errors, that are used to recover from errors, and to continue the
/// parsing process.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TopLevel {
    Error(HirError),
    Using(UsingTopLevel),
    Command(CommandTopLevel),
    BindingGroup(BindingGroup),
    Inductive(Inductive),
}

impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for TopLevel {
    fn fmt(&self, f: &mut Formatter<'_>, db: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
        match self {
            TopLevel::Error(error) => Debug::fmt(error, f),
            TopLevel::Using(using) => using.debug_all(db).fmt(f),
            TopLevel::Command(command) => command.debug_all(db).fmt(f),
            TopLevel::BindingGroup(binding) => binding.debug_all(db).fmt(f),
            TopLevel::Inductive(data_decl) => data_decl.debug_all(db).fmt(f),
        }
    }
}

impl walking::Walker for TopLevel {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        match self {
            TopLevel::Error(_) => {}
            TopLevel::Using(using) => using.accept(db, listener),
            TopLevel::Command(command) => command.accept(db, listener),
            TopLevel::BindingGroup(binding) => binding.accept(db, listener),
            TopLevel::Inductive(data_decl) => data_decl.accept(db, listener),
        }
    }
}

impl HirElement for TopLevel {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        match self {
            Self::Error(downcast) => downcast.source_code.clone(),
            Self::Using(downcast) => downcast.location(db),
            Self::Command(downcast) => downcast.location(db),
            Self::BindingGroup(downcast) => downcast.location(db),
            Self::Inductive(downcast) => downcast.location(db),
        }
    }
}

/// Defines a descriptor of a top level declaration. It's used to create a [`Definition`]. It's
/// just like a [`TopLevel`], but it doesn't have the "command-like" top level declarations.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum DeclDescriptor {
    Error(HirError),
    BindingGroup(BindingGroup),
    Inductive(Inductive),
}

impl HirElement for DeclDescriptor {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        match self {
            Self::Error(downcast) => downcast.source_code.clone(),
            Self::BindingGroup(downcast) => downcast.location(db),
            Self::Inductive(downcast) => downcast.location(db),
        }
    }
}

/// A conversion from a [`TopLevel`] to a [`DeclDescriptor`]. It's used to create a descriptor
/// of the top level declaration, that can be used to create a [`Definition`].
impl TryFrom<DeclDescriptor> for TopLevel {
    type Error = ();

    fn try_from(value: DeclDescriptor) -> Result<Self, ()> {
        Ok(match value {
            DeclDescriptor::Error(downcast) => Self::Error(downcast),
            DeclDescriptor::BindingGroup(downcast) => Self::BindingGroup(downcast),
            DeclDescriptor::Inductive(downcast) => Self::Inductive(downcast),
        })
    }
}

/// A conversion from a [`DeclDescriptor`] to a [`TopLevel`]. It's used to create a top level
/// declaration, that can be used to create a [`Definition`].
impl TryFrom<TopLevel> for DeclDescriptor {
    type Error = ();

    fn try_from(value: TopLevel) -> Result<Self, ()> {
        Ok(match value {
            TopLevel::Error(downcast) => Self::Error(downcast),
            TopLevel::Using(_) => return Err(()),
            TopLevel::Command(_) => return Err(()),
            TopLevel::BindingGroup(downcast) => Self::BindingGroup(downcast),
            TopLevel::Inductive(downcast) => Self::Inductive(downcast),
        })
    }
}

/// Bridges the [`Declaration`] trait with the [`DeclDescriptor`] one. It's used to create a
/// descriptor of the declaration, that can be used to create a [`Definition`].
///
/// It's implementation functions just pattern match the [`DeclDescriptor`] variants, and
/// delegates the call to the [`Declaration`] trait.
impl declaration::Declaration for DeclDescriptor {
    fn attributes(&self, db: &dyn crate::HirDb) -> HashSet<declaration::Attribute, FxBuildHasher> {
        match self {
            Self::Error(_) => Default::default(),
            Self::BindingGroup(downcast) => downcast.attributes(db),
            Self::Inductive(downcast) => downcast.attributes(db),
        }
    }

    fn visibility(&self, db: &dyn crate::HirDb) -> Spanned<declaration::Vis> {
        match self {
            Self::Error(_) => Default::default(),
            Self::BindingGroup(downcast) => downcast.visibility(db),
            Self::Inductive(downcast) => downcast.visibility(db),
        }
    }

    fn docs(&self, db: &dyn crate::HirDb) -> Vec<declaration::DocString> {
        match self {
            Self::Error(_) => Default::default(),
            Self::BindingGroup(downcast) => downcast.docs(db),
            Self::Inductive(downcast) => downcast.docs(db),
        }
    }

    fn name(&self, db: &dyn crate::HirDb) -> Definition {
        match self {
            Self::Error(_) => default_with_db(db),
            Self::BindingGroup(downcast) => downcast.name(db),
            Self::Inductive(downcast) => downcast.name(db),
        }
    }

    fn parameters(&self, db: &dyn crate::HirDb) -> Vec<declaration::Parameter> {
        match self {
            Self::Error(_) => Default::default(),
            Self::BindingGroup(downcast) => downcast.parameters(db),
            Self::Inductive(downcast) => downcast.parameters(db),
        }
    }

    fn type_rep(&self, db: &dyn crate::HirDb) -> Option<type_rep::TypeRep> {
        match self {
            Self::Error(_) => Default::default(),
            Self::BindingGroup(downcast) => downcast.type_rep(db),
            Self::Inductive(downcast) => downcast.type_rep(db),
        }
    }

    fn upcast(&self, _db: &dyn crate::HirDb) -> top_level::DeclDescriptor {
        self.clone()
    }
}
