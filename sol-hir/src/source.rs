//! Defines the High-Level Intermediate Representation of the Sol language. It's intended to be
//! used to create a compiler for the Sol language.
//!
//! The HIR is exposing a object-oriented API, that can be used to create a compiler for the Sol

#![allow(clippy::too_many_arguments)]

use std::{
    collections::HashSet,
    fmt::{Debug, Display, Formatter},
    sync::Arc,
};

use sol_diagnostic::{Offset, TextRange};
use sol_syntax::Source;

use crate::{package::Package, reparse::reparse_hir_path, scope::Scope, walking};

pub trait OptionExt<T> {
    /// Returns the contained [`Some`] value or a default.
    ///
    /// Consumes the `self` argument then, if [`Some`], returns the contained
    /// value, otherwise if [`None`], returns the [default value] for that
    /// type.
    fn unwrap_or_default_with_db(self, db: &dyn crate::HirDb) -> T;
}

/// A trait for giving a type a useful default value.
///
/// Sometimes, you want to fall back to some kind of default value, and
/// don't particularly care what it is.
pub trait DefaultWithDb {
    /// Returns the "default value" for a type.
    ///
    /// Default values are often some kind of initial value, identity value, or anything else that
    /// may make sense as a default.
    fn default_with_db(db: &dyn crate::HirDb) -> Self;

    /// Returns a sentinel value for this type that signals that the value is
    /// not available.
    fn extra_data(db: &dyn crate::HirDb, location: Location) -> Self
    where
        Self: Sized,
    {
        let _ = location;

        Self::default_with_db(db)
    }

    /// Returns a sentinel value for this type that signals that the value is
    /// not available.
    fn incorrect_kind(db: &dyn crate::HirDb, kind: &str, location: Location) -> Self
    where
        Self: Sized,
    {
        let error = HirError::new(db, location, HirErrorKind::Kind(kind.into()));

        Self::error(db, error)
    }

    fn error(db: &dyn crate::HirDb, error: HirError) -> Self
    where
        Self: Sized,
    {
        let _ = error;

        Self::default_with_db(db)
    }
}

impl<T: DefaultWithDb> OptionExt<T> for Option<T> {
    /// Returns the contained [`Some`] value or a default.
    ///
    /// Consumes the `self` argument then, if [`Some`], returns the contained
    /// value, otherwise if [`None`], returns the [default value] for that
    /// type.
    fn unwrap_or_default_with_db(self, db: &dyn crate::HirDb) -> T {
        self.unwrap_or_else(|| T::default_with_db(db))
    }
}

impl<T: Default> DefaultWithDb for T {
    fn default_with_db(_db: &dyn crate::HirDb) -> Self {
        Self::default()
    }
}

/// Return the default value of a type according to the `DefaultWithDb` trait.
///
/// The type to return is inferred from context; this is equivalent to
/// `DefaultWithDb::default_with_db(db)` but shorter to type.
pub fn default_with_db<T: DefaultWithDb>(db: &dyn crate::HirDb) -> T {
    T::default_with_db(db)
}

/// A text range in a source file with a file name and the source text. This is the concrete
/// implementation of [`Location`].
///
/// [`Location`]: crate::Location
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct HirTextRange {
    pub source: Source,
    pub start: Offset,
    pub end: Offset,

    pub file_name: String,
    pub text: Arc<String>,
}

impl serde::Serialize for HirTextRange {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        #[derive(serde::Serialize)]
        struct HirTextRangeDelegate {
            pub start: Offset,
            pub end: Offset,
            pub file_name: String,
            pub text: String,
        }

        HirTextRangeDelegate {
            start: self.start,
            end: self.end,
            file_name: self.file_name.clone(),
            text: (*self.text).clone(),
        }
        .serialize(serializer)
    }
}

/// A location in a source file. It can be either a text range or a lazy location to be evaluated
/// in the `call_site`.
#[derive(Default, Clone, Hash, PartialEq, Eq, serde::Serialize)]
pub enum Location {
    /// A text range in a source file.
    TextRange(HirTextRange),

    #[default]
    CallSite,
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TextRange(_) => write!(f, "TextRange"),
            Self::CallSite => write!(f, "CallSite"),
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TextRange(range) => f
                .debug_struct("TextRange")
                .field("start", &range.start)
                .field("end", &range.end)
                .field("file_name", &range.file_name)
                .finish(),
            Self::CallSite => write!(f, "CallSite"),
        }
    }
}

#[salsa::input]
pub struct HirLocation {
    pub location: Location,
}

impl Location {
    /// Creates a new [Location] with the given [`source`] and range of [`start`] and [`end`].
    pub fn new<I>(db: &dyn crate::HirDb, src: Source, text: Arc<String>, start: I, end: I) -> Self
    where
        I: Into<Offset>,
    {
        Self::TextRange(HirTextRange {
            source: src,
            start: start.into(),
            end: end.into(),
            file_name: src.file_path(db).to_string_lossy().into_owned(),
            text,
        })
    }

    /// Creates a "call site" location, this is a location that is not resolved yet, and will be
    /// resolved later.
    ///
    /// It takes an [`db`] argument, just to be consistent with the other methods. And if sometime
    /// it will need the db argument, it will be already there, and we won't need to change the
    /// method signature, and the call sites.
    pub fn call_site(_db: &dyn crate::HirDb) -> Self {
        Self::CallSite
    }

    /// Sets the [`end`] of the location. It's useful when we don't know the end of the location
    /// when we create it, but we know it later.
    pub fn ending(self, end: Offset) -> Self {
        match self {
            Self::TextRange(mut range) => {
                range.end = end;
                Self::TextRange(range)
            }
            Self::CallSite => Self::CallSite,
        }
    }
}

impl walking::Walker for Location {
    fn accept<T: walking::HirListener>(self, _db: &dyn crate::HirDb, _listener: &mut T) {}
}

impl TextRange for Location {
    /// Returns the start offset of the source file.
    fn start(&self) -> Offset {
        match self {
            Location::TextRange(range) => range.start,
            Location::CallSite => Offset(0),
        }
    }

    /// Returns the end offset of the source file.
    fn end(&self) -> Offset {
        match self {
            Location::TextRange(range) => range.end,
            Location::CallSite => Offset(0),
        }
    }

    /// Returns the file name of the source file.
    fn file_name(&self) -> &str {
        match self {
            Location::TextRange(range) => &range.file_name,
            Location::CallSite => "unresolved",
        }
    }

    /// Returns the text of the source file.
    fn source(&self) -> &str {
        match self {
            Location::TextRange(range) => &range.text,
            Location::CallSite => "",
        }
    }
}

/// Defines an element of the High-Level Intermediate Representation. It's implemented for all
/// elements of the HIR.
pub trait HirElement {
    /// The range of the element in the source file.
    fn location(&self, db: &dyn crate::HirDb) -> Location;
}

/// A diagnostic error node for the HIR. It can be anything that can be reported to the diagnostic
/// database.
#[salsa::tracked]
pub struct HirError {
    /// The location of the error.
    pub location: Location,
    pub kind: HirErrorKind,
}

impl serde::Serialize for HirError {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str("HirError")
    }
}

/// The kind of the error. It can be anything that can be reported to the diagnostic database.
/// It's used to distinguish between different kinds of errors.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum HirErrorKind {
    ExtraData,
    Unknown,
    Kind(String),
}

impl walking::Walker for HirError {
    fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.location(db).accept(db, listener);
    }
}

/// Defines the tracking of a HIR source code file. It's the base struct of the HIR.
#[salsa::tracked]
pub struct HirSource {
    /// The source of the source file.
    pub source: Source,

    /// The package of the source file.
    pub package: Package,

    /// The scope of the source file. It's a [`Scope`] that contains all the definitions of the
    /// source file.
    ///
    /// It's useful for searching for definitions, and for resolving names.
    pub scope: Scope,

    /// The resolved top level definitions of the source file.
    #[return_ref]
    pub contents: Vec<top_level::TopLevel>,
}

impl walking::Walker for HirSource {
    fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.contents(db).clone().accept(db, listener);
    }
}

/// A unresolved path in the HIR. It's used to represent a path that is not resolved yet, and will
/// be resolved as a [`Definition`] later.
///
/// It does take segments and location.
#[salsa::input]
pub struct HirPath {
    /// The location of the path.
    pub location: Location,

    #[return_ref]
    pub segments: Vec<Identifier>,
}

impl HirPath {
    /// Creates a new artificial path. It's used to create a path from a string, and it's used for
    /// diagnostics.
    ///
    /// It takes a [`db`] argument, just to be consistent with the other methods. And if sometime
    /// it will need the db argument, it will be already there, and we won't need to change the
    /// method signature, and the call sites.
    ///
    /// It does calls the [`new_path`] query, that is the actual implementation of the method. It's
    /// only a bridge between the query and the method.
    pub fn create(db: &dyn crate::HirDb, text: &str) -> Self {
        let input = VirtualPath::new(db, text.into());

        new_path(db, input)
    }
}

#[salsa::input]
pub struct VirtualPath {
    pub path: String,
}

/// Creates a new artificial path. It's used to create a path from a string, and it's used for
/// diagnostics.
#[salsa::tracked]
pub fn new_path(db: &dyn crate::HirDb, new_name: VirtualPath) -> HirPath {
    let base = HirLocation::new(db, Location::CallSite);
    let segments = reparse_hir_path(db, base, new_name.path(db));

    HirPath::new(db, Location::call_site(db), segments)
}

#[salsa::tracked]
impl HirPath {
    /// Dumps to string the path. It's used to create a string representation of the path for human
    /// readable diagnostics.
    ///
    /// It's just like `a.b.c`, for example.
    #[salsa::tracked]
    pub fn to_string(self, db: &dyn crate::HirDb) -> Option<String> {
        // If the path is empty, it's not a valid path. So, we return `None`.
        if self.segments(db).is_empty() {
            return None;
        }

        self.segments(db)
            .iter()
            .map(|segment| segment.contents(db))
            .collect::<Vec<_>>()
            .join(".")
            .into()
    }
}

impl walking::Walker for HirPath {
    fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.location(db).accept(db, listener);
        self.segments(db).clone().accept(db, listener);
    }
}

impl DefaultWithDb for HirPath {
    fn default_with_db(db: &dyn crate::HirDb) -> Self {
        Self::new(db, Location::call_site(db), vec![])
    }
}

/// A segment of a [`HirPath`]. It's used to represent a segment of a path, that can be either a
/// symbol or a identifier.
///
/// Both have string contents, and a location.
#[salsa::tracked]
pub struct Identifier {
    pub contents: String,
    pub refers_symbol: bool,

    /// The location of the identifier.
    pub location: Location,
}

impl walking::Walker for Identifier {
    fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        self.location(db).accept(db, listener);
    }
}

impl Identifier {
    /// Creates a new identifier with the given [`contents`] and [`location`].
    pub fn symbol(db: &dyn crate::HirDb, contents: &str, location: Location) -> Self {
        Self::new(db, contents.into(), true, location)
    }
}

/// Represents contents within a location. It's used to represent a generic value that can have a
/// location too. Just like a tuple.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Spanned<T> {
    /// The value of the spanned value.
    pub value: T,

    /// The location of the value. It can be [`None`] if the value is a call site.
    ///
    /// TODO: change directly to `Location`, as it have a `CallSite` variant now.
    pub location: Option<Location>,
}

impl<T> Spanned<T> {
    /// Creates a new spanned value with the given [`value`] and the location pointing to the
    /// `call_site`.
    pub fn on_call_site(value: T) -> Self {
        Self {
            value,
            location: None,
        }
    }

    /// Creates a new spanned value with the given [`value`] and [`location`].
    pub fn new(value: T, location: Location) -> Self {
        Self {
            value,
            location: Some(location),
        }
    }
}

impl<A> HirElement for Spanned<A> {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        self.location
            .clone()
            .unwrap_or_else(|| Location::call_site(db))
    }
}

/// A spanned value that can be created with the [`Default`] trait. Pointing to the `call_site`.
impl<T: Default> Default for Spanned<T> {
    fn default() -> Self {
        Self {
            value: Default::default(),
            location: None,
        }
    }
}
impl<T> salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for Spanned<T>
where
    T: for<'a> salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'a>>::DynDb>,
{
    fn fmt(&self, f: &mut Formatter<'_>, db: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
        self.value.debug_all(db).fmt(f)
    }
}

/// Defines a kind of definition. The declarations are definitions that can have a name, and can be
/// referenced by other definitions.
///
/// The other definitions are just like declarations, but they can't be referenced by other.
pub mod declaration {
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
}

/// Defines a kind of definition. The top level declarations are like statements for a file in HIR,
/// and they can maybe be referenced by other definitions.
///
/// The other definitions are just like declarations, but they can't be referenced by other.
pub mod top_level {
    use core::panic;
    use std::{fmt::Formatter, sync::Arc};

    use fxhash::FxBuildHasher;

    use super::*;
    use crate::{
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
        fn attributes(
            &self,
            db: &dyn crate::HirDb,
        ) -> HashSet<declaration::Attribute, FxBuildHasher> {
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
        fn attributes(
            &self,
            db: &dyn crate::HirDb,
        ) -> HashSet<declaration::Attribute, FxBuildHasher> {
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
        fn attributes(
            &self,
            db: &dyn crate::HirDb,
        ) -> HashSet<declaration::Attribute, FxBuildHasher> {
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
        fn attributes(
            &self,
            db: &dyn crate::HirDb,
        ) -> HashSet<declaration::Attribute, FxBuildHasher> {
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
    #[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
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
                TopLevel::Error(error) => write!(f, "Error({:?})", error.debug_all(db)),
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
                TopLevel::Error(error) => {
                    listener.enter_error_top_level(error);
                    error.accept(db, listener);
                    listener.exit_error_top_level(error);
                }
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
                Self::Error(downcast) => downcast.location(db),
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
                Self::Error(downcast) => downcast.location(db),
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
        fn attributes(
            &self,
            db: &dyn crate::HirDb,
        ) -> HashSet<declaration::Attribute, FxBuildHasher> {
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
}

/// Defines a kind of terms. It does define patterns that can be used in clauses and pattern
/// matching agains't a value.
///
/// It can be known as the name of eliminating a value.
pub mod pattern {
    use std::fmt::Formatter;

    use super::*;
    use crate::{
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
            self.name(db).accept(db, listener);
            self.arguments(db).accept(db, listener);
            self.location(db).accept(db, listener);
            listener.exit_constructor_pattern(self);
        }
    }

    impl ConstructorPattern {
        pub fn new(
            _: &dyn crate::HirDb,
            name: Constructor,
            arguments: Vec<Pattern>,
            location: Location,
        ) -> ConstructorPattern {
            ConstructorPattern {
                name,
                arguments,
                location,
            }
        }

        pub fn name(&self, _db: &dyn crate::HirDb) -> Constructor {
            self.name.clone()
        }

        pub fn arguments(&self, _db: &dyn crate::HirDb) -> Vec<Pattern> {
            self.arguments.clone()
        }

        pub fn location(&self, _db: &dyn crate::HirDb) -> Location {
            self.location.clone()
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

    impl BindingPattern {
        pub fn new(_: &dyn crate::HirDb, name: Definition, location: Location) -> Self {
            Self { name, location }
        }

        pub fn name(&self, _db: &dyn crate::HirDb) -> Definition {
            self.name
        }

        pub fn location(&self, _db: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl walking::Walker for BindingPattern {
        fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            listener.enter_binding_pattern(self.clone());
            self.name(db).accept(db, listener);
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
                Pattern::Error(error) => write!(f, "Error({:?})", error.debug_all(db)),
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
                Pattern::Error(error) => {
                    listener.enter_error_pattern(error);
                    error.accept(db, listener);
                    listener.exit_error_pattern(error);
                }
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
                Self::Error(downcast) => downcast.location(db),
                Self::Constructor(downcast) => downcast.location(db),
                Self::Binding(downcast) => downcast.location(db),
            }
        }
    }
}

/// Defines a kind of statements. It does define statements that can be used in a block, and
/// "do-notations", that are used to do imperative code in a functional language.
pub mod stmt {
    use std::{fmt::Formatter, sync::Arc};

    use super::*;
    use crate::{scope::ScopeKind, walking::HirListener};

    /// Defines a ask statement, it will bind the value to a pattern, and it will return the value
    /// of the pattern.
    ///
    /// It's a sugar for a (`>>=`) function application. In the first versions of this language,
    /// there will not exist the function to desugar, but in the future, it will be implemented.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub struct AskStmt {
        pub pattern: pattern::Pattern,
        pub value: expr::Expr,
        pub location: Location,
    }

    impl AskStmt {
        pub fn new(
            _: &dyn crate::HirDb,
            pattern: pattern::Pattern,
            value: expr::Expr,
            location: Location,
        ) -> Self {
            Self {
                pattern,
                value,
                location,
            }
        }

        pub fn pattern(&self, _db: &dyn crate::HirDb) -> pattern::Pattern {
            self.pattern.clone()
        }

        pub fn value(&self, _db: &dyn crate::HirDb) -> expr::Expr {
            self.value.clone()
        }

        pub fn location(&self, _db: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl walking::Walker for AskStmt {
        fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            listener.enter_ask_stmt(self.clone());
            self.pattern(db).accept(db, listener);
            self.value(db).accept(db, listener);
            self.location(db).accept(db, listener);
            listener.exit_ask_stmt(self);
        }
    }

    impl HirElement for AskStmt {
        fn location(&self, _: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for AskStmt {
        fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    /// Defines a let binding statement, it will bind the value to a pattern, and it will return
    /// the value of the pattern. It's a sugar to a function application, and it's just like a
    /// [`AskStmt`], but it's not a sugar to binding the "do-notation".
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub struct LetStmt {
        pub pattern: pattern::Pattern,
        pub value: expr::Expr,
        pub location: Location,
    }

    impl LetStmt {
        pub fn new(
            _: &dyn crate::HirDb,
            pattern: pattern::Pattern,
            value: expr::Expr,
            location: Location,
        ) -> Self {
            Self {
                pattern,
                value,
                location,
            }
        }

        pub fn pattern(&self, _db: &dyn crate::HirDb) -> pattern::Pattern {
            self.pattern.clone()
        }

        pub fn value(&self, _db: &dyn crate::HirDb) -> expr::Expr {
            self.value.clone()
        }

        pub fn location(&self, _db: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl walking::Walker for LetStmt {
        fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            listener.enter_let_stmt(self.clone());
            self.pattern(db).accept(db, listener);
            self.value(db).accept(db, listener);
            self.location(db).accept(db, listener);
            listener.exit_let_stmt(self);
        }
    }

    impl HirElement for LetStmt {
        fn location(&self, _: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for LetStmt {
        fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    /// Defines the statement element in the HIR.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Stmt {
        Empty,
        Error(HirError),
        Ask(AskStmt),
        Let(LetStmt),
        Downgrade(expr::Expr),
    }

    impl DefaultWithDb for Stmt {
        fn default_with_db(_db: &dyn crate::HirDb) -> Self {
            Self::Empty
        }

        fn error(_: &dyn crate::HirDb, error: HirError) -> Self {
            Self::Error(error)
        }
    }

    impl walking::Walker for Stmt {
        fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            match self {
                Stmt::Empty => listener.visit_empty_stmt(),
                Stmt::Ask(ask_stmt) => ask_stmt.accept(db, listener),
                Stmt::Let(let_stmt) => let_stmt.accept(db, listener),
                Stmt::Error(error) => {
                    listener.enter_error_stmt(error);
                    error.accept(db, listener);
                    listener.exit_error_stmt(error);
                }
                Stmt::Downgrade(expr) => {
                    listener.enter_downgrade_stmt(expr.clone());
                    expr.clone().accept(db, listener);
                    listener.exit_downgrade_stmt(expr);
                }
            }
        }
    }
    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for Stmt {
        fn fmt(&self, f: &mut Formatter<'_>, db: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            match self {
                Stmt::Empty => write!(f, "Empty"),
                Stmt::Error(error) => write!(f, "Error({:?})", error.debug_all(db)),
                Stmt::Ask(ask_stmt) => ask_stmt.debug_all(db).fmt(f),
                Stmt::Let(let_stmt) => let_stmt.debug_all(db).fmt(f),
                Stmt::Downgrade(expr) => expr.debug_all(db).fmt(f),
            }
        }
    }

    impl HirElement for Stmt {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Empty => Location::call_site(db),
                Self::Error(downcast) => downcast.location(db),
                Self::Ask(downcast) => downcast.location(db),
                Self::Let(downcast) => downcast.location(db),
                Self::Downgrade(downcast) => (*downcast).location(db),
            }
        }
    }

    /// Represents a list of statements within a return value of a block. It's representing a
    /// "do-notation" code block.
    ///
    /// It can be either a last function's arguments or a function body.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub struct Block {
        pub statements: Vec<Stmt>,
        pub location: Location,
        pub scope: Arc<Scope>,
    }

    impl walking::Walker for Block {
        fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            listener.enter_block(self.clone());
            self.statements(db).accept(db, listener);
            self.location(db).accept(db, listener);
            listener.exit_block(self);
        }
    }

    impl DefaultWithDb for Block {
        fn default_with_db(db: &dyn crate::HirDb) -> Self {
            let scope = Scope::new_ref(ScopeKind::Block);

            Self::new(db, vec![], Location::call_site(db), scope)
        }
    }

    impl Block {
        pub fn new(
            _: &dyn crate::HirDb,
            statements: Vec<Stmt>,
            location: Location,
            scope: Arc<Scope>,
        ) -> Self {
            Self {
                statements,
                location,
                scope,
            }
        }

        pub fn statements(&self, _: &dyn crate::HirDb) -> Vec<Stmt> {
            self.statements.clone()
        }

        pub fn scope(&self, _db: &dyn crate::HirDb) -> Arc<Scope> {
            self.scope.clone()
        }

        pub fn location(&self, _db: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl HirElement for Block {
        fn location(&self, _: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for Block {
        fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }
}

/// Defines a kind of primaries. It does define terms that are literally literals, and it's used
/// as numbers, strings, etc... These are the base of the base of the base of the language
pub mod literal {
    use super::*;
    use crate::walking::HirListener;

    /// Defines a literal element in the HIR.
    #[derive(Default, Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Literal {
        #[default]
        Empty,

        Int8(i8),
        UInt8(u8),
        Int16(i16),
        UInt16(u16),
        Int32(i32),
        UInt32(u32),
        Int64(i64),
        UInt64(u64),

        /// Defines a string literal. It's used to represent a string value.
        String(String),

        /// Defines a boolean literal. It's used to represent a boolean value.
        Boolean(bool),

        /// Defines a character literal. It's used to represent a character value.
        Char(char),
    }

    impl walking::Walker for Spanned<Literal> {
        fn accept<T: HirListener>(self, _db: &dyn crate::HirDb, _listener: &mut T) {}
    }

    impl Literal {
        /// Defines the false literal. It's used to represent a boolean value.
        pub const FALSE: Literal = Literal::Boolean(false);
        /// Defines the true literal. It's used to represent a boolean value.
        pub const TRUE: Literal = Literal::Boolean(true);

        /// Creates a literal pattern from a literal. It's used to create a pattern from a
        /// literal.
        /// It's not currently supported by the language, but it will be in the future. So the
        /// compiler will emit an error.
        pub fn upgrade_pattern(self, loc: Location, _db: &dyn crate::HirDb) -> pattern::Pattern {
            pattern::Pattern::Literal(Spanned::new(self, loc))
        }

        /// Creates a literal expression from a literal. It's used to create a expression from a
        /// literal.
        pub fn upgrade_expr(self, loc: Location, _db: &dyn crate::HirDb) -> expr::Expr {
            expr::Expr::Literal(Spanned::new(self, loc))
        }
    }
}

/// Defines a kind of terms. It does define expressions that can be used in a block. These are the
/// base of the language grammar and semantics.
pub mod expr {
    use std::{fmt::Formatter, sync::Arc};

    use sol_diagnostic::{message, Diagnostics, ErrorId, Report};

    use super::*;
    use crate::{
        primitives::primitive_type_rep,
        solver::{HirDiagnostic, Reference},
    };

    /// Defines a kind of match. It's used to define the kind of a match expression, and to improve
    /// pretty printing, and showing assists on the IDE or CLI.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum MatchKind {
        If,
        Match,

        /// If it's like an "If statement" instead of an "If expression", it will be a statement
        /// level match, and it will not return a value.
        StmtLevel(Box<MatchKind>),
    }

    /// Represents the value a call expression is calling. It can be either a definition, or an
    /// expression. Or it can be a special value, like a tuple, an array, or a unit.
    ///
    /// It's used to improve the type checking of the call expressions.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Callee {
        Array,
        Tuple,
        Unit,
        Pure,
        Do,
        Reference(Reference),
        Expr(Box<expr::Expr>),
    }

    impl walking::Walker for Callee {
        fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            match self {
                Callee::Array => {}
                Callee::Tuple => {}
                Callee::Unit => {}
                Callee::Pure => {}
                Callee::Do => {}
                Callee::Reference(reference) => reference.accept(db, listener),
                Callee::Expr(expr) => expr.accept(db, listener),
            }
        }
    }

    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for Callee {
        fn fmt(&self, f: &mut Formatter<'_>, db: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            use salsa::DebugWithDb;

            match self {
                Callee::Array => write!(f, "Array"),
                Callee::Tuple => write!(f, "Tuple"),
                Callee::Unit => write!(f, "()"),
                Callee::Pure => write!(f, "return"),
                Callee::Do => write!(f, "do"),
                Callee::Reference(reference) => DebugWithDb::fmt(reference, f, db, true),
                Callee::Expr(expr) => DebugWithDb::fmt(expr, f, db, true),
            }
        }
    }

    /// Defines a kind of call. It's used to define the kind of a call expression, and to improve
    /// pretty printing.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum CallKind {
        Error,
        Infix,
        Prefix,
    }

    /// Represents a lambda abstraction expression, it's always curried, and it's used to define
    /// functions.
    ///
    /// As it is curried, it does mean that the parameters are in the reality alone, and the body
    /// is a function that receives the parameters, and returns a value.
    ///
    /// This is called Abs as a short for Abstraction, because lambda abstraction is the main
    /// principle of abstraction in a functional language.
    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    pub struct AbsExpr {
        pub parameters: Vec<pattern::Pattern>,
        pub value: Box<expr::Expr>,
        pub location: Location,
        pub scope: Arc<Scope>,
    }

    impl walking::Walker for AbsExpr {
        fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            listener.enter_abs_expr(self.clone());
            self.parameters(db).accept(db, listener);
            self.value(db).accept(db, listener);
            self.location(db).accept(db, listener);
            listener.exit_abs_expr(self);
        }
    }

    impl AbsExpr {
        pub fn new(
            _: &dyn crate::HirDb,
            parameters: Vec<pattern::Pattern>,
            value: expr::Expr,
            location: Location,
            scope: Arc<Scope>,
        ) -> Self {
            Self {
                parameters,
                value: Box::new(value),
                location,
                scope,
            }
        }

        pub fn parameters(&self, _db: &dyn crate::HirDb) -> Vec<pattern::Pattern> {
            self.parameters.clone()
        }

        pub fn value(&self, _db: &dyn crate::HirDb) -> Expr {
            (*self.value).clone()
        }

        pub fn location(&self, _db: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }

        pub fn scope(&self, _db: &dyn crate::HirDb) -> Arc<Scope> {
            self.scope.clone()
        }
    }

    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for AbsExpr {
        fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    impl HirElement for AbsExpr {
        fn location(&self, _: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    /// Represents an annotation expression, it works just like a cast operator, but in the type
    /// system, the type system will try to "cast", and if it's unsound, it will report an error.
    ///
    /// It's used to improve the type checking of the expressions.
    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    pub struct AnnExpr {
        pub value: Box<expr::Expr>,
        pub type_rep: type_rep::TypeRep,
        pub location: Location,
    }

    impl walking::Walker for AnnExpr {
        fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            listener.enter_ann_expr(self.clone());
            self.value(db).accept(db, listener);
            self.type_rep(db).accept(db, listener);
            self.location(db).accept(db, listener);
            listener.exit_ann_expr(self);
        }
    }

    impl AnnExpr {
        pub fn new(
            _: &dyn crate::HirDb,
            value: expr::Expr,
            type_rep: type_rep::TypeRep,
            location: Location,
        ) -> Self {
            Self {
                value: Box::new(value),
                type_rep,
                location,
            }
        }

        pub fn value(&self, _db: &dyn crate::HirDb) -> Expr {
            (*self.value).clone()
        }

        pub fn type_rep(&self, _db: &dyn crate::HirDb) -> type_rep::TypeRep {
            self.type_rep.clone()
        }

        pub fn location(&self, _db: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl HirElement for AnnExpr {
        fn location(&self, _: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for AnnExpr {
        fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    /// Represents a match arm. It's used to define a match expression, and to improve the type
    /// checking of the match expressions.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub struct MatchArm {
        /// The leftmost pattern of the match arm. It's used to match agains't the value.
        pub pattern: pattern::Pattern,

        /// The rightmost expression of the match arm. It's used to return a value.
        pub value: expr::Expr,

        pub location: Location,
    }

    impl walking::Walker for MatchArm {
        fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            self.pattern.accept(db, listener);
            self.value.accept(db, listener);
            self.location.accept(db, listener);
        }
    }

    impl HirElement for MatchArm {
        fn location(&self, _db: &dyn crate::HirDb) -> Location {
            // As it's not tracked by salsa, it's not possible to get the location from the salsa
            // database, so it's just returning the location of the match arm, cloning it
            self.location.clone()
        }
    }

    /// Represents a match expression. It's used to match agains't a value, and to return a value
    /// based on the match.
    ///
    /// It's used to improve the type checking of the expressions.
    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    pub struct MatchExpr {
        pub kind: MatchKind,
        pub scrutinee: Box<expr::Expr>,
        pub clauses: Vec<MatchArm>,
        pub location: Location,
    }

    impl walking::Walker for MatchExpr {
        fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            listener.enter_match_expr(self.clone());
            self.scrutinee(db).accept(db, listener);
            self.clauses(db).accept(db, listener);
            self.location(db).accept(db, listener);
            listener.exit_match_expr(self);
        }
    }

    impl MatchExpr {
        pub fn new(
            _: &dyn crate::HirDb,
            kind: MatchKind,
            scrutinee: expr::Expr,
            clauses: Vec<MatchArm>,
            location: Location,
        ) -> Self {
            Self {
                kind,
                scrutinee: Box::new(scrutinee),
                clauses,
                location,
            }
        }

        pub fn scrutinee(&self, _db: &dyn crate::HirDb) -> Expr {
            (*self.scrutinee).clone()
        }

        pub fn clauses(&self, _db: &dyn crate::HirDb) -> Vec<MatchArm> {
            self.clauses.clone()
        }

        pub fn location(&self, _db: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl HirElement for MatchExpr {
        fn location(&self, _: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for MatchExpr {
        fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    /// Represents a call expression, or a function application, or anything like this... It's used
    /// to call a [`Callee`], or a value that can be called.
    ///
    /// It does holds the kind of the call to improve the pretty printing.
    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    pub struct CallExpr {
        pub kind: CallKind,
        pub callee: Callee,
        pub arguments: Vec<expr::Expr>,

        /// The do-notation is a syntax sugar for a block, so it's possible to have a block as the
        /// last parameter of a function, and it will be used as the do-notation. It's inspired on
        /// Kotlin's syntax sugar for lambdas.
        pub do_notation: Option<stmt::Block>,

        pub location: Location,
    }

    impl walking::Walker for CallExpr {
        fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            listener.enter_call_expr(self.clone());
            self.callee(db).accept(db, listener);
            self.arguments(db).accept(db, listener);
            self.do_notation(db).accept(db, listener);
            self.location(db).accept(db, listener);
            listener.exit_call_expr(self);
        }
    }

    impl CallExpr {
        pub fn new(
            _: &dyn crate::HirDb,
            kind: CallKind,
            callee: Callee,
            arguments: Vec<expr::Expr>,
            do_notation: Option<stmt::Block>,
            location: Location,
        ) -> Self {
            Self {
                kind,
                callee,
                arguments,
                do_notation,
                location,
            }
        }

        pub fn callee(&self, _db: &dyn crate::HirDb) -> Callee {
            self.callee.clone()
        }

        pub fn arguments(&self, _db: &dyn crate::HirDb) -> Vec<Expr> {
            self.arguments.clone()
        }

        pub fn do_notation(&self, _db: &dyn crate::HirDb) -> Option<stmt::Block> {
            self.do_notation.clone()
        }

        pub fn location(&self, _db: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl HirElement for CallExpr {
        fn location(&self, _: &dyn crate::HirDb) -> Location {
            self.location.clone()
        }
    }

    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for CallExpr {
        fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    /// Defines a type representation. It's used to define a type that can
    /// be used in the type level
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Type {
        Unit,
        String,
        Bool,
        Int8,
        UInt8,
        Int16,
        UInt16,
        Int32,
        UInt32,
        Int64,
        UInt64,
        Nat,
    }

    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    pub struct Pi {
        pub parameters: Vec<pattern::Pattern>,
        pub value: Box<type_rep::TypeRep>,
        pub location: Location,
    }

    impl walking::Walker for Pi {
        fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            listener.enter_pi(self.clone());
            self.parameters.clone().accept(db, listener);
            self.value.clone().accept(db, listener);
            self.location.clone().accept(db, listener);
            listener.exit_pi(self);
        }
    }

    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for Pi {
        fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            Debug::fmt(self, f)
        }
    }

    /// Defines the expression element in the HIR. It's the most important element in the HIR, and
    /// in the language itself, as it's defines instructions that can be executed, and values that
    /// can be used.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum Expr {
        /// The empty expression should work as a sentinele value, it's just like a `()` value. But
        /// it's splitted into a different variant to make it easier to work with, when we don't
        /// have an actual expression, like in [`Self::Error`].
        Empty,
        Type(Type, Location),
        Error(HirError),
        Path(Reference),
        Literal(Spanned<literal::Literal>),
        Call(CallExpr),
        Ann(AnnExpr),
        Abs(AbsExpr),
        Match(MatchExpr),
        Pi(Pi),
        Sigma(Pi),
    }

    impl Expr {
        /// Creates a unit expression. It's used to create a unit expression, that is just like a
        /// `()` value.
        pub fn call_unit_expr(location: Location, db: &dyn crate::HirDb) -> Self {
            Self::Call(CallExpr::new(
                db,
                /* kind        = */ CallKind::Prefix,
                /* callee      = */ Callee::Unit,
                /* arguments   = */ vec![],
                /* do_notation = */ None,
                /* location    = */ location,
            ))
        }

        /// Creates a block do-notation expression. It's used to create a block expression, that is
        /// just like a `do { }` value.
        pub fn block(db: &dyn crate::HirDb, do_notation: stmt::Block) -> Self {
            Self::Call(CallExpr::new(
                db,
                /* kind        = */ CallKind::Prefix,
                /* callee      = */ Callee::Do,
                /* arguments   = */ vec![],
                /* do_notation = */ Some(do_notation.clone()),
                /* location    = */ do_notation.location(db),
            ))
        }

        /// Upgrades this expression to a type representation. This is useful for error recovery and
        /// future dependent types or refinement types integration.
        ///
        /// This function also reports an error currently, because it's not allowed dependent types
        /// on the language, this is the reason because it's good to error recovery.
        pub fn upgrade(self, db: &dyn crate::HirDb) -> type_rep::TypeRep {
            fn find_or_primitive_path(db: &dyn crate::HirDb, path: Reference) -> type_rep::TypeRep {
                primitive_type_rep(db, path.definition(db).name(db)).unwrap_or_else(|| {
                    type_rep::TypeRep {
                        expr: Box::new(Expr::Path(path)),
                    }
                })
            }

            type_rep::TypeRep {
                expr: Box::new(match self {
                    // Upgrades a path to a type representation. It does not require an
                    // error report, because it's not an error.
                    //
                    // It tries to find a primitive bound to the path, and if it does not find it,
                    // it will downgrade the type representation to a path.
                    Self::Path(path) => return find_or_primitive_path(db, path),

                    // Upgrades a call expression to a type
                    // representation.
                    Self::Call(CallExpr {
                        callee: Callee::Unit,
                        do_notation: Option::None,
                        arguments,
                        location,
                        kind: _,
                    }) if arguments.is_empty() => Expr::Type(Type::Unit, location),

                    // Upgrades a group expression to a type
                    Self::Call(CallExpr {
                        callee: Callee::Tuple,
                        do_notation: None,
                        arguments,
                        location: _,
                        kind: _,
                    }) if arguments.len() == 1 => {
                        return arguments.first().cloned().unwrap().upgrade(db)
                    }

                    // Upgrades application into type application
                    Self::Call(CallExpr {
                        callee: Callee::Reference(reference),
                        do_notation: None,
                        arguments,
                        location,
                        kind,
                    }) => Self::Call(CallExpr {
                        // Create dummy path type reference
                        callee: Callee::Expr(find_or_primitive_path(db, reference).expr),
                        arguments: arguments
                            .into_iter()
                            .map(|argument| *argument.upgrade(db).expr)
                            .collect(),
                        do_notation: None,
                        location,
                        kind,
                    }),

                    // Upgrades application into type application
                    Self::Call(CallExpr {
                        callee: Callee::Expr(expr),
                        do_notation: Option::None,
                        arguments,
                        location,
                        kind,
                    }) => Self::Call(CallExpr {
                        callee: Callee::Expr(expr.upgrade(db).expr),
                        arguments: arguments
                            .into_iter()
                            .map(|argument| *argument.upgrade(db).expr)
                            .collect(),
                        do_notation: None,
                        location,
                        kind,
                    }),

                    // TODO: report error
                    _ => self,
                }),
            }
        }
    }

    impl DefaultWithDb for Expr {
        /// The default expression is `Empty`. But it's not allowed to be used in any
        /// contexts, so this function should report it as an error, and return `Empty`. For better
        /// error reporting, the location of the `Empty` expression should be the same as
        /// the location of the context where it's used.
        fn default_with_db(db: &dyn crate::HirDb) -> Self {
            Diagnostics::push(
                db,
                Report::new(HirDiagnostic {
                    message: message![
                        "Empty expression representation is not allowed to be used in any contexts",
                    ],
                    id: ErrorId("empty-expression"),
                    location: Location::call_site(db),
                }),
            );

            Self::Empty
        }

        fn error(_db: &dyn crate::HirDb, error: HirError) -> Self {
            Self::Error(error)
        }
    }

    impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for Expr {
        fn fmt(&self, f: &mut Formatter<'_>, db: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
            match self {
                Expr::Empty => write!(f, "Empty"),
                Expr::Error(error) => write!(f, "Error({:?})", error.debug_all(db)),
                Expr::Literal(literal) => write!(f, "Literal({literal:?})"),
                Expr::Path(reference) => reference.debug_all(db).fmt(f),
                Expr::Call(call_expr) => call_expr.debug_all(db).fmt(f),
                Expr::Ann(ann_expr) => ann_expr.debug_all(db).fmt(f),
                Expr::Abs(abs_expr) => abs_expr.debug_all(db).fmt(f),
                Expr::Match(match_expr) => match_expr.debug_all(db).fmt(f),
                Expr::Type(type_ref, _) => write!(f, "Type({:?})", type_ref),
                Expr::Pi(pi) => pi.debug_all(db).fmt(f),
                Expr::Sigma(pi) => pi.debug_all(db).fmt(f),
            }
        }
    }

    impl walking::Walker for Expr {
        fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            match self {
                Expr::Empty => listener.visit_empty_expr(),
                Expr::Call(call_expr) => call_expr.accept(db, listener),
                Expr::Ann(ann_expr) => ann_expr.accept(db, listener),
                Expr::Abs(abs_expr) => abs_expr.accept(db, listener),
                Expr::Match(match_expr) => match_expr.accept(db, listener),
                Expr::Error(error) => {
                    listener.enter_error_expr(error);
                    error.accept(db, listener);
                    listener.exit_error_expr(error);
                }
                Expr::Path(path) => {
                    listener.enter_path_expr(path);
                    path.accept(db, listener);
                    listener.exit_path_expr(path);
                }
                Expr::Literal(literal) => {
                    listener.enter_literal_expr(literal.clone());
                    literal.clone().accept(db, listener);
                    listener.exit_literal_expr(literal);
                }
                Expr::Pi(pi) => {
                    listener.enter_pi(pi.clone());
                    pi.clone().accept(db, listener);
                    listener.exit_pi(pi);
                }
                Expr::Sigma(pi) => {
                    listener.enter_sigma(pi.clone());
                    pi.clone().accept(db, listener);
                    listener.exit_sigma(pi);
                }
                Expr::Type(type_ref, location) => listener.visit_type(type_ref, location),
            }
        }
    }

    impl HirElement for Expr {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            match self {
                Self::Empty => Location::call_site(db),
                Self::Error(downcast) => downcast.location(db),
                Self::Path(downcast) => downcast.location(db),
                Self::Literal(downcast) => downcast.location.clone().unwrap(),
                Self::Call(downcast) => downcast.location(db),
                Self::Ann(downcast) => downcast.location(db),
                Self::Abs(downcast) => downcast.location(db),
                Self::Match(downcast) => downcast.location(db),
                Self::Pi(downcast) => downcast.location.clone(),
                Self::Sigma(downcast) => downcast.location.clone(),
                Self::Type(_, location) => location.clone(),
            }
        }
    }
}

/// Defines a kind of terms. It does define type representations that can be used in the type level
/// of the language. These are the base of the language grammar and semantics.
pub mod type_rep {
    use super::*;

    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    pub struct TypeRep {
        pub expr: Box<expr::Expr>,
    }

    impl HirElement for TypeRep {
        fn location(&self, db: &dyn crate::HirDb) -> Location {
            self.expr.location(db)
        }
    }

    impl walking::Walker for TypeRep {
        fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
            self.expr.accept(db, listener);
        }
    }
}
