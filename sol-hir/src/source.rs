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

use miette::{MietteError, SourceCode, SourceOffset, SourceSpan, SpanContents};
use sol_diagnostic::TextSource;
use sol_syntax::Source;

use crate::{
    errors::{HirError, HirErrorKind},
    package::Package,
    reparse::reparse_hir_path,
    scope::Scope,
    walking,
};

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
        Self::error(db, HirError {
            kind: HirErrorKind::IncorrectKind(kind.to_string()),
            source_code: location.clone(),
            label: location.as_source_span(),
        })
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
    pub abstract_source: Source,
    pub start: SourceOffset,
    pub end: SourceOffset,
    pub text: TextSource,
}

impl miette::SourceCode for HirTextRange {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        self.text
            .read_span(span, context_lines_before, context_lines_after)
    }
}

impl serde::Serialize for HirTextRange {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        #[derive(serde::Serialize)]
        struct HirTextRangeDelegate {
            pub start: usize,
            pub end: usize,
            pub file_name: String,
            pub text: String,
        }

        HirTextRangeDelegate {
            start: self.start.offset(),
            end: self.end.offset(),
            file_name: self.text.name().into(),
            text: self.text.data().to_string(),
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

impl SourceCode for Location {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        match self {
            Self::TextRange(range) => {
                range.read_span(span, context_lines_before, context_lines_after)
            }
            Self::CallSite => Err(MietteError::OutOfBounds),
        }
    }
}

impl Location {
    pub fn start(&self) -> SourceOffset {
        match self {
            Location::TextRange(range) => range.start,
            Location::CallSite => SourceOffset::from(0),
        }
    }

    pub fn end(&self) -> SourceOffset {
        match self {
            Location::TextRange(range) => range.end,
            Location::CallSite => SourceOffset::from(0),
        }
    }

    pub fn as_source_span(self) -> SourceSpan {
        match self {
            Location::TextRange(range) => SourceSpan::new(range.start, range.end),
            Location::CallSite => SourceSpan::new(SourceOffset::from(0), SourceOffset::from(0)),
        }
    }

    pub fn text_source(self) -> TextSource {
        match self {
            Location::TextRange(range) => range.text,
            Location::CallSite => TextSource::new("internal_error.txt", Arc::default()),
        }
    }
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
                .field("file.name", &range.text.name())
                .field("file.text", &range.text.data())
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
    pub fn new<I: Into<SourceOffset>>(src: Source, text: TextSource, start: I, end: I) -> Self {
        Self::TextRange(HirTextRange {
            abstract_source: src,
            start: start.into(),
            end: end.into(),
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
    pub fn ending(self, end: SourceOffset) -> Self {
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

/// Defines an element of the High-Level Intermediate Representation. It's implemented for all
/// elements of the HIR.
pub trait HirElement {
    /// The range of the element in the source file.
    fn location(&self, db: &dyn crate::HirDb) -> Location;
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

pub mod declaration;
pub mod expr;
pub mod literal;
pub mod pattern;
pub mod stmt;
pub mod top_level;
pub mod type_rep;
