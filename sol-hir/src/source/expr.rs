//! Defines a kind of terms. It does define expressions that can be used in a block. These are the
//! base of the language grammar and semantics.

use std::{fmt::Formatter, sync::Arc};

use miette::{SourceOffset, SourceSpan};
use sol_diagnostic::report_error;

use super::*;
use crate::{
    errors::{HirError, HirErrorKind},
    primitives::primitive_type_rep,
    solver::Reference,
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
pub struct LamExpr {
    pub parameters: Vec<pattern::Pattern>,
    pub value: Box<expr::Expr>,
    pub location: Location,
    pub scope: Arc<Scope>,
}

impl walking::Walker for LamExpr {
    fn accept<T: walking::HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        listener.enter_lam_expr(self.clone());
        self.parameters.clone().accept(db, listener);
        self.value.clone().accept(db, listener);
        self.location(db).accept(db, listener);
        listener.exit_lam_expr(self);
    }
}

impl salsa::DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for LamExpr {
    fn fmt(&self, f: &mut Formatter<'_>, _: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl HirElement for LamExpr {
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
        self.value.clone().accept(db, listener);
        self.type_rep.clone().accept(db, listener);
        self.location(db).accept(db, listener);
        listener.exit_ann_expr(self);
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
        self.scrutinee.clone().accept(db, listener);
        self.clauses.clone().accept(db, listener);
        self.location(db).accept(db, listener);
        listener.exit_match_expr(self);
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
        self.callee.clone().accept(db, listener);
        self.arguments.clone().accept(db, listener);
        self.do_notation.clone().accept(db, listener);
        self.location(db).accept(db, listener);
        listener.exit_call_expr(self);
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
    Universe,
    This,
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
    Hole(Location),
    Type(Type, Location),
    Error(HirError),
    Path(Reference),
    Literal(Spanned<literal::Literal>),
    Call(CallExpr),
    Ann(AnnExpr),
    Lam(LamExpr),
    Match(MatchExpr),
    Pi(Pi),
    Sigma(Pi),
}

impl Expr {
    /// Creates a unit expression. It's used to create a unit expression, that is just like a
    /// `()` value.
    pub fn call_unit_expr(location: Location) -> Self {
        Self::Call(CallExpr {
            kind: CallKind::Prefix,
            callee: Callee::Unit,
            arguments: vec![],
            do_notation: None,
            location,
        })
    }

    /// Creates a block do-notation expression. It's used to create a block expression, that is
    /// just like a `do { }` value.
    pub fn block(db: &dyn crate::HirDb, do_notation: stmt::Block) -> Self {
        Self::Call(CallExpr {
            kind: CallKind::Prefix,
            callee: Callee::Do,
            arguments: vec![],
            do_notation: Some(do_notation.clone()),
            location: do_notation.location(db),
        })
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
        report_error(db, HirError {
            kind: HirErrorKind::Empty,
            label: SourceSpan::new(SourceOffset::from(0), SourceOffset::from(0)),
            source_code: Location::CallSite,
        });

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
            Expr::Hole(_) => write!(f, "Hole"),
            Expr::Error(error) => write!(f, "Error({error:?})"),
            Expr::Literal(literal) => write!(f, "Literal({literal:?})"),
            Expr::Path(reference) => reference.debug_all(db).fmt(f),
            Expr::Call(call_expr) => call_expr.debug_all(db).fmt(f),
            Expr::Ann(ann_expr) => ann_expr.debug_all(db).fmt(f),
            Expr::Lam(abs_expr) => abs_expr.debug_all(db).fmt(f),
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
            Expr::Type(type_ref, location) => listener.visit_type(type_ref, location),
            Expr::Hole(location) => listener.visit_hole(location),
            Expr::Call(call_expr) => call_expr.accept(db, listener),
            Expr::Ann(ann_expr) => ann_expr.accept(db, listener),
            Expr::Lam(abs_expr) => abs_expr.accept(db, listener),
            Expr::Match(match_expr) => match_expr.accept(db, listener),
            Expr::Error(_) => {}
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
        }
    }
}

impl HirElement for Expr {
    fn location(&self, db: &dyn crate::HirDb) -> Location {
        match self {
            Self::Empty => Location::call_site(db),
            Self::Error(downcast) => downcast.source_code.clone(),
            Self::Path(downcast) => downcast.location(db),
            Self::Literal(downcast) => downcast.location.clone().unwrap(),
            Self::Call(downcast) => downcast.location(db),
            Self::Ann(downcast) => downcast.location(db),
            Self::Lam(downcast) => downcast.location(db),
            Self::Match(downcast) => downcast.location(db),
            Self::Pi(downcast) => downcast.location.clone(),
            Self::Sigma(downcast) => downcast.location.clone(),
            Self::Type(_, location) => location.clone(),
            Self::Hole(location) => location.clone(),
        }
    }
}
