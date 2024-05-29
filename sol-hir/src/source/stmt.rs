//! Defines a kind of statements. It does define statements that can be used in a block, and
//! "do-notations", that are used to do imperative code in a functional language.

use std::{fmt::Formatter, sync::Arc};

use super::*;
use crate::{errors::HirError, scope::ScopeKind, walking::HirListener};

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

impl walking::Walker for AskStmt {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        listener.enter_ask_stmt(self.clone());
        self.pattern.clone().accept(db, listener);
        self.value.clone().accept(db, listener);
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

impl walking::Walker for LetStmt {
    fn accept<T: HirListener>(self, db: &dyn crate::HirDb, listener: &mut T) {
        listener.enter_let_stmt(self.clone());
        self.pattern.clone().accept(db, listener);
        self.value.clone().accept(db, listener);
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
            Stmt::Error(_) => {}
            Stmt::Ask(ask_stmt) => ask_stmt.accept(db, listener),
            Stmt::Let(let_stmt) => let_stmt.accept(db, listener),
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
            Stmt::Error(error) => write!(f, "Error({error:?})"),
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
            Self::Error(downcast) => downcast.label.clone(),
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
        self.statements.clone().accept(db, listener);
        self.location(db).accept(db, listener);
        listener.exit_block(self);
    }
}

impl DefaultWithDb for Block {
    fn default_with_db(db: &dyn crate::HirDb) -> Self {
        let scope = Scope::new_ref(ScopeKind::Block);

        Self {
            statements: vec![],
            location: Location::call_site(db),
            scope,
        }
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
