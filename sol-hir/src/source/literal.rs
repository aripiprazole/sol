//! Defines a kind of primaries. It does define terms that are literally literals, and it's used
//! as numbers, strings, etc... These are the base of the base of the base of the language

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
