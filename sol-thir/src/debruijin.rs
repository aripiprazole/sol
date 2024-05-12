//! Debruijin indexes and levels. It's used for type checker's efficient indexing
//! and substitution.

use super::*;

/// Defines a debruijin level. It does represent the level of the context/environment
///
/// It can be transformed into a debruijin index by using the [`Level::as_idx`] method.
#[salsa::tracked]
pub struct Level {
    pub value: usize,
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum Level2IdxError {
    /// The level is greater than the index.
    #[error("expected l > x")]
    LevelGreaterThanIndex,
    /// The level is equal to zero.
    #[error("expected l > 0")]
    LevelEqualToZero,
}

#[salsa::tracked]
impl Level {
    /// Transforms a level into a debruijin index.
    #[salsa::tracked]
    pub fn as_idx(self, db: &dyn crate::ThirDb, l: Level) -> Result<Index, Level2IdxError> {
        if l.value(db) > self.value(db) {
            return Err(Level2IdxError::LevelGreaterThanIndex);
        }
        if l.value(db) == 0 {
            return Err(Level2IdxError::LevelEqualToZero);
        }
        Ok(Index(0))
    }

    #[salsa::tracked]
    pub fn increase(self, db: &dyn crate::ThirDb) -> Level {
        let value = self.value(db);
        Level::new(db, value + 1)
    }
}

/// Defines a debruijin index. That can be converted by two levels.
///
/// It's used to represent a variable in the syntax tree.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Index(usize);

impl From<usize> for Index {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl std::ops::Add<usize> for Index {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl std::ops::AddAssign<usize> for Index {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs
    }
}

/// Debruijin indices construction, that can be used to get the names of the
/// variables.
#[salsa::accumulator]
pub struct Indices((String, source::Term));
