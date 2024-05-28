#![feature(stmt_expr_attributes)]

use std::sync::Arc;

use miette::{MietteError, MietteSpanContents, SpanContents};
use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = DiagnosticDb)]
pub struct Jar(crate::Diagnostics);

/// The diagnostic accumulator, used to store diagnostics, accross
/// salsa revisions.
///
/// This is used to store diagnostics, accross salsa revisions.
#[salsa::accumulator]
pub struct Diagnostics(Arc<miette::Report>);

/// Report miette error to the diagnostic accumulator.
pub fn report_error<T: Into<miette::Report>>(db: &dyn DiagnosticDb, report: T) {
    Diagnostics::push(db, Arc::new(report.into()));
}

pub trait DiagnosticDb: DbWithJar<Jar> {}

impl<DB> DiagnosticDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

/// Utility struct for when you have a regular [`SourceCode`] type that doesn't
/// implement `name`. For example [`String`]. Or if you want to override the
/// `name` returned by the `SourceCode`.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TextSource {
    source: Arc<String>,
    name: String,
}

impl std::fmt::Debug for TextSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NamedSource")
            .field("name", &self.name)
            .field("source", &"<redacted>");
        Ok(())
    }
}

impl TextSource {
    /// Create a new `NamedSource` using a regular [`SourceCode`] and giving
    /// its returned [`SpanContents`] a name.
    pub fn new(name: impl AsRef<str>, source: Arc<String>) -> Self {
        Self {
            source,
            name: name.as_ref().to_string(),
        }
    }

    /// Gets the name of this `NamedSource`.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns a reference the inner [`SourceCode`] type for this
    /// `NamedSource`.
    pub fn inner(&self) -> &(dyn miette::SourceCode + 'static) {
        &*self.source
    }

    pub fn data(&self) -> &str {
        &self.source
    }
}

impl miette::SourceCode for TextSource {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        let contents = self
            .inner()
            .read_span(span, context_lines_before, context_lines_after)?;
        Ok(Box::new(MietteSpanContents::new_named(
            self.name.clone(),
            contents.data(),
            *contents.span(),
            contents.line(),
            contents.column(),
            contents.line_count(),
        )))
    }
}
