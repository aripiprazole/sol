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
pub struct Diagnostics(Arc<sol_eyre::Report>);

/// A result type that uses [`Diagnostic`] as the error type.
pub type Result<T, E = Diagnostic> = std::result::Result<T, E>;

/// A trait for types that can be unwrapped or report an error. By reporting
/// an error, it means that the error is added to the diagnostic accumulator.
pub trait UnwrapOrReport<T: Default> {
    fn unwrap_or_report(self, db: &dyn DiagnosticDb) -> T;
}

impl<T: Default> UnwrapOrReport<T> for sol_eyre::Result<T> {
    fn unwrap_or_report(self, db: &dyn DiagnosticDb) -> T {
        self.unwrap_or_else(|diagnostic| {
            report_error(db, diagnostic);
            T::default()
        })
    }
}

pub trait IntoSolDiagnostic<T> {
    fn into_sol_diagnostic(self) -> Result<T>;
}

impl<T> IntoSolDiagnostic<T> for Result<T, sol_eyre::Report> {
    fn into_sol_diagnostic(self) -> Result<T> {
        self.map_err(Diagnostic::from)
    }
}

#[derive(Clone, Debug)]
pub struct Diagnostic(pub Arc<sol_eyre::Report>);

impl Eq for Diagnostic {}

impl From<sol_eyre::Report> for Diagnostic {
    fn from(report: sol_eyre::Report) -> Self {
        Self(Arc::new(report))
    }
}

impl PartialEq for Diagnostic {
    fn eq(&self, other: &Diagnostic) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

/// Report miette error to the diagnostic accumulator.
pub fn report_error<T: Into<sol_eyre::Report>>(db: &dyn DiagnosticDb, report: T) {
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
