use std::sync::Arc;

use miette::{SourceOffset, SourceSpan};
use sol_diagnostic::{report_error, TextSource};

use crate::Source;

#[derive(Debug, Clone, PartialEq, Eq, miette::Diagnostic, thiserror::Error)]
#[error("syntax error: {message}")]
#[diagnostic(code(solc::syntax_error), url(docsrs))]
pub struct SyntaxError {
    #[source_code]
    pub source_code: TextSource,

    #[label = "here"]
    pub location: SourceSpan,

    pub message: String,
}

#[salsa::tracked]
impl Source {
    /// Defines the [`Source::errors`] query.
    ///
    /// Returns a list of syntax errors in this program. This query is memoized, so it will only be
    /// executed once for each program.
    #[salsa::tracked]
    pub fn errors(self, db: &dyn crate::ParseDb) -> Vec<SyntaxError> {
        let node = self.syntax_node(db).as_ref().root_node();

        let mut stack = vec![node];
        let mut errors = vec![];

        let file_name = self.file_path(db).to_string_lossy().to_string();
        let text = Arc::new(self.source_text(db).clone());

        while let Some(node) = stack.pop() {
            for child in node.children(&mut node.walk()) {
                stack.push(child);
            }

            match true {
                _ if node.has_error() && node.is_missing() => {
                    errors.push(SyntaxError {
                        // The error message is the node's S-expression.
                        message: node.to_sexp().to_lowercase(),
                        source_code: TextSource::new(file_name.clone(), text.clone()),
                        location: SourceSpan::new(
                            SourceOffset::from(0),
                            SourceOffset::from(text.len()),
                        ),
                    });
                }
                _ if node.is_error() => {
                    let mut cursor = node.walk();
                    let unexpected = node
                        .children(&mut cursor)
                        .flat_map(|node| node.utf8_text(text.as_bytes()))
                        .map(|pao| pao.to_string())
                        .collect::<Vec<_>>()
                        .join(" ");

                    errors.push(SyntaxError {
                        message: format!("unexpected token(s): {unexpected}"),
                        source_code: TextSource::new(file_name.clone(), text.clone()),
                        location: SourceSpan::new(
                            SourceOffset::from(0),
                            SourceOffset::from(text.len()),
                        ),
                    });
                }
                _ => {}
            };
        }

        errors
    }

    /// Defines the [`Source::validated`] query.
    ///
    /// Validates a Sol program. This query is memoized, so it will only be executed once for each
    /// program.
    ///
    /// This query will also report any errors that it finds to the diagnostics database.
    #[salsa::tracked]
    pub fn validated(self, db: &dyn crate::ParseDb) -> Source {
        for error in self.errors(db) {
            report_error(db, error);
        }

        self
    }
}
