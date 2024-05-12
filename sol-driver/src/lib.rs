use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

use dashmap::{DashMap, DashSet};
use salsa::DebugWithDb;
use sol_hir::{
    lowering::HirLowering,
    package::{HasManifest, Package},
    primitives::{PrimitiveBag, PrimitiveProvider},
    source::expr::Expr,
};
use sol_thir::{source::Term, value::Type, ThirLowering, ThirTyping};

/// Defines watcher strategies for [`RootDb`].
pub mod watcher;

pub mod rename;

#[allow(unused)]
pub mod suite;

extern crate salsa_2022 as salsa;

/// The root database implementation for the Sol frontend, after the frontend, there's the
/// codegen backend, and it's not mean't to use incremental compilation, so it's not part of the
/// database.
///
/// The database is used to store the results of the various passes of the compiler, and to
/// invalidate them when the input changes.
///
/// The database is also used to store the logs of the compiler, so that they can be printed.
#[salsa::db(
    sol_hir::Jar,
    sol_vfs::Jar,
    sol_syntax::Jar,
    sol_diagnostic::Jar,
    sol_typer::Jar,
    sol_hir_lowering::Jar,
    sol_thir::Jar,
    sol_thir_lowering::Jar
)]
#[derive(Default)]
pub struct RootDb {
    /// Salsa storage, used to store the results of the various passes of the compiler.
    storage: salsa::Storage<RootDb>,
    packages: Arc<DashSet<Package>>,

    primitives: Arc<PrimitiveBag>,
    files: DashMap<PathBuf, sol_vfs::SourceFile>,
    logs: Option<Arc<Mutex<Vec<String>>>>,
}

/// Bridges the [`RootDb`] with the [`sol_hir_lowering::HirLowering`] trait.
impl HirLowering for RootDb {
    fn hir_declare(&self, pkg: Package, src: sol_syntax::Source) -> sol_hir::source::HirSource {
        sol_hir_lowering::hir_declare(self, pkg, src)
    }

    fn hir_lower(&self, pkg: Package, src: sol_syntax::Source) -> sol_hir::source::HirSource {
        sol_hir_lowering::hir_lower(self, pkg, src)
    }
}

/// Bridges the [`RootDb`] with the [`sol_thir::ThirLowering`] trait.
impl ThirLowering for RootDb {
    fn thir_eval(&self, env: sol_thir::shared::Env, term: Term) -> sol_thir::value::Value {
        sol_thir_lowering::thir_eval(self, env, term)
    }

    fn thir_quote(&self, lvl: sol_thir::debruijin::Level, value: sol_thir::value::Value) -> Term {
        sol_thir_lowering::thir_quote(self, lvl, value)
    }
}

/// Bridges the [`RootDb`] with the [`sol_thir::ThirTyping`] trait.
impl ThirTyping for RootDb {
    fn thir_infer(&self, ctx: sol_thir::shared::Context, expr: Expr) -> (Term, Type) {
        sol_thir_lowering::thir_infer(self, ctx, expr)
    }

    fn thir_check(&self, ctx: sol_thir::shared::Context, expr: Expr, type_repr: Type) -> Term {
        sol_thir_lowering::thir_check(self, ctx, expr, type_repr)
    }
}

impl RootDb {
    /// Registers a package in the database.
    pub fn register_package(&self, package: Package) -> Package {
        self.packages.insert(package);
        package
    }
}

impl PrimitiveProvider for RootDb {
    /// Gets primitives lazily
    fn primitives(&self) -> Arc<PrimitiveBag> {
        self.primitives.clone()
    }
}

impl HasManifest for RootDb {
    fn all_packages(&self) -> Vec<Package> {
        self.packages.iter().map(|p| *p).collect::<Vec<_>>()
    }
}

impl salsa::Database for RootDb {
    fn salsa_event(&self, event: salsa::Event) {
        // Log interesting events, if logging is enabled
        if let Some(logs) = &self.logs {
            // don't log boring events
            if let salsa::EventKind::WillExecute { .. } = event.kind {
                logs.lock()
                    .unwrap()
                    .push(format!("Event: {:?}", event.debug(self)));
            }
        }
    }
}

impl salsa::ParallelDatabase for RootDb {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            primitives: self.primitives.clone(),
            storage: self.storage.snapshot(),
            logs: self.logs.clone(),
            files: self.files.clone(),
            packages: self.packages.clone(),
        })
    }
}
