pub use sol_hir::source::expr::{Expr, Meta};
pub use sol_hir::source::Location;
use sol_hir::source::{expr::CallExpr, pattern::Pattern};
use sol_hir::source::{literal::Literal, HirElement};

pub use crate::*;

pub fn literal_to_constructor_kind(literal: Literal) -> ConstructorKind {
    match literal {
        Literal::Empty => todo!(),
        Literal::Int8(_) => todo!(),
        Literal::UInt8(_) => todo!(),
        Literal::Int16(_) => todo!(),
        Literal::UInt16(_) => todo!(),
        Literal::Int32(_) => todo!(),
        Literal::UInt32(_) => todo!(),
        Literal::Int64(_) => todo!(),
        Literal::UInt64(_) => todo!(),
        Literal::String(_) => todo!(),
        Literal::Boolean(_) => todo!(),
        Literal::Char(_) => todo!(),
    }
}

pub fn get_definition(db: &dyn crate::ThirDb, pattern: Pattern) -> Definition {
    match pattern {
        Pattern::Hole => todo!(),
        Pattern::Literal(_) => todo!(),
        Pattern::Wildcard(_) => todo!(),
        Pattern::Rest(_) => todo!(),
        Pattern::Error(_) => todo!(),
        Pattern::Constructor(_) => todo!(),
        Pattern::Binding(binding) => binding.name,
    }
}

#[salsa::tracked]
pub fn eval(db: &dyn crate::ThirDb, env: Env, expr: Expr) -> Term {
    Term::located(expr.location(db), match expr {
        Expr::Meta(Meta(location, value)) => Term::sorry_but_no(value),
        Expr::Literal(ref literal) => Term::Constructor(Constructor {
            location: literal.location.clone().unwrap_or(expr.location(db)),
            kind: literal_to_constructor_kind(literal.value.clone()),
        }),
        Expr::Path(path) => Term::Constructor(Constructor {
            location: path.location(db),
            kind: ConstructorKind::Definition(path.definition(db)),
        }),
        Expr::Call(call) => todo!(),
        Expr::Ann(ann) => todo!(),
        Expr::Abs(abs) => abs
            .parameters
            .into_iter()
            .fold(Err(*abs.value), |acc, param| {
                let definition = get_definition(db, param.binding(db));
                let location = param.location(db);
                let term = Term::Lam(definition, param.is_implicit(db).into(), Closure {
                    env,
                    expr: match acc {
                        Ok(value) => quote(db, value),
                        Err(expr) => expr,
                    },
                });

                Ok(Term::located(location, term))
            })
            .expect("no first parameter"),
        Expr::Match(expr) => Term::sorry(expr.location(db), ThirError::MatchNotSupported),
        Expr::Upgrade(upgrade) => Term::sorry(upgrade.location(db), ThirError::UpgradeNotSupported),
        Expr::Error(error) => Term::sorry(error.location(db), ThirError::HirError(error)),
        Expr::Empty => Term::no(),
    })
}

pub fn quote(db: &dyn crate::ThirDb, term: Term) -> Expr {
    todo!()
}

pub fn infer(db: &dyn crate::ThirDb, env: Env, expr: Expr) -> (Expr, Type) {
    todo!()
}
