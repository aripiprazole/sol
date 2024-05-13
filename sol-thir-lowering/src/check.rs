use sol_thir::InferResult;
use Implicitness::*;

use super::*;

/// CASE: lam-pi / lam-pi-implicit
fn lam_pi(db: &dyn ThirLoweringDb, ctx: Context, lam: Curried, pi: Pi, icit: Implicitness) -> Term {
    let Curried::Lam(definition, value) = lam else {
        panic!("handle: no parameters")
    };
    let inner_ctx = ctx.create_new_value(db, definition, *pi.type_repr);
    let term_type = pi.closure.apply(db, Value::new_var(ctx.lvl(db), None));
    let elab_term = lam_thir_check(db, inner_ctx, *value, term_type, icit);

    Term::Lam(definition, pi.implicitness, elab_term.into())
}

enum Curried {
    Lam(Definition, Box<Curried>),
    Expr(Expr),
}

fn new_curried_function(db: &dyn ThirLoweringDb, abs: AbsExpr) -> Curried {
    let mut acc = Curried::Expr(*abs.value);
    for parameter in abs.parameters.into_iter() {
        let parameter = extract_parameter_definition(db, parameter);
        acc = Curried::Lam(parameter, Box::new(acc));
    }
    if let Curried::Expr(value) = acc {
        todo!("handle: no parameters")
    }
    acc
}

#[rustfmt::skip]
fn lam_thir_check(db: &dyn ThirLoweringDb, ctx: Context, expr: Curried, type_repr: Type, icit: Implicitness) -> Term {
    match (&expr, &type_repr) {
        (Curried::Lam(_, _), Value::Pi(pi)) => lam_pi(db, ctx, expr, pi.clone(), icit),
        (Curried::Expr(expr), Value::Pi(pi @ Pi { implicitness: Implicit, .. })) => implicit_fun_eta(db, ctx, expr.clone(), pi.clone()),
        (Curried::Lam(_, _), _) => expected_lam_pi(db, ctx, expr, type_repr),
        (Curried::Expr(expr), _) => db.thir_check(ctx, expr.clone(), type_repr),
    }
}

fn expected_lam_pi(db: &dyn ThirLoweringDb, ctx: Context, expr: Curried, type_repr: Value) -> Term {
    todo!("handle: error")
}

/// CASE: implicit-fun-n
///
/// If the pi is implicit, we transform it into an implicit lambda
fn implicit_fun_eta(db: &dyn ThirLoweringDb, ctx: Context, value: Expr, pi: Pi) -> Term {
    let inner_ctx = ctx.insert_new_binder(db, pi.name.unwrap(), *pi.type_repr);
    let term_type = pi.closure.apply(db, Value::new_var(ctx.lvl(db), None));
    let elab_term = thir_check(db, inner_ctx, value, term_type);

    Term::Lam(pi.name.unwrap(), Implicit, elab_term.into())
}

fn type_hole() -> Term {
    Term::InsertedMeta(MetaVar::new(None))
}

/// CASE: term_equality
fn term_equality(db: &dyn ThirLoweringDb, ctx: Context, expr: Expr, expected: Type) -> Term {
    let InferResult(term, type_repr) = db.thir_infer(ctx, expr);
    let InferResult(term, inferred_type) = elaboration::insert(ctx, term, type_repr);
    elaboration::unify_catch(db, ctx, expected, inferred_type);
    term
}

/// The check function to check the type of the term.
#[salsa::tracked]
#[rustfmt::skip]
pub fn thir_check(db: &dyn ThirLoweringDb, ctx: Context, expr: Expr, type_repr: Type) -> Term {
    match (expr, type_repr) {
        (Expr::Abs(abs), Type::Pi(pi)) => lam_pi(db, ctx, new_curried_function(db, abs), pi.clone(), pi.implicitness),
        (value, Type::Pi(pi @ Pi { implicitness: Implicit, .. })) => implicit_fun_eta(db, ctx, value, pi),
        (Expr::Upgrade(box TypeRep::Hole), _) => type_hole(),
        (value, expected) => term_equality(db, ctx, value, expected)
    }
}
