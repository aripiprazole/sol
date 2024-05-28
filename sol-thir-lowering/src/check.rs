use sol_diagnostic::{Diagnostic, IntoSolDiagnostic};
use sol_thir::ElaboratedTerm;
use Implicitness::*;

use super::*;

/// CASE: lam-pi / lam-pi-implicit
fn lam_pi(
    db: &dyn ThirLoweringDb,
    ctx: Context,
    lam: Curried,
    pi: Pi,
    icit: Implicitness,
) -> sol_eyre::Result<Term> {
    let Curried::Lam(definition, value) = lam else {
        panic!("handle: no parameters")
    };
    let inner_ctx = ctx.create_new_value(db, definition, *pi.type_repr);
    let term_type = pi.closure.apply(db, Value::new_var(ctx.lvl(db), None))?;
    let elab_term = lam_thir_check(db, inner_ctx, *value, term_type, icit)?;

    Ok(Term::Lam(definition, pi.implicitness, elab_term.into()))
}

enum Curried {
    Lam(Definition, Box<Curried>),
    Expr(Expr),
}

fn new_curried_function(db: &dyn ThirLoweringDb, abs: LamExpr) -> Curried {
    let mut acc = Curried::Expr(*abs.value);
    for parameter in abs.parameters.into_iter() {
        let parameter = extract_parameter_definition(db, parameter);
        acc = Curried::Lam(parameter, Box::new(acc));
    }
    if let Curried::Expr(_) = acc {
        todo!("handle: no parameters")
    }
    acc
}

#[rustfmt::skip]
fn lam_thir_check(db: &dyn ThirLoweringDb, ctx: Context, expr: Curried, type_repr: Type, icit: Implicitness) -> sol_eyre::Result<Term> {
    match (&expr, &type_repr) {
        (Curried::Lam(_, _), Value::Pi(pi)) => lam_pi(db, ctx, expr, pi.clone(), icit),
        (Curried::Expr(expr), Value::Pi(pi @ Pi { implicitness: Implicit, .. })) => implicit_fun_eta(db, ctx, expr.clone(), pi.clone()),
        (Curried::Lam(_, _), _) => expected_lam_pi(db, ctx, expr, type_repr),
        (Curried::Expr(expr), _) => db.thir_check(ctx, expr.clone(), type_repr),
    }
}

fn expected_lam_pi(
    _: &dyn ThirLoweringDb,
    _: Context,
    _: Curried,
    _: Value,
) -> sol_eyre::Result<Term> {
    todo!("handle: error")
}

/// CASE: implicit-fun-n
///
/// If the pi is implicit, we transform it into an implicit lambda
fn implicit_fun_eta(
    db: &dyn ThirLoweringDb,
    ctx: Context,
    value: Expr,
    pi: Pi,
) -> sol_eyre::Result<Term> {
    let inner_ctx = ctx.insert_new_binder(db, pi.name.unwrap(), *pi.type_repr);
    let term_type = pi.closure.apply(db, Value::new_var(ctx.lvl(db), None))?;
    let elab_term = db.thir_check(inner_ctx, value, term_type)?;

    Ok(Term::Lam(pi.name.unwrap(), Implicit, elab_term.into()))
}

fn type_hole() -> sol_eyre::Result<Term> {
    Ok(Term::InsertedMeta(MetaVar::new(None)))
}

/// CASE: term_equality
fn term_equality(
    db: &dyn ThirLoweringDb,
    ctx: Context,
    expr: Expr,
    expected: Type,
) -> sol_eyre::Result<Term> {
    let ElaboratedTerm(term, type_repr) = db.thir_infer(ctx, expr)?;
    let ElaboratedTerm(term, inferred_type) = elaboration::insert(ctx, term, type_repr);
    elaboration::unify_catch(db, ctx, expected, inferred_type);
    Ok(term)
}

/// The check function to check the type of the term.
#[salsa::tracked]
#[rustfmt::skip]
pub fn thir_check(db: &dyn ThirLoweringDb, ctx: Context, expr: Expr, type_repr: Type) -> sol_eyre::Result<Term, sol_diagnostic::Diagnostic> {
    match (expr, type_repr) {
        (Expr::Lam(abs), Type::Pi(pi)) => lam_pi(db, ctx, new_curried_function(db, abs), pi.clone(), pi.implicitness).into_sol_diagnostic(),
        (value, Type::Pi(pi @ Pi { implicitness: Implicit, .. })) => implicit_fun_eta(db, ctx, value, pi).into_sol_diagnostic(),
        (Expr::Hole(_), _) => type_hole().into_sol_diagnostic(),
        (value, expected) => term_equality(db, ctx, value, expected).into_sol_diagnostic()
    }
}
