use sol_thir::{
    shared::{Constructor, ConstructorKind},
    ElaboratedTerm,
};

use super::*;

fn create_from_type(definition: sol_hir::source::expr::Type, location: Location) -> Term {
    use sol_hir::source::expr::Type::*;

    Term::Constructor(Constructor {
        location,
        kind: match definition {
            Universe => return Term::U,
            This => todo!("handle: error"),
            Unit => ConstructorKind::UnitType,
            String => ConstructorKind::StringType,
            Bool => ConstructorKind::BooleanType,
            Nat => ConstructorKind::NatType,
            Int8 => ConstructorKind::IntType(true, 8),
            UInt8 => ConstructorKind::IntType(false, 8),
            Int16 => ConstructorKind::IntType(true, 16),
            UInt16 => ConstructorKind::IntType(false, 16),
            Int32 => ConstructorKind::IntType(true, 32),
            UInt32 => ConstructorKind::IntType(false, 32),
            Int64 => ConstructorKind::IntType(true, 64),
            UInt64 => ConstructorKind::IntType(false, 64),
        },
    })
}

/// The infer function to infer the type of the term.
#[salsa::tracked]
pub fn thir_infer(
    db: &dyn ThirLoweringDb,
    ctx: Context,
    expr: Expr,
) -> sol_diagnostic::Result<ElaboratedTerm> {
    use Expr::*;

    Ok(ElaboratedTerm::from(match expr {
        Empty | Error(_) | Match(_) => todo!(),
        Path(_) => todo!(),
        Literal(literal) => {
            let constructor = Constructor {
                location: literal.location(db),
                kind: literal.value.into(),
            };
            (Term::Constructor(constructor.clone()), constructor.infer())
        }
        Type(definition, location) => match create_from_type(definition, location) {
            Term::U => (Term::U, Value::U),
            term => (term, Value::U),
        },
        Ann(ann) => {
            let actual_type = db.thir_check(ctx, *ann.type_rep.expr, Value::U)?;
            let actual_type = db.thir_eval(ctx.locals(db), actual_type)?;
            let term = db.thir_check(ctx, *ann.value, actual_type.clone())?;
            (term, actual_type)
        }
        Call(_) => todo!(),
        Lam(_) => todo!(),
        Pi(_) => todo!(),
        Sigma(_) => todo!(),
        Hole(_) => {
            let meta = MetaVar::new(None);
            let term = Term::InsertedMeta(meta.clone());
            (term, Value::Flexible(meta, vec![]))
        }
    }))
}
