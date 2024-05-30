use sol_diagnostic::bail;
use sol_thir::{
    find_reference_type, infer_constructor,
    shared::{Constructor, ConstructorKind},
    ElaboratedTerm, ThirConstructor,
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
        Path(path) => {
            let constructor = Constructor {
                kind: ConstructorKind::Reference(path),
                location: path.location(db),
            };
            let (_, inferred_type) = find_reference_type(db, ctx, path)?;

            (Term::Constructor(constructor), inferred_type)
        }
        Literal(literal) => {
            let constructor = Constructor {
                location: literal.location(db),
                kind: literal.value.into(),
            };
            let inferred_type = infer_constructor(db, ctx, constructor.clone())?;

            (Term::Constructor(constructor), inferred_type)
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
        Call(call) => {
            todo!()
        }
        Lam(_) => todo!(),
        Pi(_) => todo!(),
        Sigma(_) => bail!("sigma types are not supported yet"),
        Hole(_) => {
            let meta = MetaVar::new(None);
            let term = Term::InsertedMeta(meta.clone());
            (term, Value::Flexible(meta, vec![]))
        }
    }))
}
