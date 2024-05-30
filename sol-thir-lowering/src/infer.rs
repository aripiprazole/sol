use sol_diagnostic::fail;
use sol_thir::{
    find_reference_type, infer_constructor,
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

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("unsupported term")]
#[diagnostic(code(sol::thir::unsupported_term))]
pub struct UnsupportedTerm {
    #[source_code]
    #[label = "here"]
    pub location: Location,
}

/// The infer function to infer the type of the term.
#[salsa::tracked]
pub fn thir_infer(
    db: &dyn ThirLoweringDb,
    ctx: Context,
    expr: Expr,
) -> sol_diagnostic::Result<ElaboratedTerm> {
    use sol_hir::source::expr::Pi as EPi;
    use sol_hir::source::pattern::Pattern;
    use Expr::*;

    Ok(ElaboratedTerm::from(match expr {
        Empty | Error(_) | Match(_) | Sigma(_) => {
            return fail(UnsupportedTerm {
                location: expr.location(db),
            })
        }
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
        Call(_) => todo!(),
        Lam(_) => todo!(),
        Pi(EPi {
            parameters, value, ..
        }) => {
            let mut codomain = db.thir_check(ctx, *value.expr, Value::U)?;
            for parameter in parameters {
                let parameter_type = parameter.parameter_type(db);
                let domain = db.thir_check(ctx, *parameter_type.expr, Value::U)?;
                let name = if let Pattern::Binding(binding) = parameter.binding(db) {
                    Some(binding.name)
                } else {
                    None
                };
                let implicitness = if parameter.is_implicit(db) {
                    Implicitness::Implicit
                } else {
                    Implicitness::Explicit
                };
                codomain = Term::Pi(name, implicitness, domain.into(), codomain.into());
            }

            (codomain, Value::U)
        }
        Hole(_) => {
            let meta = MetaVar::new(None);
            let term = Term::InsertedMeta(meta.clone());
            (term, Value::Flexible(meta, vec![]))
        }
    }))
}
