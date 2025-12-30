use egg::{Analysis, DidMerge, EGraph};
use mlton_ssa::ssa::{Const, Prim, SmlType, WordSize};

use crate::fpeg::{FPegL, PrimWrapper, Region};

#[derive(Debug, Clone, Default)]
pub struct TypeAnalysis;

pub type TypeData = Option<SmlType>;

impl Analysis<FPegL> for TypeAnalysis {
    type Data = TypeData;

    fn make(egraph: &mut EGraph<FPegL, Self>, enode: &FPegL) -> Self::Data {
        match enode {
            FPegL::PrimApp(PrimWrapper { ty, .. }, _) => ty.clone(),
            FPegL::Construct(constr, ids) => todo!(),
            FPegL::Select([tuple_id, offset_id]) => {
                let tup_ty = match egraph[*tuple_id].data.clone() {
                    Some(ty) => ty,
                    None => return None,
                };

                let idx = literal_offset(egraph, *offset_id)?;

                match tup_ty {
                    SmlType::Tuple(tup) => tup.get(idx).cloned(),
                    _ => None,
                }
            }
            FPegL::Tuple(ids) => todo!(),
            FPegL::Literal(_) => todo!(),
            FPegL::Arg(_) => todo!(),
        }
    }

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        if let Some(from_ty) = from {
            if let Some(ex_ty) = to {
                assert_eq!(&from_ty, ex_ty);
                DidMerge(false, false)
            } else {
                *to = Some(from_ty);
                DidMerge(true, true)
            }
        } else {
            DidMerge(false, false)
        }
    }
}

fn literal_offset(egraph: &EGraph<FPegL, TypeAnalysis>, offset_id: Region) -> Option<usize> {
    let offset_class = &egraph[offset_id];
    offset_class.nodes.iter().find_map(|node| match node {
        FPegL::Literal(Const::IntInf(value)) => usize::try_from(*value).ok(),
        FPegL::Literal(Const::Word(_, value)) => usize::try_from(*value).ok(),
        _ => None,
    })
}
