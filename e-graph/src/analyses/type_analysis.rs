use core::panic;
use std::sync::Arc;

use egg::{Analysis, DidMerge, EGraph};
use mlton_ssa::ssa::{Const, Constr, Datatype, Prim, SmlType, WordSize};

use crate::fpeg::{FPegL, PrimWrapper, Region};

#[derive(Debug, Clone)]
pub struct TypeAnalysis {
    datatypes: Arc<Vec<Datatype>>,
}

impl Default for TypeAnalysis {
    fn default() -> Self {
        Self {
            datatypes: Arc::new(Vec::new()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeInfo {
    Concrete(SmlType),
    Applied(Prim, Vec<TypeInfo>),
    Unknown,
}

impl TypeInfo {
    fn unify(&self, other: &TypeInfo) -> Option<TypeInfo> {
        match (self, other) {
            (TypeInfo::Concrete(sml_ty1), TypeInfo::Concrete(sml_ty2)) => {
                if sml_ty1 == sml_ty2 {
                    Some(self.clone())
                } else {
                    None
                }
            }
            (TypeInfo::Applied(prim1, args1), TypeInfo::Applied(prim2, args2)) => {
                if prim1 == prim2 && args1.len() == args2.len() {
                    let unified_args = args1
                        .iter()
                        .zip(args2.iter())
                        .filter_map(|(a1, a2)| a1.unify(a2))
                        .collect();
                    Some(TypeInfo::Applied(prim1.clone(), unified_args))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl TypeAnalysis {
    pub fn with_datatypes(datatypes: Vec<Datatype>) -> Self {
        Self {
            datatypes: Arc::new(datatypes),
        }
    }

    fn datatype_from_con(&self, cons: &str) -> Option<Datatype> {
        self.datatypes.iter().find(|dt| dt.contains_cons(cons)).cloned()
    }

    fn unify_constructor(
        &self,
        egraph: &EGraph<FPegL, Self>,
        constr: &Constr,
        arg_ids: &[Region],
    ) -> Option<TypeInfo> {
        let dt = match self.datatype_from_con(&constr.name) {
            Some(dt) => dt,
            None => panic!(
                "Constructor {} not found in datatypes {:?}",
                constr.name, self.datatypes
            ),
        };

        let expected_arg_tys = dt.get_constructor_types(constr)?.clone();

        if expected_arg_tys.len() != arg_ids.len() {
            panic!(
                "Constructor {} expected {} arguments, got {}",
                constr.name,
                expected_arg_tys.len(),
                arg_ids.len()
            );
        }

        if expected_arg_tys.is_empty() {
            return Some(TypeInfo::Concrete(constr.constr_type.clone()));
        }

        for (expected_ty, arg_id) in expected_arg_tys.iter().zip(arg_ids.iter()) {
            let arg_ty = match &egraph[*arg_id].data {
                TypeInfo::Concrete(ty) => ty.clone(),
                _ => todo!(),
            };

            if *expected_ty != arg_ty {
                panic!(
                    "Constructor {} expected argument of type {:?}, got {:?}",
                    constr.name, expected_ty, arg_ty
                );
            }
        }

        todo!()
    }
}

impl Analysis<FPegL> for TypeAnalysis {
    type Data = TypeInfo;

    fn make(egraph: &mut EGraph<FPegL, Self>, enode: &FPegL) -> Self::Data {
        match enode {
            FPegL::PrimApp(PrimWrapper { ty, .. }, _) => todo!(),
            FPegL::Construct(constr, ids) => todo!()
            FPegL::Select([tuple_id, offset_id]) => {
                let tup_tys = match egraph[*tuple_id].data.clone() {
                    TypeInfo::Concrete(SmlType::Tuple(tys)) => tys,
                    _ => panic!(
                        "Expected tuple type for Select, got {:?}",
                        egraph[*tuple_id].data
                    ),
                };

                let idx = literal_offset(egraph, *offset_id).unwrap();

                todo!()
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
