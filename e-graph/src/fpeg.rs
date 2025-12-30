use std::{collections::HashMap, iter};

use egg::{EGraph, Id, define_language};

use mlton_ssa::ssa::{
    Const, ConstructorId as MltConstructorId, Datatype as MltDatatype, Exp as MltExp,
    Prim as MltPrim, SmlType, VarId as MltVarId,
};

pub type Region = Id;

/// Represents a constructor use in the FPeg IR
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constr {
    pub constr_type: SmlType,
    pub tycon: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct PrimWrapper {
    pub(crate) prim: MltPrim,
    pub(crate) targs: Option<Vec<SmlType>>,
    pub(crate) ty: Option<SmlType>,
}

define_language! {
    /// Defines the FPeg IR for the e-graph
    pub enum FPegL {
         // Represents a call of an SML primitive function
        PrimApp(PrimWrapper, Box<[Region]>),

        // Construct a datatype given a constructor and an arbitrary number of args
        Construct(Constr, Box<[Region]>),

        // Select an item from a tuple given an offset
        "Select" = Select([Region; 2]),

        // Create a tuple
        "Tuple" = Tuple(Box<[Region]>),

        // A literal of an SML primitive type
        Literal(Const),

        // Represents an argument
        Arg(MltVarId),
    }
}

type Analysis = crate::analyses::type_analysis::TypeAnalysis;

/// Wrapper for FPeg e-graph
#[derive(Debug, Clone)]
pub struct FPeg {
    pub(crate) egraph: EGraph<FPegL, Analysis>,
    region_map: HashMap<String, Region>,
}

impl FPeg {
    pub fn find_region_by_name(&self, name: &str) -> Option<Region> {
        self.region_map.get(name).copied()
    }
}

impl Default for FPeg {
    fn default() -> Self {
        FPeg {
            egraph: EGraph::<FPegL, Analysis>::default(),
            region_map: HashMap::<String, Region>::default(),
        }
    }
}

impl FPeg {
    pub(crate) fn datatype_from_con(
        datatypes: &Vec<MltDatatype>,
        cons: &MltConstructorId,
    ) -> Option<MltDatatype> {
        datatypes.iter().find(|dt| dt.contains_cons(cons)).cloned()
    }

    pub fn make_region(
        &mut self,
        datatypes: &Vec<MltDatatype>,
        scope: &HashMap<MltVarId, SmlType>,
        mlton_expr: &MltExp,
        exp_ty: Option<&SmlType>,
    ) -> Option<Region> {
        let exp = match mlton_expr {
            MltExp::ConApp { con, args } => {
                let dt = Self::datatype_from_con(datatypes, &con)?;
                let constr = Constr {
                    constr_type: SmlType::Datatype(dt.tycon),
                    tycon: con.clone(),
                };

                let expected_arg_tys = dt.constrs.iter().find(|(c, _)| c == con)?.1.clone();

                let actual_arg_tys = args
                    .iter()
                    .map(|a| scope.get(a).unwrap().clone())
                    .collect::<Vec<SmlType>>();

                assert!(expected_arg_tys == actual_arg_tys);

                let arg_rs = args
                    .iter()
                    .zip(expected_arg_tys.iter())
                    .map(|(a, ty): (&String, &SmlType)| {
                        self.make_region(datatypes, scope, &MltExp::Var(a.clone()), Some(ty))
                    })
                    .collect::<Option<Box<[Region]>>>()?;
                FPegL::Construct(constr, arg_rs)
            }
            MltExp::PrimApp { prim, targs, args } => {
                let arg_rs = args
                    .iter()
                    .map(|a| {
                        let ty = scope
                            .get(a)
                            .unwrap_or_else(|| {
                                panic!("Variable {} not found in scope {:?}", a, scope)
                            })
                            .clone();
                        self.make_region(datatypes, scope, &MltExp::Var(a.clone()), Some(&ty))
                    })
                    .collect::<Option<Box<[Region]>>>()?;

                FPegL::PrimApp(
                    PrimWrapper {
                        prim: prim.clone(),
                        targs: targs.clone(),
                        ty: exp_ty.cloned(),
                    },
                    arg_rs,
                )
            }
            MltExp::Const(c) => FPegL::Literal(c.clone()),
            MltExp::Profile() => todo!(),
            MltExp::Select { tuple, offset } => {
                let ty = scope
                    .get(tuple)
                    .unwrap_or_else(|| panic!("Variable {} not found in scope {:?}", tuple, scope))
                    .clone();
                let tup_r =
                    self.make_region(datatypes, scope, &MltExp::Var(tuple.clone()), Some(&ty))?;
                let offset_r = self.make_region(
                    datatypes,
                    scope,
                    &MltExp::Const(Const::IntInf(*offset)),
                    Some(&SmlType::IntInf),
                )?;

                FPegL::Select([tup_r, offset_r])
            }
            MltExp::Tuple(items) => {
                let item_rs = items
                    .iter()
                    .map(|i| {
                        let ty = scope
                            .get(i)
                            .unwrap_or_else(|| {
                                panic!("Variable {} not found in scope {:?}", i, scope)
                            })
                            .clone();
                        self.make_region(datatypes, scope, &MltExp::Var(i.clone()), Some(&ty))
                    })
                    .collect::<Option<Box<[Region]>>>()?;

                FPegL::Tuple(item_rs)
            }
            MltExp::Var(v) => {
                if scope.get(v).unwrap_or_else(|| {
                    panic!("Variable {} not found in scope {:?}", v, scope);
                }) != exp_ty.unwrap_or(&SmlType::Tuple(vec![]))
                {
                    panic!(
                        "Type mismatch for variable {}: expected {:?}, found {:?}",
                        v,
                        exp_ty.unwrap_or(&SmlType::Datatype("Unknown".into())),
                        scope.get(v).unwrap()
                    );
                }
                FPegL::Arg(v.clone())
            }
        };

        Some(self.egraph.add(exp))
    }

    pub fn make_stateful_region(
        &mut self,
        datatypes: &Vec<MltDatatype>,
        scope: &HashMap<MltVarId, SmlType>,
        prim: &MltPrim,
        args: &Vec<MltVarId>,
        targs: &Option<Vec<SmlType>>,
        state_r: &Region,
        exp_ty: Option<&SmlType>,
    ) -> Option<Region> {
        let arg_rs = iter::once(Some(*state_r))
            .chain(args.iter().map(|a| {
                let ty = scope
                    .get(a)
                    .unwrap_or_else(|| panic!("Variable {} not found in scope {:?}", a, scope))
                    .clone();
                self.make_region(datatypes, scope, &MltExp::Var(a.clone()), Some(&ty))
            }))
            .collect::<Option<Box<[Region]>>>()?;

        Some(self.egraph.add(FPegL::PrimApp(
            PrimWrapper {
                prim: prim.clone(),
                targs: if let Some(ts) = targs {
                    Some(
                        iter::once(&SmlType::State)
                            .chain(ts.clone().iter())
                            .cloned()
                            .collect::<Vec<SmlType>>(),
                    )
                } else {
                    None
                },
                ty: exp_ty.cloned(),
            },
            arg_rs,
        )))
    }

    pub fn new() -> Self {
        Self::default()
    }
}

#[cfg(test)]
mod tests {
    use mlton_ssa::ssa::WordSize;

    use super::*;

    fn make_test_datatypes() -> Vec<MltDatatype> {
        vec![MltDatatype {
            tycon: "list_0".into(),
            constrs: vec![
                (
                    "cons_0".into(),
                    vec![
                        SmlType::Word(WordSize::W64),
                        SmlType::Datatype("list_0".into()),
                    ],
                ),
                ("nil_0".into(), vec![]),
            ],
        }]
    }

    fn make_test_fpeg() -> FPeg {
        FPeg::new()
    }

    #[test]
    fn test_datatype_from_con() {
        let datatypes: Vec<MltDatatype> = make_test_datatypes();

        let con = MltConstructorId::from("cons_0");
        let dt = FPeg::datatype_from_con(&datatypes, &con).unwrap();
        assert_eq!(dt.tycon, "list_0");

        let con = MltConstructorId::from("nil_0");
        let dt = FPeg::datatype_from_con(&datatypes, &con).unwrap();
        assert_eq!(dt.tycon, "list_0");

        let con = MltConstructorId::from("nonexistent");
        let dt = FPeg::datatype_from_con(&datatypes, &con);
        assert!(dt.is_none());
    }

    #[test]
    fn test_make_region() {
        let mut fpeg = make_test_fpeg();
        let datatypes: Vec<MltDatatype> = make_test_datatypes();

        let scope = HashMap::<MltVarId, SmlType>::from([
            ("x".into(), SmlType::Word(WordSize::W64)),
            ("y".into(), SmlType::Word(WordSize::W64)),
            ("xs".into(), SmlType::Datatype("list_0".into())),
        ]);

        let e1 = MltExp::ConApp {
            con: "cons_0".into(),
            args: vec!["x".into(), "xs".into()],
        };

        let r1 = fpeg.make_region(
            &datatypes,
            &scope,
            &e1,
            Some(&SmlType::Datatype("list_0".into())),
        );
        assert!(r1.is_some());

        let e2 = MltExp::PrimApp {
            prim: MltPrim::WordAdd(WordSize::W64),
            targs: None,
            args: vec!["x".into(), "y".into()],
        };

        let r2 = fpeg.make_region(&datatypes, &scope, &e2, Some(&SmlType::Word(WordSize::W64)));
        assert!(r2.is_some());

        assert_eq!(fpeg.egraph.number_of_classes(), 5);
        assert_eq!(fpeg.egraph.total_number_of_nodes(), 5);
    }

    #[test]
    fn test_make_stateful_region() {
        let mut fpeg = make_test_fpeg();
        let state_r = fpeg.egraph.add(FPegL::Arg("state".into()));
        let r = fpeg.make_stateful_region(
            &vec![MltDatatype {
                tycon: "array_0".into(),
                constrs: vec![],
            }],
            &HashMap::<MltVarId, SmlType>::new(),
            &MltPrim::ArrayArray,
            &vec![],
            &None,
            &state_r,
            Some(&SmlType::Datatype("array_0".into())),
        );

        assert!(r.is_some());
        let prim_app_r = &fpeg.egraph[r.unwrap()].nodes.iter().collect::<Vec<_>>();
        assert_eq!(prim_app_r.len(), 1);
        assert_eq!(
            prim_app_r[0],
            &FPegL::PrimApp(
                PrimWrapper {
                    prim: MltPrim::ArrayArray,
                    targs: None,
                    ty: Some(SmlType::Datatype("array_0".into())),
                },
                Box::new([state_r]),
            )
        );
    }
}
