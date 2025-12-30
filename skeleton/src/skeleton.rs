use std::collections::{HashMap, };

use std::hash::Hash;

use e_graph::fpeg::{FPeg, Region};

use mlton_ssa::ssa::{
    Cases, Datatype, Exp as MltExp, FunctionId, Label, MltonSsa as MltSsa, Return, SmlType, Var,
    VarId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SkTransfer {
    Bug,
    Call {
        func: FunctionId,
        state: Region,
        args: Vec<Region>,
        ret: Return,
    },
    Case {
        test: Region,
        state: Region,
        cases: Cases,
        default: Option<Label>,
    },
    Goto {
        dst: Label,
        state: Region,
        args: Vec<Region>,
    },
    Raise {
        state: Region,
        args: Vec<Region>,
    },
    Return {
        state: Region,
        args: Vec<Region>,
    },
    Runtime,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Block {
    pub(crate) label: Label,
    pub(crate) state: Region,
    pub(crate) args: Vec<Region>,
    pub(crate) transfer: SkTransfer,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) state: Region,
    pub(crate) args: Vec<Region>,
    pub(crate) blocks: Vec<Block>,
    pub(crate) may_inline: bool,
    pub(crate) name: FunctionId,
    pub(crate) raises: Option<Vec<SmlType>>,
    pub(crate) returns: Option<Vec<SmlType>>,
    pub(crate) start: Label,
}

#[derive(Debug, Clone)]
pub struct Skeleton {
    pub datatypes: Vec<Datatype>,
    pub globals: Vec<Region>,
    pub functions: Vec<Function>,
    pub(crate) graph: FPeg,
    pub main: FunctionId,
}

impl Default for Skeleton {
    fn default() -> Self {
        Self {
            datatypes: vec![],
            globals: vec![],
            functions: vec![],
            graph: FPeg::new(),
            main: "(* Bug *)".to_string(),
        }
    }
}

impl Skeleton {
    pub fn insert_exp(
        &mut self,
        scope: &HashMap<VarId, SmlType>,
        exp: &MltExp,
        ty: Option<&SmlType>,
    ) -> Option<Region> {
        self.graph.make_region(&self.datatypes, scope, exp, ty)
    }

    pub fn insert_stateful_exp(
        &mut self,
        scope: &HashMap<VarId, SmlType>,
        exp: &MltExp,
        state_r: &Region,
        ty: Option<&SmlType>,
    ) -> Option<Region> {
        if let MltExp::PrimApp { prim, args, targs } = exp {
            self.graph
                .make_stateful_region(&self.datatypes, scope, prim, args, targs, state_r, ty)
        } else {
            panic!(
                "Expected stateful expression to be a PrimApp, got {:?}",
                exp
            )
        }
    }

    pub fn insert_arg(&mut self, var: &Var) -> Region {
        if let SmlType::Datatype(dt) = &var.ty {
            assert!(self.datatypes.iter().any(|d| &d.tycon == dt));
        }
        let scope = HashMap::from([(var.name.clone(), var.ty.clone())]);
        self.graph
            .make_region(
                &self.datatypes,
                &scope,
                &MltExp::Var(var.name.clone()),
                Some(&var.ty),
            )
            .unwrap_or_else(|| {
                panic!(
                    "Failed to insert function argument region for var {:?}",
                    var
                )
            })
    }

    pub fn insert_state_arg(&mut self, prefix: &str) -> Region {
        let var_name = format!("{prefix}_s");
        let state_var = Var {
            name: var_name.clone(),
            ty: SmlType::State,
        };
        self.insert_arg(&state_var)
    }

    pub fn from_mltonssa(mlton: &MltSsa) -> Self {
        crate::from_mlton::sk_from_mltonssa(mlton)
    }
}
