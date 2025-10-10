use std::collections::hash_map;
use std::collections::{HashMap, HashSet};

use std::hash::Hash;

use e_graph::fpeg::{FPeg, Region};

use mlton_ssa::ssa::{
    Cases, Datatype, Exp as MltExp, FunctionId, Label, Return, SmlType, Var, VarId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SkTransfer {
    Bug,
    Call {
        func: FunctionId,
        args: Vec<Region>,
        ret: Return,
    },
    Case {
        test: Region,
        cases: Cases,
        default: Option<Label>,
    },
    Goto {
        dst: Label,
        args: Vec<Region>,
    },
    Raise {
        args: Vec<Region>,
    },
    Return {
        args: Vec<Region>,
    },
    Runtime,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Block {
    pub(crate) label: Label,
    pub(crate) args: Vec<Region>,
    pub(crate) transfer: SkTransfer,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
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
    pub fn insert_exp(&mut self, scope: &HashMap<VarId, SmlType>, exp: &MltExp) -> Option<Region> {
        self.graph.make_region(&self.datatypes, scope, exp)
    }

    pub fn insert_arg(&mut self, var: &Var) -> Region {
        if let SmlType::Datatype(dt) = &var.ty {
            assert!(self.datatypes.iter().any(|d| &d.tycon == dt));
        }
        let scope = HashMap::from([(var.name.clone(), var.ty.clone())]);
        self.graph
            .make_region(&self.datatypes, &scope, &MltExp::Var(var.name.clone()))
            .unwrap_or_else(|| {
                panic!(
                    "Failed to insert function argument region for var {:?}",
                    var
                )
            })
    }
}
