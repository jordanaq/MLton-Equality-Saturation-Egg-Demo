use std::collections::hash_map;
use std::collections::{HashMap, HashSet};

use std::hash::Hash;

use e_graph::fpeg::{FPeg, Region};

use mlton_ssa::ssa::{ConstructorId, FunctionId, Label, RealSize, SmlType, Var, VarId, WordSize};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SkHandler {
    Caller,
    Dead,
    Handle { habler: Label },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum SkReturn {
    Dead,
    NonTail { cont_id: Label, handler: SkHandler },
    Tail,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SkCases {
    Con(Vec<(ConstructorId, Label)>),
    Word(WordSize, Vec<(u64, Label)>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum SkTransfer {
    Bug,
    Call {
        func: FunctionId,
        args: Vec<Region>,
        ret: SkReturn,
    },
    Case {
        test: Region,
        cases: SkCases,
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Block {
    pub(crate) label: Label,
    pub(crate) args: Vec<Region>,
    pub(crate) transfer: SkTransfer,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct SkFunc {
    args: Vec<Region>,
    blocks: Vec<Block>,
    may_inline: bool,
    name: FunctionId,
    raises: Option<Vec<SmlType>>,
    returns: Option<Vec<SmlType>>,
    start: Label,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Datatype {
    tycons: String,
    constructors: Vec<(ConstructorId, Vec<SmlType>)>,
}

#[derive(Debug, Clone)]
pub struct Skeleton {
    datatyeps: Vec<Datatype>,
    globals: Vec<Region>,
    functions: Vec<SkFunc>,
    graph: FPeg,
    main: FunctionId,
    curr_id_gen: u64,
}
