use std::collections::HashMap;

use e_graph::{ FPeg, Region };

use sml_utils::SmlType;

type ArgId = u64;
type BlockId = u64;
type GlobalId = u64;
type FuncId = u64;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct BlockArg {
    id: ArgId,
    arg_t: SmlType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Transfer {
    Goto(BlockId, Vec<Region>),
    Call,
    Raise,
    Return,
    Match,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Block {
    id: BlockId,
    inputs: Vec<BlockArg>,
    transfer: Transfer,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Skeleton {
    graph: FPeg,
    root: Block,
    blocks: HashMap<BlockId, Block>,
}
