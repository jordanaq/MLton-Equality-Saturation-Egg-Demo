use std::collections::HashMap;

use e_graph::*;

use sml_utils::SmlType;


type IdType = u64;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct BlockArg {
    id: IdType,
    arg_t: SmlType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Transfer {

}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Block {
    id: IdType,
    inputs: Vec<BlockArg>,
    transfer: Transfer
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Skeleton {
    graph: FPeg,
    root: Block,
    blocks: HashMap<IdType, Block>,
}
