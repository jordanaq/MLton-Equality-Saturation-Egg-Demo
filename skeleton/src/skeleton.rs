use std::collections::{HashMap, HashSet};
use std::collections::hash_map;

use std::hash::Hash;

use e_graph::fpeg::{ FPeg, Region };

use sml_utils::SmlType;

type IdType = u64;
pub(crate) type ArgId = IdType;
pub(crate) type BlockId = IdType;
pub(crate) type FuncId = IdType;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BlockArg {
    id: ArgId,
    arg_t: SmlType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Transfer {
    Bug,
    Goto(BlockId, Vec<Region>),
    Call,
    Raise,
    Return,
    Match,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Block {
    pub(crate) id: BlockId,
    pub(crate) name: String,
    pub(crate) inputs: Vec<BlockArg>,
    pub(crate) transfer: Transfer,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct FuncArg { 
    id: ArgId,
    arg_t: SmlType,
}

#[derive(Debug, Clone)]
pub(crate) struct Func {
    name: String,
    inputs: Vec<FuncArg>,
    return_t: SmlType,
    entry: BlockId,
    blocks: HashMap<BlockId, Block>,
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.inputs == other.inputs
            && self.return_t == other.return_t
    }
}

impl Eq for Func {}

impl Hash for Func {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.inputs.hash(state);
        self.return_t.hash(state);
    }
}

impl Func {
    fn add_block(&mut self, b: Block) -> Result<BlockId, String> {
        let id = b.id;
        match self.blocks.entry(id) {
            hash_map::Entry::Occupied(o) =>
                Err(format!("Attempt to create block with existing id `{}`", o.key())),
            hash_map::Entry::Vacant(v) => {
                v.insert(b);
                Ok(id)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct DatatypeConstructor {
    name: String,
    inputs: Vec<SmlType>
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Datatype {
    name: String,
    constructors: Vec<DatatypeConstructor>
}


#[derive(Debug, Clone)]
pub struct Skeleton {
    curr_id_gen: u64,
    funcs: HashMap<FuncId, Func>,
    datatyeps: HashSet<Datatype>,
    graph: FPeg,
}

impl Skeleton {
    pub fn new() -> Self {
        Skeleton {
            curr_id_gen: 0,
            funcs: HashMap::<FuncId, Func>::default(),
            datatyeps: HashSet::<Datatype>::default(),
            graph: FPeg::default()
        }
    }

    pub(crate) fn fresh_id(&mut self) -> u64 {
        self.curr_id_gen += 1;
        self.curr_id_gen - 1
    }

    fn block_map(&self) -> HashMap<BlockId, &Block> {
        self.funcs
            .values()
            .flat_map(|f| f.blocks.iter().map(|(&k, v)| (k, v)))
            .collect::<HashMap<_, _>>()
    }

    fn blocks(&self) -> HashSet<&Block> {
        self.funcs
            .values()
            .map(|f| f.blocks.values())
            .flatten()
            .collect::<HashSet<_>>()
    }

    pub(crate) fn find_block_by_name(&self, name: &str) -> Option<&Block> {
        self.blocks()
            .iter()
            .map(|&b| b)
            .find(|&b| b.name == name)
    }

    pub(crate) fn find_region_by_name(&self, name: &str) -> Option<Region> {
        self.graph.find_region_by_name(name)
    }

    pub(crate) fn add_block(&mut self, func_id: FuncId, b: Block) -> Result<BlockId, String> {
        let fun = self.funcs.get_mut(&func_id).expect(&format!("Invalid function id `{}", func_id));
        fun.add_block(b)
    }

    fn valid(&self) -> bool {
        // Only one block with each id
        self.blocks().len() == self.block_map().len()
        
        // Each id matches in its map
            && self.block_map()
                .iter()
                .all(|(&k, v)| k == v.id)
    }
}

impl Default for Skeleton {
    fn default() -> Self {
        Self::new()
    }
}
