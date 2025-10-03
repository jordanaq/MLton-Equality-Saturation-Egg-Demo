use std::{cmp::Ordering, collections::HashMap, fmt::Display, str::FromStr};

pub type ConstructorId = String;
pub type FunctionId = String;
pub type VarId = String;
pub type Label = String;

use crate::parse::{parse_exp_const, parse_sml_type_raw, parse_ssa};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RealSize {
    R32,
    R64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WordSize {
    W8,
    W16,
    W32,
    W64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SmlType {
    Array(Box<SmlType>),
    CPointer,
    Datatype(String /* TODO */),
    IntInf,
    Real(RealSize),
    Ref(Box<SmlType>),
    Thread,
    Tuple(Vec<SmlType>),
    Vector(Box<SmlType>),
    Weak(Box<SmlType>),
    Word(WordSize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CType {
    CPointer,
    Int8,
    Int16,
    Int32,
    Int64,
    Objptr,
    Real32,
    Real64,
    Word8,
    Word16,
    Word32,
    Word64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Var {
    pub name: VarId,
    pub ty: SmlType,
}

impl Var {
    pub fn new(name: VarId, ty: SmlType) -> Self {
        Var { name, ty }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Const {
    CSymbol(/* TODO */),
    IntInf(i128 /* TODO */),
    Null,
    Real(RealSize, f64),
    Word(WordSize, u64),
    WordVector(String),
}

impl Eq for Const {}

impl Ord for Const {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Const::CSymbol(), Const::CSymbol()) => Ordering::Equal,
            (Const::CSymbol(), _) => Ordering::Less,
            (_, Const::CSymbol()) => Ordering::Greater,
            (Const::IntInf(a), Const::IntInf(b)) => a.cmp(b),
            (Const::IntInf(_), _) => Ordering::Less,
            (_, Const::IntInf(_)) => Ordering::Greater,
            (Const::Null, Const::Null) => Ordering::Equal,
            (Const::Null, _) => Ordering::Less,
            (_, Const::Null) => Ordering::Greater,
            (Const::Real(rs_a, r_a), Const::Real(rs_b, r_b)) => {
                match rs_a.cmp(rs_b) {
                    Ordering::Equal => r_a.partial_cmp(r_b).unwrap_or(Ordering::Equal),
                    ord => ord,
                }
            }
            (Const::Real(_, _), _) => Ordering::Less,
            (_, Const::Real(_, _)) => Ordering::Greater,
            (Const::Word(ws_a, w_a), Const::Word(ws_b, w_b)) => {
                match ws_a.cmp(ws_b) {
                    Ordering::Equal => w_a.cmp(w_b),
                    ord => ord,
                }
            }
            (Const::Word(_, _), _) => Ordering::Less,
            (_, Const::Word(_, _)) => Ordering::Greater,
            (Const::WordVector(s_a), Const::WordVector(s_b)) => s_a.cmp(s_b),
        }
    }
}

impl std::hash::Hash for Const {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Const::CSymbol() => {
                0u8.hash(state);
            }
            Const::IntInf(i) => {
                1u8.hash(state);
                i.hash(state);
            }
            Const::Null => {
                2u8.hash(state);
            }
            Const::Real(rs, r) => {
                3u8.hash(state);
                rs.hash(state);
                // Note: f64 does not implement Hash, so we convert to bits
                r.to_bits().hash(state);
            }
            Const::Word(ws, w) => {
                4u8.hash(state);
                ws.hash(state);
                w.hash(state);
            }
            Const::WordVector(s) => {
                5u8.hash(state);
                s.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum PrimKind {
    DependsOnState,
    Functional,
    Moveable,
    SideEffect,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CFunctionKind {
    Pure,
    Impure,
    Runtime,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CFunctionTarget {
    Direct(String),
    Indirect,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CFunctionConvention {
    Cdecl,
    Stdcall,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CFunctionSymbolScope {
    External,
    Private,
    Public,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum PrimPrimitive {
    CFunction {
        args: Vec<SmlType>,
        convention: CFunctionConvention,
        inline: bool,
        kind: CFunctionKind,
        prototype: (Vec<CType>, Option<CType>),
        ret: SmlType,
        symbol_scope: CFunctionSymbolScope,
        target: CFunctionTarget,
    },
    SmlPrim(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Prim {
    pub prim: PrimPrimitive,
    pub kind: PrimKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    ConApp {
        con: ConstructorId,
        args: Vec<VarId>,
    },
    PrimApp {
        prim: Prim,
        targs: Option<Vec<SmlType>>,
        args: Vec<VarId>,
    },
    Const(Const),
    Profile(/* TODO */),
    Select {
        tuple: VarId,
        offset: i128,
    },
    Tuple(Vec<VarId>),
    Var(VarId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub var: Option<VarId>,
    pub ty: SmlType,
    pub exp: Exp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Handler {
    Caller,
    Dead,
    Handle { label: Label },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Return {
    Dead,
    NonTail { cont: Label, handler: Handler },
    Tail,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Cases {
    Con(Vec<(ConstructorId, Label)>),
    Word(WordSize, Vec<(u64, Label)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Transfer {
    Bug,
    Call {
        func: FunctionId,
        args: Vec<VarId>,
        ret: Return,
    },
    Case {
        test: VarId,
        cases: Cases,
        default: Option<Label>,
    },
    Goto {
        dst: Label,
        args: Vec<VarId>,
    },
    Raise {
        args: Vec<VarId>,
    },
    Return {
        args: Vec<VarId>,
    },
    Runtime,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub args: Vec<Var>,
    pub label: Label,
    pub statements: Vec<Statement>,
    pub transfer: Transfer,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Datatype {
    pub tycon: String, /* TODO */
    pub constrs: Vec<(ConstructorId, Vec<SmlType>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub args: Vec<Var>,
    pub blocks: Vec<Block>,
    pub may_inline: bool,
    pub name: String,
    pub raises: Option<Vec<SmlType>>,
    pub returns: Option<Vec<SmlType>>,
    pub start: Label,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MltonSsa {
    pub datatypes: HashMap<SmlType, Datatype>,
    pub globals: Vec<Statement>,
    pub functions: Vec<Function>,
    pub main: FunctionId,
}
