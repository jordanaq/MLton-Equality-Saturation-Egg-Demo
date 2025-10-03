use sml_utils::SmlType;
use std::collections::HashMap;

pub type ConstructorId = String;
pub type FunctionId = String;
pub type VarId = String;
pub type Label = String;

use crate::parse::parse_ssa;

#[derive(Debug, Clone, PartialEq)]
enum TypeTree {
    Array(Box<Type>),
    CPointer,
    Datatype(SmlType),
    IntInf,
    Real(f64), /* Enum for bit widths */
    Ref(Box<Type>),
    Thread,
    Tuple(Vec<Type>),
    Vector(Box<Type>),
    Weak(Box<Type>),
    Word(u128), /* enum for bit widths */
}

#[derive(Debug, Clone, PartialEq)]
struct Type {
    hash: u128,
    plist: Vec<String>, /* TODO */
    tree: TypeTree,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub name: VarId,
    pub var_t: SmlType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RealSize {
    R32,
    R64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum WordSize {
    W8,
    W16,
    W32,
    W64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    CSymbol(/* TODO */),
    IntInf(i128 /* TODO */),
    Null,
    Real(RealSize, f64),
    Word(WordSize, u64),
    WordVector(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimKind {
    DependsOnState,
    Functional,
    Moveable,
    SideEffect,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CFunctionKind {
    Pure,
    Impure,
    Runtime,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CFunctionTarget {
    Direct(String),
    Indirect,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CFunctionConvention {
    Cdecl,
    Stdcall,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CFunctionSymbolScope {
    External,
    Private,
    Public,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimPrimitive {
    CFunction {
        args: Vec<SmlType>,
        convention: CFunctionConvention,
        inline: bool,
        kind: CFunctionKind,
        prototype: (Vec<SmlType>, Option<SmlType>),
        ret: SmlType,
        symbol_scope: CFunctionSymbolScope,
        target: CFunctionTarget,
    },
    SmlPrim(String),
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Handler {
    Caller,
    Dead,
    Handle { label: Label },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Return {
    Dead,
    NonTail { cont_id: Label, handler: Handler },
    Tail,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Cases {
    Con(Vec<(ConstructorId, Label)>),
    Word(WordSize, Vec<(u64, Label)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Transfer {
    Bug,
    Call {
        func: VarId,
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct ParseErr;

impl std::str::FromStr for MltonSsa {
    type Err = ParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_ssa(s) {
            Ok(("", ssa)) => Ok(ssa),
            _ => Err(ParseErr),
        }
    }
}
