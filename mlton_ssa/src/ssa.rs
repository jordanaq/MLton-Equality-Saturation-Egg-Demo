use sml_utils::SmlType;
use std::collections::HashMap;

pub type ConstructorId = String;
pub type FunctionId = String;
pub type VarId = String;
pub type Label = u128;

enum TypeTree {
    Array(Box<Type>),
    CPointer,
    Datatype(SmlType),
    IntInf,
    Real(f64),
    Ref(Box<Type>),
    Thread,
    Tuple(Vec<Type>),
    Vector(Box<Type>),
    Weak(Box<Type>),
    Word(u128),
}

struct Type {
    hash: u128,
    plist: Vec<String>, /* TODO */
    tree: TypeTree,
}

struct Var {
    name: VarId,
    var_t: SmlType,
}

enum Const {
    CSymbol(/* TODO */),
    IntInf(i128 /* TODO */),
    Null,
    Real(f64),
    Word(u128),
}

enum Exp {
    ConApp { con: ConstructorId, args: Vec<Var> },
    Const(Const),
    Profile(/* TODO */),
    Select { tuple: VarId, offset: i128 },
    Tuple(Vec<VarId>),
    Var(VarId),
}

pub struct Statement {
    var: Option<VarId>,
    ty: SmlType,
    exp: Exp,
}

enum Return {
    Dead,
    NonTail(/* TODO */),
    Tail,
}

enum Cases {
    Con(HashMap<ConstructorId, Label>),
    Word(HashMap<u128, Label>),
}

enum Transfer {
    Bug,
    Call {
        args: Vec<Var>,
        func: FunctionId,
        ret: Return,
    },
    Case {
        test: (VarId, SmlType),
        cases: Cases,
        default: Option<Label>,
    },
    Goto {
        dst: Label,
        args: Vec<Var>,
    },
    Raise {
        args: Vec<Var>,
    },
    Return {
        args: Vec<Var>,
    },
    Runtime,
}

struct Block {
    args: Vec<Var>,
    label: Label,
    statements: Vec<Statement>,
    transfer: Transfer,
}

pub struct Datatype {
    pub tycon: String, /* TODO */
    pub constrs: Vec<(ConstructorId, Vec<SmlType>)>,
}

pub struct Function {
    args: Vec<Var>,
    blocks: HashMap<Label, Block>,
    may_inline: bool,
    name: String,
    raises: Option<Vec<SmlType>>,
    returns: Option<Vec<SmlType>>,
    start: Label,
}

pub struct MltonSsa {
    pub datatypes: HashMap<SmlType, Datatype>,
    pub globals: HashMap<VarId, Statement>,
    pub functions: HashMap<String, Function>,
    pub main: FunctionId,
}
