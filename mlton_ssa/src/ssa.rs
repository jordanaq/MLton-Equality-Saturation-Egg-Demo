use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use ordered_float::OrderedFloat;

pub type ConstructorId = String;
pub type FunctionId = String;
pub type VarId = String;
pub type Label = String;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RealSize {
    R32,
    R64,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Const {
    CSymbol(/* TODO */),
    IntInf(i128 /* TODO */),
    Null,
    Real(RealSize, OrderedFloat<f64>),
    Word(WordSize, u64),
    WordVector(String),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CFunction {
    pub args: Vec<SmlType>,
    pub convention: CFunctionConvention,
    pub inline: bool,
    pub kind: CFunctionKind,
    pub prototype: (Vec<CType>, Option<CType>),
    pub ret: SmlType,
    pub symbol_scope: CFunctionSymbolScope,
    pub target: CFunctionTarget,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Prim {
    ArrayAlloc {
        raw: bool,
    },
    ArrayArray,
    ArrayCopyArray,
    ArrayCopyVector,
    ArrayLength,
    ArraySub,
    ArrayToArray,
    ArrayToVector,
    ArrayUninit,
    ArrayUninitIsNop,
    ArrayUpdate,
    CFunction(CFunction),
    CPointerAdd,
    CPointerDiff,
    CPointerEqual,
    CPointerFromWord,
    CPointerGetCPointer,
    CPointerGetObjptr,
    CPointerGetReal(RealSize),
    CPointerGetWord(WordSize),
    CPointerLt,
    CPointerSetCPointer,
    CPointerSetObjptr,
    CPointerSetReal(RealSize),
    CPointerSetWord(WordSize),
    CPointerSub,
    CPointerToWord,
    ExnExtra,
    ExnName,
    ExnSetExtendExtra,
    GCCollect,
    GCState,
    IntInfAdd,
    IntInfAndb,
    IntInfArshift,
    IntInfCompare,
    IntInfGcd,
    IntInfLshift,
    IntInfMul,
    IntInfNeg,
    IntInfNotb,
    IntInfOrb,
    IntInfQuot,
    IntInfRem,
    IntInfSub,
    IntInfToString,
    IntInfToVector,
    IntInfToWord(WordSize),
    IntInfXorb,
    MLtonBogus, // Makes a bogus value of any type.
    MLtonBug,
    MLtonDeserialize,
    MLtonEq,
    MLtonEqual,
    MLtonHalt,
    MLtonHash,
    MLtonHandlesSignals,
    MLtonInstallSignalHandler,
    MLtonSerialize,
    MLtonShare,
    MLtonSize,
    MLtonTouch,
    RealMathAcos(RealSize),
    RealMathAsin(RealSize),
    RealMathAtan(RealSize),
    RealMathAtan2(RealSize),
    RealMathCos(RealSize),
    RealMathExp(RealSize),
    RealMathLn(RealSize),
    RealMathLog10(RealSize),
    RealMathSin(RealSize),
    RealMathSqrt(RealSize),
    RealMathTan(RealSize),
    RealAbs(RealSize),
    RealAdd(RealSize),
    RealCastToWord(RealSize, WordSize),
    RealDiv(RealSize),
    RealEqual(RealSize),
    RealLdexp(RealSize),
    RealLe(RealSize),
    RealLt(RealSize),
    RealMul(RealSize),
    RealMuladd(RealSize),
    RealMulsub(RealSize),
    RealNeg(RealSize),
    RealQequal(RealSize),
    RealRndToReal(RealSize, RealSize),
    RealRndToWord(RealSize, WordSize, bool), // signed
    RealRound(RealSize),
    RealSub(RealSize),
    RefAssign,
    RefDeref,
    RefRef,
    StringToWord8Vector,
    ThreadAtomicBegin,
    ThreadAtomicEnd,
    ThreadAtomicState,
    ThreadCopy,
    ThreadCopyCurrent,
    ThreadReturnToC,
    ThreadSwitchTo,
    TopLevelGetHandler,
    TopLevelGetSuffix,
    TopLevelSetHandler,
    TopLevelSetSuffix,
    VectorLength,
    VectorSub,
    VectorVector,
    WeakCanGet,
    WeakGet,
    WeakNew,
    WordAdd(WordSize),
    WordAddCheckP(WordSize, bool), // signed
    WordAndb(WordSize),
    WordCastToReal(WordSize, RealSize),
    WordEqual(WordSize),
    WordExtdToWord(WordSize, WordSize, bool), // signed
    WordLshift(WordSize),
    WordLt(WordSize, bool),        // signed
    WordMul(WordSize, bool),       // signed
    WordMulCheckP(WordSize, bool), // signed
    WordNeg(WordSize),
    WordNegCheckP(WordSize),
    WordNotb(WordSize),
    WordOrb(WordSize),
    WordQuot(WordSize, bool),                // signed
    WordRem(WordSize, bool),                 // signed
    WordRndToReal(WordSize, RealSize, bool), // signed
    WordRol(WordSize),
    WordRor(WordSize),
    WordRshift(WordSize, bool), // signed
    WordSub(WordSize),
    WordSubCheckP(WordSize, bool), // signed
    WordToIntInf,
    WordXorb(WordSize),
    WordVectorToIntInf,
    WordArraySubWord {
        seq_size: WordSize,
        ele_size: WordSize,
    },
    WordArrayUpdateWord {
        seq_size: WordSize,
        ele_size: WordSize,
    },
    WordVectorSubWord {
        seq_size: WordSize,
        ele_size: WordSize,
    },
    Word8VectorToString,
    WorldSave,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Handler {
    Caller,
    Dead,
    Handle { label: Label },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Return {
    Dead,
    NonTail { cont: Label, handler: Handler },
    Tail,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

impl Datatype {
    pub fn contains_cons(&self, cons: &str) -> bool {
        println!(
            "Checking if datatype {} contains constructor {}",
            self.tycon, cons
        );
        self.constrs.iter().any(|(cons_tag, _)| cons_tag == cons)
    }
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

type Cfg = HashMap<Label, Vec<Label>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DominatorTree {
    root: Label,
    adjacency_list: HashMap<Label, HashSet<Label>>,
}

impl Function {
    pub fn block_adjacency_list(&self) -> Cfg {
        let mut l: HashMap<Label, Vec<Label>> = HashMap::new();

        for block in &self.blocks {
            let succs: Vec<String> = match &block.transfer {
                Transfer::Bug
                | Transfer::Raise { .. }
                | Transfer::Return { .. }
                | Transfer::Runtime => vec![],
                Transfer::Call { ret, .. } => match ret {
                    Return::Dead => vec![],
                    Return::NonTail { cont, .. } => vec![cont.into()],
                    Return::Tail => vec![],
                },
                Transfer::Case { cases, default, .. } => {
                    let mut succs: Vec<Label> = match cases {
                        Cases::Con(v) => v.iter().map(|(_, lbl)| lbl.into()).collect(),
                        Cases::Word(_, v) => v.iter().map(|(_, lbl)| lbl.into()).collect(),
                    };

                    if let Some(d) = default {
                        succs.push(d.into());
                    }

                    succs
                }
                Transfer::Goto { dst, .. } => vec![dst.into()],
            };

            l.insert(block.label.clone(), succs);
        }

        l
    }

    pub fn block_predecessor_list(&self) -> HashMap<Label, Vec<Label>> {
        let adjacency_list = self.block_adjacency_list();
        let mut pred_list: HashMap<Label, Vec<Label>> = HashMap::new();
        for Block { label, .. } in &self.blocks {
            pred_list.entry(label.clone()).or_default();
        }
        for (block, succs) in adjacency_list {
            for succ in succs {
                pred_list.entry(succ).or_default().push(block.clone());
            }
        }
        pred_list
    }

    pub fn dominator_tree(&self) -> DominatorTree {
        let mut doms: HashMap<Label, HashSet<Label>> = HashMap::new();

        let cfg = self.block_adjacency_list();
        let preds = self.block_predecessor_list();

        doms.insert(self.start.clone(), [self.start.clone()].into());

        for block in &self.blocks {
            if block.label != self.start {
                doms.insert(block.label.clone(), cfg.keys().cloned().collect());
            }
        }

        let mut changed = true;
        while changed {
            changed = false;

            for block in &self.blocks {
                if block.label == self.start {
                    continue;
                }

                let pred_doms: HashSet<Label> = preds
                    .get(&block.label)
                    .unwrap()
                    .iter()
                    .filter_map(|p| doms.get(p))
                    .cloned()
                    .fold(cfg.keys().cloned().collect(), |acc, d| {
                        acc.intersection(&d).cloned().collect()
                    });

                let new_doms: HashSet<Label> = pred_doms
                    .union(&[block.label.clone()].into())
                    .cloned()
                    .collect();

                if new_doms != *doms.get(&block.label).unwrap() {
                    doms.insert(block.label.clone(), new_doms);
                    changed = true;
                }
            }
        }

        DominatorTree {
            root: self.start.clone(),
            adjacency_list: doms,
        }
    }

    fn get_scope_h(
        &self,
        dominator: &DominatorTree,
        visited: &mut HashSet<Label>,
        scopes: &mut HashMap<Label, HashSet<(VarId, SmlType)>>,
        label: &Label,
    ) {
        let doms = dominator.adjacency_list.get(label).unwrap();

        // func args
        scopes.entry(label.clone()).or_default().extend(
            self.args
                .iter()
                .map(|arg| (arg.name.clone(), arg.ty.clone())),
        );

        for dom in doms {
            if dom == label {
                continue;
            }

            if !visited.contains(dom) {
                self.get_scope_h(dominator, visited, scopes, dom);
            }

            if let Some(dom_scope) = scopes.get(dom).cloned() {
                scopes.entry(label.clone()).or_default().extend(dom_scope);
            }
        }

        if let Some(block) = self.blocks.iter().find(|b| &b.label == label) {
            for arg in &block.args {
                scopes
                    .entry(label.clone())
                    .or_default()
                    .insert((arg.name.clone(), arg.ty.clone()));
            }
            // assume statements are in valid SSA form
            for stmt in &block.statements {
                if let Some(var) = &stmt.var {
                    scopes
                        .entry(label.clone())
                        .or_default()
                        .insert((var.clone(), stmt.ty.clone()));
                }
            }
        }

        visited.insert(label.clone());
    }

    pub fn get_scope(&self, label: &Label) -> Option<HashMap<VarId, SmlType>> {
        let mut visited: HashSet<Label> = HashSet::new();
        let mut scopes: HashMap<Label, HashSet<(VarId, SmlType)>> = HashMap::new();
        self.get_scope_h(&self.dominator_tree(), &mut visited, &mut scopes, label);
        scopes.get(label).map(|s| s.iter().cloned().collect())
    }

    pub fn get_scopes(&self) -> HashMap<Label, HashMap<VarId, SmlType>> {
        let mut visited: HashSet<Label> = HashSet::new();
        let mut scopes: HashMap<Label, HashSet<(VarId, SmlType)>> = HashMap::new();
        let dominator = self.dominator_tree();
        for block in &self.blocks {
            self.get_scope_h(&dominator, &mut visited, &mut scopes, &block.label);
        }
        scopes
            .into_iter()
            .map(|(k, v)| (k, v.into_iter().collect()))
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MltonSsa {
    pub datatypes: Vec<Datatype>,
    pub globals: Vec<Statement>,
    pub functions: Vec<Function>,
    pub main: FunctionId,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_func() -> Function {
        Function {
            args: vec![Var::new("x".into(), SmlType::Word(WordSize::W32))],
            blocks: vec![
                Block {
                    args: vec![],
                    label: "L1".into(),
                    statements: vec![],
                    transfer: Transfer::Goto {
                        dst: "L2".into(),
                        args: vec![],
                    },
                },
                Block {
                    args: vec![],
                    label: "L2".into(),
                    statements: vec![Statement {
                        var: Some("x_pairity".into()),
                        ty: SmlType::Word(WordSize::W32),
                        exp: Exp::PrimApp {
                            prim: Prim::WordAdd(WordSize::W32),
                            targs: None,
                            args: vec!["x".into(), "literal_1".into()],
                        },
                    }],
                    transfer: Transfer::Case {
                        test: "x_pairity".into(),
                        cases: Cases::Word(WordSize::W32, vec![(1, "L3".into()), (2, "L4".into())]),
                        default: Some("L5".into()),
                    },
                },
                Block {
                    args: vec![],
                    label: "L3".into(),
                    statements: vec![],
                    transfer: Transfer::Goto {
                        dst: "L6".into(),
                        args: vec!["literal_false".into()],
                    },
                },
                Block {
                    args: vec![],
                    label: "L4".into(),
                    statements: vec![],
                    transfer: Transfer::Goto {
                        dst: "L6".into(),
                        args: vec!["literal_true".into()],
                    },
                },
                Block {
                    args: vec![],
                    label: "L5".into(),
                    statements: vec![],
                    transfer: Transfer::Bug,
                },
                Block {
                    args: vec![Var {
                        name: "ret".into(),
                        ty: SmlType::Word(WordSize::W32),
                    }],
                    label: "L6".into(),
                    statements: vec![],
                    transfer: Transfer::Return {
                        args: vec!["ret".into()],
                    },
                },
            ],
            may_inline: false,
            name: "test_func".into(),
            raises: None,
            returns: None,
            start: "L1".into(),
        }
    }

    #[test]
    fn test_func_adjacency() {
        let func = example_func();

        let l = func.block_adjacency_list();

        assert_eq!(l.len(), 6);
        assert_eq!(*l.get("L1".into()).unwrap(), vec!["L2"]);
        assert_eq!(*l.get("L2".into()).unwrap(), vec!["L3", "L4", "L5"]);
        assert_eq!(*l.get("L3".into()).unwrap(), vec!["L6"]);
        assert_eq!(*l.get("L4".into()).unwrap(), vec!["L6"]);
        assert_eq!(*l.get("L5".into()).unwrap(), Vec::<String>::new());
        assert_eq!(*l.get("L6".into()).unwrap(), Vec::<String>::new());
    }

    #[test]
    fn test_func_predecessor() {
        let func = example_func();
        let l = func.block_predecessor_list();

        assert_eq!(l.len(), 6);
        assert_eq!(*l.get("L1".into()).unwrap(), Vec::<String>::new());
        assert_eq!(*l.get("L2".into()).unwrap(), vec!["L1"]);
        assert_eq!(*l.get("L3".into()).unwrap(), vec!["L2"]);
        assert_eq!(*l.get("L4".into()).unwrap(), vec!["L2"]);
        assert_eq!(*l.get("L5".into()).unwrap(), vec!["L2"]);
        assert_eq!(*l.get("L6".into()).unwrap(), vec!["L3", "L4"]);
    }

    #[test]
    fn test_func_dominators() {
        let func = example_func();
        let doms = func.dominator_tree().adjacency_list;

        assert_eq!(doms.len(), 6);
        assert_eq!(
            *doms.get("L1".into()).unwrap(),
            HashSet::from(["L1".into()])
        );
        assert_eq!(
            *doms.get("L2".into()).unwrap(),
            HashSet::from(["L1".into(), "L2".into()])
        );
        assert_eq!(
            *doms.get("L3".into()).unwrap(),
            HashSet::from(["L1".into(), "L2".into(), "L3".into()])
        );
        assert_eq!(
            *doms.get("L4".into()).unwrap(),
            HashSet::from(["L1".into(), "L2".into(), "L4".into()])
        );
        assert_eq!(
            *doms.get("L5".into()).unwrap(),
            HashSet::from(["L1".into(), "L2".into(), "L5".into()])
        );
        assert_eq!(
            *doms.get("L6".into()).unwrap(),
            HashSet::from(["L1".into(), "L2".into(), "L6".into()])
        );
    }

    #[test]
    fn test_func_scope() {
        let func = example_func();

        let scope_l6 = func.get_scope(&"L6".into()).unwrap();
        assert_eq!(
            scope_l6,
            HashMap::from([
                ("x".into(), SmlType::Word(WordSize::W32)),
                ("x_pairity".into(), SmlType::Word(WordSize::W32)),
                ("ret".into(), SmlType::Word(WordSize::W32)),
            ])
        );
    }

    #[test]
    fn test_get_scopes() {
        let func = example_func();
        let scopes = func.get_scopes();

        assert_eq!(scopes.len(), 6);

        assert_eq!(
            scopes.get("L1").unwrap(),
            &HashMap::from([("x".into(), SmlType::Word(WordSize::W32)),])
        );

        assert_eq!(
            scopes.get("L2").unwrap(),
            &HashMap::from([
                ("x".into(), SmlType::Word(WordSize::W32)),
                ("x_pairity".into(), SmlType::Word(WordSize::W32)),
            ])
        );

        assert_eq!(
            scopes.get("L3").unwrap(),
            &HashMap::from([
                ("x".into(), SmlType::Word(WordSize::W32)),
                ("x_pairity".into(), SmlType::Word(WordSize::W32)),
            ])
        );

        assert_eq!(
            scopes.get("L4").unwrap(),
            &HashMap::from([
                ("x".into(), SmlType::Word(WordSize::W32)),
                ("x_pairity".into(), SmlType::Word(WordSize::W32)),
            ])
        );

        assert_eq!(
            scopes.get("L5").unwrap(),
            &HashMap::from([
                ("x".into(), SmlType::Word(WordSize::W32)),
                ("x_pairity".into(), SmlType::Word(WordSize::W32)),
            ])
        );

        assert_eq!(
            scopes.get("L6").unwrap(),
            &HashMap::from([
                ("x".into(), SmlType::Word(WordSize::W32)),
                ("x_pairity".into(), SmlType::Word(WordSize::W32)),
                ("ret".into(), SmlType::Word(WordSize::W32)),
            ])
        );
    }
}
