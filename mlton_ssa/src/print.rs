use std::{fmt::Display, vec};

use pretty::RcDoc;

use print_utils::*;

use crate::ssa::*;

impl PrettyDoc for RealSize {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            RealSize::R32 => RcDoc::text("r32"),
            RealSize::R64 => RcDoc::text("r64"),
        }
    }
}

impl Display for RealSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for WordSize {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            WordSize::W8 => RcDoc::text("w8"),
            WordSize::W16 => RcDoc::text("w16"),
            WordSize::W32 => RcDoc::text("w32"),
            WordSize::W64 => RcDoc::text("w64"),
        }
    }
}

impl Display for WordSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl SmlType {
    pub(crate) fn to_doc_raw(&self) -> RcDoc<'_> {
        match self {
            SmlType::Array(ty) => named_tuple_printer("Array", vec![ty.to_doc_raw()]),
            SmlType::CPointer => RcDoc::text("CPointer"),
            SmlType::Datatype(tycon) => {
                named_tuple_printer("Datatype", vec![string_printer(tycon)])
            }
            SmlType::IntInf => RcDoc::text("IntInf"),
            SmlType::Real(rs) => named_tuple_printer("Real", vec![rs.to_doc()]),
            SmlType::Ref(ty) => named_tuple_printer("Ref", vec![ty.to_doc_raw()]),
            SmlType::Thread => RcDoc::text("Thread"),
            SmlType::Tuple(tys) => {
                named_tuple_printer("Tuple", tys.iter().map(|t| t.to_doc_raw()).collect())
            }
            SmlType::Vector(ty) => named_tuple_printer("Vector", vec![ty.to_doc_raw()]),
            SmlType::Weak(ty) => named_tuple_printer("Weak", vec![ty.to_doc_raw()]),
            SmlType::Word(ws) => named_tuple_printer("Word", vec![ws.to_doc()]),
        }
    }
}

impl PrettyDoc for SmlType {
    fn to_doc(&self) -> RcDoc<'_> {
        let doc = self.to_doc_raw();
        RcDoc::text("(<")
            .append(RcDoc::softline())
            .append(doc)
            .nest(2)
            .append(RcDoc::softline())
            .append(RcDoc::text(">)"))
            .group()
    }
}

impl Display for SmlType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl CType {
    fn to_doc_raw(&self) -> RcDoc<'_> {
        match self {
            CType::CPointer => RcDoc::text("CPointer"),
            CType::Int8 => RcDoc::text("Int8"),
            CType::Int16 => RcDoc::text("Int16"),
            CType::Int32 => RcDoc::text("Int32"),
            CType::Int64 => RcDoc::text("Int64"),
            CType::Objptr => RcDoc::text("Objptr"),
            CType::Real32 => RcDoc::text("Real32"),
            CType::Real64 => RcDoc::text("Real64"),
            CType::Word8 => RcDoc::text("Word8"),
            CType::Word16 => RcDoc::text("Word16"),
            CType::Word32 => RcDoc::text("Word32"),
            CType::Word64 => RcDoc::text("Word64"),
        }
    }
}

impl PrettyDoc for CType {
    fn to_doc(&self) -> RcDoc<'_> {
        let doc = self.to_doc_raw();
        RcDoc::text("(<")
            .append(RcDoc::softline())
            .append(doc)
            .nest(2)
            .append(RcDoc::softline())
            .append(RcDoc::text(">)"))
            .group()
    }
}

impl Display for CType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Var {
    fn to_doc<'a>(&'a self) -> RcDoc<'a> {
        RcDoc::text(&self.name)
            .append(RcDoc::space())
            .append(RcDoc::text(":"))
            .append(RcDoc::space())
            .append(self.ty.to_doc())
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Const {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            Const::CSymbol() => named_object_printer("const::CSymbol", vec![]),
            Const::IntInf(i) => {
                named_object_printer("const::IntInf", vec![("const", RcDoc::text(i.to_string()))])
            }
            Const::Null => named_object_printer("const::Null", vec![]),
            Const::Real(rs, r) => named_object_printer(
                "const::Real",
                vec![("const", RcDoc::text(format!("{}:{}", r, rs)))],
            ),
            Const::Word(ws, w) => named_object_printer(
                "const::Word",
                vec![("const", RcDoc::text(format!("{:#X}:{}", w, ws)))],
            ),
            Const::WordVector(s) => named_object_printer(
                "const::WordVector",
                vec![("const", RcDoc::text(format!("\"{}\"", s)))],
            ),
        }
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for CFunctionKind {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            CFunctionKind::Pure => RcDoc::text("Pure"),
            CFunctionKind::Impure => RcDoc::text("Impure"),
            CFunctionKind::Runtime => RcDoc::text("Runtime"),
        }
    }
}

impl Display for CFunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for CFunctionTarget {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            CFunctionTarget::Direct(s) => named_object_printer(
                "target",
                vec![
                    ("type", RcDoc::text("Direct")),
                    ("name", RcDoc::text(format!("\"{}\"", s))),
                ],
            ),
            CFunctionTarget::Indirect => {
                named_object_printer("target", vec![("type", RcDoc::text("Indirect"))])
            }
        }
    }
}

impl Display for CFunctionTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for CFunctionConvention {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            CFunctionConvention::Cdecl => RcDoc::text("cdecl"),
            CFunctionConvention::Stdcall => RcDoc::text("stdcall"),
        }
    }
}

impl Display for CFunctionConvention {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for CFunctionSymbolScope {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            CFunctionSymbolScope::External => RcDoc::text("external"),
            CFunctionSymbolScope::Private => RcDoc::text("private"),
            CFunctionSymbolScope::Public => RcDoc::text("public"),
        }
    }
}

impl Display for CFunctionSymbolScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for CFunction {
    fn to_doc<'a>(&'a self) -> RcDoc<'a> {
        named_object_printer(
            "CFunction",
            vec![
                ("args", paren_list_printer(&self.args)),
                ("convention", self.convention.to_doc()),
                ("inline", RcDoc::text(self.inline.to_string())),
                ("kind", self.kind.to_doc()),
                ("prototype", named_object_printer("prototype", vec![
                    ("args", paren_list_printer(&self.prototype.0)),
                    ("res", self.prototype.1.to_doc()),
                ])),
                ("return", self.ret.to_doc()),
                ("symbolScope", self.symbol_scope.to_doc()),
                ("target", self.target.to_doc()),
            ],
        )
    }
}

impl Display for CFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Prim {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            Prim::ArrayAlloc { raw } => named_object_printer(
                "prim::ArrayAlloc",
                vec![("raw", RcDoc::text(raw.to_string()))],
            ),
            Prim::ArrayArray => named_object_printer("prim::ArrayArray", vec![]),
            Prim::ArrayCopyArray => named_object_printer("prim::ArrayCopyArray", vec![]),
            Prim::ArrayCopyVector => named_object_printer("prim::ArrayCopyVector", vec![]),
            Prim::ArrayLength => named_object_printer("prim::ArrayLength", vec![]),
            Prim::ArraySub => named_object_printer("prim::ArraySub", vec![]),
            Prim::ArrayToArray => named_object_printer("prim::ArrayToArray", vec![]),
            Prim::ArrayToVector => named_object_printer("prim::ArrayToVector", vec![]),
            Prim::ArrayUninit => named_object_printer("prim::ArrayUninit", vec![]),
            Prim::ArrayUninitIsNop => named_object_printer("prim::ArrayUninitIsNop", vec![]),
            Prim::ArrayUpdate => named_object_printer("prim::ArrayUpdate", vec![]),

            Prim::CFunction(c) => named_object_printer(
                "prim::CFunction",
                vec![("func", c.to_doc())],
            ),

            Prim::CPointerAdd => named_object_printer("prim::CPointerAdd", vec![]),
            Prim::CPointerDiff => named_object_printer("prim::CPointerDiff", vec![]),
            Prim::CPointerEqual => named_object_printer("prim::CPointerEqual", vec![]),
            Prim::CPointerFromWord => named_object_printer("prim::CPointerFromWord", vec![]),
            Prim::CPointerGetCPointer => named_object_printer("prim::CPointerGetCPointer", vec![]),
            Prim::CPointerGetObjptr => named_object_printer("prim::CPointerGetObjptr", vec![]),
            Prim::CPointerGetReal(sz) => {
                named_object_printer("prim::CPointerGetReal", vec![("size", sz.to_doc())])
            }
            Prim::CPointerGetWord(sz) => {
                named_object_printer("prim::CPointerGetWord", vec![("size", sz.to_doc())])
            }
            Prim::CPointerLt => named_object_printer("prim::CPointerLt", vec![]),
            Prim::CPointerSetCPointer => named_object_printer("prim::CPointerSetCPointer", vec![]),
            Prim::CPointerSetObjptr => named_object_printer("prim::CPointerSetObjptr", vec![]),
            Prim::CPointerSetReal(sz) => {
                named_object_printer("prim::CPointerSetReal", vec![("size", sz.to_doc())])
            }
            Prim::CPointerSetWord(sz) => {
                named_object_printer("prim::CPointerSetWord", vec![("size", sz.to_doc())])
            }
            Prim::CPointerSub => named_object_printer("prim::CPointerSub", vec![]),
            Prim::CPointerToWord => named_object_printer("prim::CPointerToWord", vec![]),

            Prim::ExnExtra => named_object_printer("prim::ExnExtra", vec![]),
            Prim::ExnName => named_object_printer("prim::ExnName", vec![]),
            Prim::ExnSetExtendExtra => named_object_printer("prim::ExnSetExtendExtra", vec![]),

            Prim::GCCollect => named_object_printer("prim::GCCollect", vec![]),
            Prim::GCState => named_object_printer("prim::GCState", vec![]),

            Prim::IntInfAdd => named_object_printer("prim::IntInfAdd", vec![]),
            Prim::IntInfAndb => named_object_printer("prim::IntInfAndb", vec![]),
            Prim::IntInfArshift => named_object_printer("prim::IntInfArshift", vec![]),
            Prim::IntInfCompare => named_object_printer("prim::IntInfCompare", vec![]),
            Prim::IntInfGcd => named_object_printer("prim::IntInfGcd", vec![]),
            Prim::IntInfLshift => named_object_printer("prim::IntInfLshift", vec![]),
            Prim::IntInfMul => named_object_printer("prim::IntInfMul", vec![]),
            Prim::IntInfNeg => named_object_printer("prim::IntInfNeg", vec![]),
            Prim::IntInfNotb => named_object_printer("prim::IntInfNotb", vec![]),
            Prim::IntInfOrb => named_object_printer("prim::IntInfOrb", vec![]),
            Prim::IntInfQuot => named_object_printer("prim::IntInfQuot", vec![]),
            Prim::IntInfRem => named_object_printer("prim::IntInfRem", vec![]),
            Prim::IntInfSub => named_object_printer("prim::IntInfSub", vec![]),
            Prim::IntInfToString => named_object_printer("prim::IntInfToString", vec![]),
            Prim::IntInfToVector => named_object_printer("prim::IntInfToVector", vec![]),
            Prim::IntInfToWord(sz) => named_object_printer("prim::IntInfToWord", vec![("size", sz.to_doc())]),
            Prim::IntInfXorb => named_object_printer("prim::IntInfXorb", vec![]),

            Prim::MLtonBogus => named_object_printer("prim::MLtonBogus", vec![]),
            Prim::MLtonBug => named_object_printer("prim::MLtonBug", vec![]),
            Prim::MLtonDeserialize => named_object_printer("prim::MLtonDeserialize", vec![]),
            Prim::MLtonEq => named_object_printer("prim::MLtonEq", vec![]),
            Prim::MLtonEqual => named_object_printer("prim::MLtonEqual", vec![]),
            Prim::MLtonHalt => named_object_printer("prim::MLtonHalt", vec![]),
            Prim::MLtonHash => named_object_printer("prim::MLtonHash", vec![]),
            Prim::MLtonHandlesSignals => named_object_printer("prim::MLtonHandlesSignals", vec![]),
            Prim::MLtonInstallSignalHandler => {
                named_object_printer("prim::MLtonInstallSignalHandler", vec![])
            }
            Prim::MLtonSerialize => named_object_printer("prim::MLtonSerialize", vec![]),
            Prim::MLtonShare => named_object_printer("prim::MLtonShare", vec![]),
            Prim::MLtonSize => named_object_printer("prim::MLtonSize", vec![]),
            Prim::MLtonTouch => named_object_printer("prim::MLtonTouch", vec![]),

            Prim::RealMathAcos(sz) => named_object_printer("prim::RealMathAcos", vec![("size", sz.to_doc())]),
            Prim::RealMathAsin(sz) => named_object_printer("prim::RealMathAsin", vec![("size", sz.to_doc())]),
            Prim::RealMathAtan(sz) => named_object_printer("prim::RealMathAtan", vec![("size", sz.to_doc())]),
            Prim::RealMathAtan2(sz) => named_object_printer("prim::RealMathAtan2", vec![("size", sz.to_doc())]),
            Prim::RealMathCos(sz) => named_object_printer("prim::RealMathCos", vec![("size", sz.to_doc())]),
            Prim::RealMathExp(sz) => named_object_printer("prim::RealMathExp", vec![("size", sz.to_doc())]),
            Prim::RealMathLn(sz) => named_object_printer("prim::RealMathLn", vec![("size", sz.to_doc())]),
            Prim::RealMathLog10(sz) => named_object_printer("prim::RealMathLog10", vec![("size", sz.to_doc())]),
            Prim::RealMathSin(sz) => named_object_printer("prim::RealMathSin", vec![("size", sz.to_doc())]),
            Prim::RealMathSqrt(sz) => named_object_printer("prim::RealMathSqrt", vec![("size", sz.to_doc())]),
            Prim::RealMathTan(sz) => named_object_printer("prim::RealMathTan", vec![("size", sz.to_doc())]),
            Prim::RealAbs(sz) => named_object_printer("prim::RealAbs", vec![("size", sz.to_doc())]),
            Prim::RealAdd(sz) => named_object_printer("prim::RealAdd", vec![("size", sz.to_doc())]),

            Prim::RealCastToWord(from, to) => named_object_printer(
                "prim::RealCastToWord",
                vec![("from", from.to_doc()), ("to", to.to_doc())],
            ),

            Prim::RealDiv(sz) => named_object_printer("prim::RealDiv", vec![("size", sz.to_doc())]),
            Prim::RealEqual(sz) => named_object_printer("prim::RealEqual", vec![("size", sz.to_doc())]),
            Prim::RealLdexp(sz) => named_object_printer("prim::RealLdexp", vec![("size", sz.to_doc())]),
            Prim::RealLe(sz) => named_object_printer("prim::RealLe", vec![("size", sz.to_doc())]),
            Prim::RealLt(sz) => named_object_printer("prim::RealLt", vec![("size", sz.to_doc())]),
            Prim::RealMul(sz) => named_object_printer("prim::RealMul", vec![("size", sz.to_doc())]),
            Prim::RealMuladd(sz) => named_object_printer("prim::RealMuladd", vec![("size", sz.to_doc())]),
            Prim::RealMulsub(sz) => named_object_printer("prim::RealMulsub", vec![("size", sz.to_doc())]),
            Prim::RealNeg(sz) => named_object_printer("prim::RealNeg", vec![("size", sz.to_doc())]),
            Prim::RealQequal(sz) => named_object_printer("prim::RealQequal", vec![("size", sz.to_doc())]),

            Prim::RealRndToReal(from, to) => named_object_printer(
                "prim::RealRndToReal",
                vec![("from", from.to_doc()), ("to", to.to_doc())],
            ),

            Prim::RealRndToWord(from, to, signed) => named_object_printer(
                "prim::RealRndToWord",
                vec![
                    ("from", from.to_doc()),
                    ("to", to.to_doc()),
                    ("signed", RcDoc::text(signed.to_string())),
                ],
            ),

            Prim::RealRound(sz) => named_object_printer("prim::RealRound", vec![("size", sz.to_doc())]),
            Prim::RealSub(sz) => named_object_printer("prim::RealSub", vec![("size", sz.to_doc())]),

            Prim::RefAssign => named_object_printer("prim::RefAssign", vec![]),
            Prim::RefDeref => named_object_printer("prim::RefDeref", vec![]),
            Prim::RefRef => named_object_printer("prim::RefRef", vec![]),
            Prim::StringToWord8Vector => named_object_printer("prim::StringToWord8Vector", vec![]),

            Prim::ThreadAtomicBegin => named_object_printer("prim::ThreadAtomicBegin", vec![]),
            Prim::ThreadAtomicEnd => named_object_printer("prim::ThreadAtomicEnd", vec![]),
            Prim::ThreadAtomicState => named_object_printer("prim::ThreadAtomicState", vec![]),
            Prim::ThreadCopy => named_object_printer("prim::ThreadCopy", vec![]),
            Prim::ThreadCopyCurrent => named_object_printer("prim::ThreadCopyCurrent", vec![]),
            Prim::ThreadReturnToC => named_object_printer("prim::ThreadReturnToC", vec![]),
            Prim::ThreadSwitchTo => named_object_printer("prim::ThreadSwitchTo", vec![]),

            Prim::TopLevelGetHandler => named_object_printer("prim::TopLevelGetHandler", vec![]),
            Prim::TopLevelGetSuffix => named_object_printer("prim::TopLevelGetSuffix", vec![]),
            Prim::TopLevelSetHandler => named_object_printer("prim::TopLevelSetHandler", vec![]),
            Prim::TopLevelSetSuffix => named_object_printer("prim::TopLevelSetSuffix", vec![]),

            Prim::VectorLength => named_object_printer("prim::VectorLength", vec![]),
            Prim::VectorSub => named_object_printer("prim::VectorSub", vec![]),
            Prim::VectorVector => named_object_printer("prim::VectorVector", vec![]),

            Prim::WeakCanGet => named_object_printer("prim::WeakCanGet", vec![]),
            Prim::WeakGet => named_object_printer("prim::WeakGet", vec![]),
            Prim::WeakNew => named_object_printer("prim::WeakNew", vec![]),

            Prim::WordAdd(sz) => named_object_printer("prim::WordAdd", vec![("size", sz.to_doc())]),
            Prim::WordAddCheckP(sz, signed) => named_object_printer(
                "prim::WordAddCheckP",
                vec![("size", sz.to_doc()), ("signed", RcDoc::text(signed.to_string()))],
            ),
            Prim::WordAndb(sz) => named_object_printer("prim::WordAndb", vec![("size", sz.to_doc())]),
            Prim::WordCastToReal(from, to) => named_object_printer(
                "prim::WordCastToReal",
                vec![("from", from.to_doc()), ("to", to.to_doc())],
            ),
            Prim::WordEqual(sz) => named_object_printer("prim::WordEqual", vec![("size", sz.to_doc())]),
            Prim::WordExtdToWord(from, to, signed) => named_object_printer(
                "prim::WordExtdToWord",
                vec![
                    ("from", from.to_doc()),
                    ("to", to.to_doc()),
                    ("signed", RcDoc::text(signed.to_string())),
                ],
            ),
            Prim::WordLshift(sz) => named_object_printer("prim::WordLshift", vec![("size", sz.to_doc())]),
            Prim::WordLt(sz, signed) => named_object_printer(
                "prim::WordLt",
                vec![("size", sz.to_doc()), ("signed", RcDoc::text(signed.to_string()))],
            ),
            Prim::WordMul(sz, signed) => named_object_printer(
                "prim::WordMul",
                vec![("size", sz.to_doc()), ("signed", RcDoc::text(signed.to_string()))],
            ),
            Prim::WordMulCheckP(sz, signed) => named_object_printer(
                "prim::WordMulCheckP",
                vec![("size", sz.to_doc()), ("signed", RcDoc::text(signed.to_string()))],
            ),
            Prim::WordNeg(sz) => named_object_printer("prim::WordNeg", vec![("size", sz.to_doc())]),
            Prim::WordNegCheckP(sz) => named_object_printer("prim::WordNegCheckP", vec![("size", sz.to_doc())]),
            Prim::WordNotb(sz) => named_object_printer("prim::WordNotb", vec![("size", sz.to_doc())]),
            Prim::WordOrb(sz) => named_object_printer("prim::WordOrb", vec![("size", sz.to_doc())]),

            Prim::WordQuot(sz, signed) => named_object_printer(
                "prim::WordQuot",
                vec![("size", sz.to_doc()), ("signed", RcDoc::text(signed.to_string()))],
            ),
            Prim::WordRem(sz, signed) => named_object_printer(
                "prim::WordRem",
                vec![("size", sz.to_doc()), ("signed", RcDoc::text(signed.to_string()))],
            ),
            Prim::WordRndToReal(from, to, signed) => named_object_printer(
                "prim::WordRndToReal",
                vec![
                    ("from", from.to_doc()),
                    ("to", to.to_doc()),
                    ("signed", RcDoc::text(signed.to_string())),
                ],
            ),
            Prim::WordRol(sz) => named_object_printer("prim::WordRol", vec![("size", sz.to_doc())]),
            Prim::WordRor(sz) => named_object_printer("prim::WordRor", vec![("size", sz.to_doc())]),
            Prim::WordRshift(sz, signed) => named_object_printer(
                "prim::WordRshift",
                vec![("size", sz.to_doc()), ("signed", RcDoc::text(signed.to_string()))],
            ),
            Prim::WordSub(sz) => named_object_printer("prim::WordSub", vec![("size", sz.to_doc())]),
            Prim::WordSubCheckP(sz, signed) => named_object_printer(
                "prim::WordSubCheckP",
                vec![("size", sz.to_doc()), ("signed", RcDoc::text(signed.to_string()))],
            ),

            Prim::WordToIntInf => named_object_printer("prim::WordToIntInf", vec![]),
            Prim::WordXorb(sz) => named_object_printer("prim::WordXorb", vec![("size", sz.to_doc())]),
            Prim::WordVectorToIntInf => named_object_printer("prim::WordVectorToIntInf", vec![]),

            Prim::WordArraySubWord { seq_size, ele_size } => named_object_printer(
                "prim::WordArraySubWord",
                vec![
                    ("seq_size", seq_size.to_doc()),
                    ("ele_size", ele_size.to_doc()),
                ],
            ),

            Prim::WordArrayUpdateWord { seq_size, ele_size } => named_object_printer(
                "prim::WordArrayUpdateWord",
                vec![
                    // follow the existing printer comment which used seqSize/eleSize
                    ("seqSize", seq_size.to_doc()),
                    ("eleSize", ele_size.to_doc()),
                ],
            ),

            Prim::WordVectorSubWord { seq_size, ele_size } => named_object_printer(
                "prim::WordVectorSubWord",
                vec![
                    ("seqSize", seq_size.to_doc()),
                    ("eleSize", ele_size.to_doc()),
                ],
            ),

            Prim::Word8VectorToString => named_object_printer("prim::Word8VectorToString", vec![]),
            Prim::WorldSave => named_object_printer("prim::WorldSave", vec![]),
        }
    }
}

impl Display for Prim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Exp {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            Exp::ConApp { con, args } => named_object_printer(
                "exp::ConApp",
                vec![
                    ("con", string_printer(con)),
                    ("args", paren_string_list_printer(args)),
                ],
            ),
            Exp::PrimApp { prim, targs, args } => named_object_printer(
                "exp::PrimApp",
                match targs {
                    None => vec![
                        ("prim", prim.to_doc()),
                        ("args", paren_string_list_printer(args)),
                    ],
                    Some(ts) => vec![
                        ("prim", prim.to_doc()),
                        ("args", paren_string_list_printer(args)),
                        ("targs", paren_list_printer(ts)),
                    ],
                },
            ),
            Exp::Const(c) => named_object_printer("exp::Const", vec![("const", c.to_doc())]),
            Exp::Profile() => named_object_printer("exp::Profile", vec![]),
            Exp::Select { tuple, offset } => named_object_printer(
                "exp::Select",
                vec![
                    ("tuple", tuple.to_doc()),
                    ("offset", RcDoc::text(offset.to_string())),
                ],
            ),
            Exp::Tuple(items) => {
                named_object_printer("exp::Tuple", vec![("args", paren_list_printer(items))])
            }
            Exp::Var(v) => named_object_printer("exp::Var", vec![("var", v.to_doc())]),
        }
    }
}

impl Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Statement {
    fn to_doc(&self) -> RcDoc<'_> {
        named_object_printer(
            "statement",
            vec![
                ("var", self.var.to_doc()),
                ("type", self.ty.to_doc()),
                ("exp", self.exp.to_doc()),
            ],
        )
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Handler {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            Handler::Caller => named_object_printer("handler::Caller", vec![]),
            Handler::Dead => named_object_printer("handler::Dead", vec![]),
            Handler::Handle { label } => {
                named_object_printer("handler::Handle", vec![("label", label.to_doc())])
            }
        }
    }
}

impl Display for Handler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Cases {
    fn to_doc<'a>(&'a self) -> RcDoc<'a> {
        match self {
            Cases::Con(items) => RcDoc::text("(")
                .append(RcDoc::softline())
                .append(
                    RcDoc::intersperse(
                        items
                            .iter()
                            .map(|(con, lbl)| RcDoc::text(format!("{} => {}", con, lbl))),
                        RcDoc::text(",").append(RcDoc::line()),
                    )
                    .nest(2),
                )
                .append(RcDoc::softline())
                .append(RcDoc::text(")"))
                .group(),
            Cases::Word(word_size, items) => RcDoc::text("(")
                .append(RcDoc::softline())
                .append(
                    RcDoc::intersperse(
                        items.iter().map(|(w, lbl)| {
                            RcDoc::text(format!("{:#x}:{} => {}", w, word_size, lbl))
                        }),
                        RcDoc::text(",").append(RcDoc::line()),
                    )
                    .nest(2),
                )
                .append(RcDoc::softline())
                .append(RcDoc::text(")"))
                .group(),
        }
    }
}

impl Display for Cases {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Transfer {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            Transfer::Bug => named_object_printer("transfer::Bug", vec![]),
            Transfer::Call { func, args, ret } => match ret {
                Return::Dead => named_object_printer(
                    "transfer::call::Dead",
                    vec![("func", func.to_doc()), ("args", paren_list_printer(args))],
                ),
                Return::NonTail { cont, handler } => named_object_printer(
                    "transfer::call::NonTail",
                    vec![
                        ("func", func.to_doc()),
                        ("args", paren_list_printer(args)),
                        ("cont", RcDoc::text(cont.clone())),
                        ("handler", handler.to_doc()),
                    ],
                ),
                Return::Tail => named_object_printer(
                    "transfer::call::Tail",
                    vec![("func", func.to_doc()), ("args", paren_list_printer(args))],
                ),
            },
            Transfer::Case {
                test,
                cases,
                default,
            } => {
                let (obj_name, mut key_values) = match cases {
                    Cases::Con(_) => (
                        "transfer::case::Con",
                        vec![("test", test.to_doc()), ("cases", cases.to_doc())],
                    ),
                    Cases::Word(ws, _) => (
                        "transfer::case::Word",
                        vec![
                            ("test", test.to_doc()),
                            ("size", ws.to_doc()),
                            ("cases", cases.to_doc()),
                        ],
                    ),
                };
                if let Some(d) = default {
                    key_values.push(("default", RcDoc::text(d)));
                }
                named_object_printer(obj_name, key_values)
            }
            Transfer::Goto { dst, args } => named_object_printer(
                "transfer::Goto",
                vec![
                    ("dst", RcDoc::text(dst.clone())),
                    ("args", paren_list_printer(args)),
                ],
            ),
            Transfer::Raise { args } => {
                named_object_printer("transfer::Raise", vec![("args", paren_list_printer(args))])
            }
            Transfer::Return { args } => {
                named_object_printer("transfer::Return", vec![("args", paren_list_printer(args))])
            }
            Transfer::Runtime => todo!(),
        }
    }
}

impl Display for Transfer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Block {
    fn to_doc(&self) -> RcDoc<'_> {
        named_object_printer(
            "block",
            vec![
                ("label", RcDoc::text(self.label.clone())),
                ("args", paren_list_printer(&self.args)),
                ("statements", paren_list_printer(&self.statements)),
                ("transfer", self.transfer.to_doc()),
            ],
        )
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Datatype {
    fn to_doc(&self) -> RcDoc<'_> {
        fn constr_printer<'a>(constr: &'a (String, Vec<SmlType>)) -> RcDoc<'a> {
            RcDoc::text("(")
                .append(RcDoc::softline())
                .append(RcDoc::text(&constr.0))
                .append(if constr.1.is_empty() {
                    RcDoc::nil()
                } else {
                    RcDoc::text(",")
                        .append(RcDoc::space())
                        .append(paren_list_printer(&constr.1))
                        .append(RcDoc::space())
                        .nest(2)
                })
                .append(RcDoc::softline())
                .append(RcDoc::text(")"))
                .group()
        }

        named_object_printer(
            "datatype",
            vec![
                ("tycon", string_printer(&self.tycon)),
                (
                    "cons",
                    RcDoc::text("(")
                        .append(RcDoc::softline())
                        .append(
                            RcDoc::intersperse(
                                self.constrs.iter().map(constr_printer),
                                RcDoc::text(",").append(RcDoc::line()),
                            )
                            .nest(2),
                        )
                        .append(RcDoc::softline())
                        .append(RcDoc::text(")"))
                        .group(),
                ),
            ],
        )
    }
}

impl Display for Datatype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Function {
    fn to_doc(&self) -> RcDoc<'_> {
        named_object_printer(
            "function",
            vec![
                ("name", string_printer(&self.name)),
                ("mayInline", self.may_inline.to_doc()),
                ("args", paren_list_printer(&self.args)),
                ("start", RcDoc::text(self.start.clone())),
                ("returns", option_printer(&self.returns)),
                ("raises", option_printer(&self.raises)),
                ("blocks", paren_list_printer(&self.blocks)),
            ],
        )
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for MltonSsa {
    fn to_doc(&self) -> RcDoc<'_> {
        named_object_printer(
            "mltonssa",
            vec![
                (
                    "datatypes",
                    RcDoc::text("(")
                        .append(RcDoc::softline())
                        .append(
                            RcDoc::intersperse(
                                self.datatypes.iter().map(|d| d.to_doc()),
                                RcDoc::text(",").append(RcDoc::line()),
                            )
                            .nest(2),
                        )
                        .append(RcDoc::softline())
                        .append(RcDoc::text(")"))
                        .group(),
                ),
                (
                    "globals",
                    RcDoc::text("(")
                        .append(RcDoc::softline())
                        .append(
                            RcDoc::intersperse(
                                self.globals.iter().map(|g| g.to_doc()),
                                RcDoc::text(",").append(RcDoc::line()),
                            )
                            .nest(2),
                        )
                        .append(RcDoc::softline())
                        .append(RcDoc::text(")"))
                        .group(),
                ),
                (
                    "functions",
                    RcDoc::text("(")
                        .append(RcDoc::softline())
                        .append(
                            RcDoc::intersperse(
                                self.functions.iter().map(|f| f.to_doc()),
                                RcDoc::text(",").append(RcDoc::line()),
                            )
                            .nest(2),
                        )
                        .append(RcDoc::softline())
                        .append(RcDoc::text(")"))
                        .group(),
                ),
                ("main", RcDoc::text(&self.main)),
            ],
        )
    }
}

impl Display for MltonSsa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

#[cfg(test)]
mod tests {
    use ordered_float::OrderedFloat;

    use super::*;

    #[test]
    fn test_print_real_size() {
        assert_eq!(pretty_print(&RealSize::R32), "r32");
        assert_eq!(pretty_print(&RealSize::R64), "r64");
    }

    #[test]
    fn test_print_real_size_inverse() {
        let r32: RealSize = RealSize::R32;
        let r64: RealSize = RealSize::R64;
        assert_eq!(r32.to_string().parse(), Ok(r32));
        assert_eq!(r64.to_string().parse(), Ok(r64));
    }

    #[test]
    fn test_print_word_size() {
        assert_eq!(pretty_print(&WordSize::W8), "w8");
        assert_eq!(pretty_print(&WordSize::W16), "w16");
        assert_eq!(pretty_print(&WordSize::W32), "w32");
        assert_eq!(pretty_print(&WordSize::W64), "w64");
    }

    #[test]
    fn test_print_word_size_inverse() {
        let w8: WordSize = WordSize::W8;
        let w16: WordSize = WordSize::W16;
        let w32: WordSize = WordSize::W32;
        let w64: WordSize = WordSize::W64;
        assert_eq!(w8.to_string().parse(), Ok(w8));
        assert_eq!(w16.to_string().parse(), Ok(w16));
        assert_eq!(w32.to_string().parse(), Ok(w32));
        assert_eq!(w64.to_string().parse(), Ok(w64));
    }

    #[test]
    fn test_print_sml_type() {
        let t1 = SmlType::Array(Box::new(SmlType::Word(WordSize::W32)));
        assert_eq!(pretty_print(&t1), "(< Array ( Word ( w32 ) ) >)");

        let t2 = SmlType::Ref(Box::new(SmlType::Datatype("MyType".to_owned())));
        assert_eq!(pretty_print(&t2), "(< Ref ( Datatype ( \"MyType\" ) ) >)");

        let t3 = SmlType::Tuple(vec![
            SmlType::IntInf,
            SmlType::Real(RealSize::R64),
            SmlType::Vector(Box::new(SmlType::Word(WordSize::W8))),
        ]);
        assert_eq!(
            pretty_print(&t3),
            "(< Tuple ( IntInf, Real ( r64 ), Vector ( Word ( w8 ) ) ) >)"
        );
    }

    #[test]
    fn test_print_sml_type_inverse() {
        let t1: SmlType = SmlType::Array(Box::new(SmlType::Word(WordSize::W32)));
        assert_eq!(t1.to_string().parse(), Ok(t1));

        let t2: SmlType = SmlType::Ref(Box::new(SmlType::Datatype("MyType".to_owned())));
        assert_eq!(t2.to_string().parse(), Ok(t2));
    }

    #[test]
    fn test_print_c_type() {
        let c1 = CType::CPointer;
        assert_eq!(pretty_print(&c1), "(< CPointer >)");
    }

    #[test]
    fn test_print_c_type_inverse() {
        let c1: CType = CType::CPointer;
        assert_eq!(c1.to_string().parse(), Ok(c1));
    }

    #[test]
    fn test_print_var() {
        let v = Var {
            name: "x".to_owned(),
            ty: SmlType::Word(WordSize::W64),
        };
        assert_eq!(pretty_print(&v), "x : (< Word ( w64 ) >)");
    }

    #[test]
    fn test_print_var_inverse() {
        let v = Var {
            name: "x".to_owned(),
            ty: SmlType::Word(WordSize::W64),
        };
        println!("{}", v.to_string());
        assert_eq!(v.to_string().parse(), Ok(v));
    }

    #[test]
    fn test_print_const() {
        let c1 = Const::IntInf(42);
        assert_eq!(pretty_print(&c1), "const::IntInf {\n  const = 42\n}");

        let c2 = Const::Real(RealSize::R32, OrderedFloat::<f64>(3.14));
        assert_eq!(pretty_print(&c2), "const::Real {\n  const = 3.14:r32\n}");

        let c3 = Const::Word(WordSize::W16, 255);
        assert_eq!(pretty_print(&c3), "const::Word {\n  const = 0xFF:w16\n}");
    }

    #[test]
    fn test_print_const_inverse() {
        let c2 = Const::Real(RealSize::R32, OrderedFloat::<f64>(3.14));
        assert_eq!(c2.to_string().parse(), Ok(c2));

        let c3 = Const::Word(WordSize::W16, 255);
        assert_eq!(c3.to_string().parse(), Ok(c3));

        let c4 = Const::WordVector("hello".to_owned());
        assert_eq!(c4.to_string().parse(), Ok(c4));
    }

    #[test]
    fn test_print_c_function_kind() {
        assert_eq!(pretty_print(&CFunctionKind::Pure), "Pure");
        assert_eq!(pretty_print(&CFunctionKind::Impure), "Impure");
        assert_eq!(pretty_print(&CFunctionKind::Runtime), "Runtime");
    }

    #[test]
    fn test_print_c_function_kind_inverse() {
        let k1: CFunctionKind = CFunctionKind::Pure;
        let k2: CFunctionKind = CFunctionKind::Impure;
        let k3: CFunctionKind = CFunctionKind::Runtime;
        assert_eq!(k1.to_string().parse(), Ok(k1));
        assert_eq!(k2.to_string().parse(), Ok(k2));
        assert_eq!(k3.to_string().parse(), Ok(k3));
    }

    #[test]
    fn test_print_c_function_target() {
        let c1 = CFunctionTarget::Direct("my_function".to_owned());
        assert_eq!(
            pretty_print(&c1),
            "target {\n  type = Direct,\n  name = \"my_function\"\n}"
        );

        let c2 = CFunctionTarget::Indirect;
        assert_eq!(pretty_print(&c2), "target {\n  type = Indirect\n}");
    }

    #[test]
    fn test_print_c_function_target_inverse() {
        let c1 = CFunctionTarget::Direct("my_function".to_owned());
        assert_eq!(c1.to_string().parse(), Ok(c1));
        let c2 = CFunctionTarget::Indirect;
        assert_eq!(c2.to_string().parse(), Ok(c2));
    }

    #[test]
    fn test_print_c_function_convention() {
        assert_eq!(pretty_print(&CFunctionConvention::Cdecl), "cdecl");
        assert_eq!(pretty_print(&CFunctionConvention::Stdcall), "stdcall");
    }

    #[test]
    fn test_print_c_function_convention_inverse() {
        let c1: CFunctionConvention = CFunctionConvention::Cdecl;
        let c2: CFunctionConvention = CFunctionConvention::Stdcall;
        assert_eq!(c1.to_string().parse(), Ok(c1));
        assert_eq!(c2.to_string().parse(), Ok(c2));
    }

    #[test]
    fn test_print_c_function_symbol_scope() {
        assert_eq!(pretty_print(&CFunctionSymbolScope::External), "external");
        assert_eq!(pretty_print(&CFunctionSymbolScope::Private), "private");
        assert_eq!(pretty_print(&CFunctionSymbolScope::Public), "public");
    }

    #[test]
    fn test_print_c_function_symbol_scope_inverse() {
        let c1: CFunctionSymbolScope = CFunctionSymbolScope::External;
        let c2: CFunctionSymbolScope = CFunctionSymbolScope::Private;
        let c3: CFunctionSymbolScope = CFunctionSymbolScope::Public;
        assert_eq!(c1.to_string().parse(), Ok(c1));
        assert_eq!(c2.to_string().parse(), Ok(c2));
        assert_eq!(c3.to_string().parse(), Ok(c3));
    }

    #[test]
    fn test_cfunction_inverse() {
        let cfunc = CFunction {
                args: vec![SmlType::Vector(Box::new(SmlType::Word(WordSize::W8)))],
                convention: CFunctionConvention::Cdecl,
                inline: false,
                kind: CFunctionKind::Impure,
                prototype: (vec![CType::Objptr], None),
                ret: SmlType::Tuple(vec![]),
                symbol_scope: CFunctionSymbolScope::Private,
                target: CFunctionTarget::Direct("Stdio_print".to_string()),
            };
        println!("{}", cfunc.to_string());
        assert_eq!(cfunc.to_string().parse(), Ok(cfunc));
    }

    #[test]
    fn test_prim_inverse() {
        let p1 = Prim::WordEqual(WordSize::W32);
        assert_eq!(p1.to_string().parse(), Ok(p1));

        let p2 = Prim::RealRndToWord(RealSize::R64, WordSize::W64, true);
        assert_eq!(p2.to_string().parse(), Ok(p2));

        let p3 = Prim::CFunction(CFunction {
            args: vec![SmlType::IntInf, SmlType::Word(WordSize::W8)],
            convention: CFunctionConvention::Cdecl,
            inline: true,
            kind: CFunctionKind::Impure,
            prototype: (vec![
                CType::CPointer,
                CType::Word64,
            ], Some(CType::Word64)),
            ret: SmlType::Real(RealSize::R32),
            symbol_scope: CFunctionSymbolScope::Public,
            target: CFunctionTarget::Direct("my_c_func".to_string()),
        });
        assert_eq!(p3.to_string().parse(), Ok(p3));

        let p4 = Prim::WordArrayUpdateWord {
            seq_size: WordSize::W32,
            ele_size: WordSize::W16,
        };
        assert_eq!(p4.to_string().parse(), Ok(p4));

        let p5 = Prim::WordExtdToWord(WordSize::W8, WordSize::W32, false);
        assert_eq!(p5.to_string().parse(), Ok(p5));

        // Non-exhaustive
    }

    #[test]
    fn test_print_exp_inverse() {
        let exp = Exp::Var("x_0".into());
        assert_eq!(exp.to_string().parse(), Ok(exp));

        let exp = Exp::Tuple(vec!["global_15".to_string(), "global_14".to_string()]);
        assert_eq!(exp.to_string().parse(), Ok(exp));

        let exp = Exp::Select {
            tuple: "x_108".to_string(),
            offset: 0,
        };
        assert_eq!(exp.to_string().parse(), Ok(exp));

        let exp = Exp::ConApp {
            con: "::_1".to_string(),
            args: vec!["global_43".to_string(), "x_49".to_string()],
        };
        assert_eq!(exp.to_string().parse(), Ok(exp));

        let exp = Exp::Const(Const::Null);
        assert_eq!(exp.to_string().parse(), Ok(exp));
    }

    #[test]
    fn test_print_statement_inverse() {
        let stmt = Statement {
            var: None,
            ty: SmlType::Vector(Box::new(SmlType::Word(WordSize::W8))),
            exp: Exp::Const(crate::ssa::Const::WordVector(
                "unhandled exception: ".to_string(),
            )),
        };
        assert_eq!(stmt.to_string().parse(), Ok(stmt));

        let stmt = Statement {
            var: Some("global_0".to_string()),
            ty: SmlType::Vector(Box::new(SmlType::Word(WordSize::W8))),
            exp: Exp::Const(crate::ssa::Const::WordVector(
                "unhandled exception: ".to_string(),
            )),
        };
        assert_eq!(stmt.to_string().parse(), Ok(stmt));
    }

    #[test]
    fn test_print_handler_inverse() {
        let h1 = Handler::Caller;
        assert_eq!(h1.to_string().parse(), Ok(h1));
        let h2 = Handler::Dead;
        assert_eq!(h2.to_string().parse(), Ok(h2));
        let h3 = Handler::Handle {
            label: "my_label".to_string(),
        };
        println!("{}", h3.to_string());
        assert_eq!(h3.to_string().parse(), Ok(h3));
    }

    #[test]
    fn test_print_cases_inverse() {
        let c1 = Cases::Con(vec![
            ("::_1".to_string(), "L1".to_string()),
            ("::_2".to_string(), "L2".to_string()),
        ]);
        assert_eq!(c1.to_string().parse(), Ok(c1));

        let c2 = Cases::Word(
            WordSize::W8,
            vec![(0, "L1".to_string()), (255, "L2".to_string())],
        );
        assert_eq!(c2.to_string().parse(), Ok(c2));
    }

    #[test]
    fn test_print_transfer_inverse() {
        let t1 = Transfer::Bug;
        assert_eq!(t1.to_string().parse(), Ok(t1));

        let t2 = Transfer::Call {
            func: "f".to_string(),
            args: vec!["x".to_string(), "y".to_string()],
            ret: Return::Dead,
        };
        assert_eq!(t2.to_string().parse(), Ok(t2));

        let t3 = Transfer::Case {
            test: "x".to_string(),
            cases: Cases::Con(vec![
                ("::_1".to_string(), "L1".to_string()),
                ("::_2".to_string(), "L2".to_string()),
            ]),
            default: Some("L3".to_string()),
        };
        assert_eq!(t3.to_string().parse(), Ok(t3));

        let t4 = Transfer::Case {
            test: "x".to_string(),
            cases: Cases::Word(
                WordSize::W8,
                vec![(0, "L1".to_string()), (255, "L2".to_string())],
            ),
            default: None,
        };
        assert_eq!(t4.to_string().parse(), Ok(t4));

        let t5 = Transfer::Goto {
            dst: "L1".to_string(),
            args: vec!["x".to_string(), "y".to_string()],
        };
        assert_eq!(t5.to_string().parse(), Ok(t5));

        let t6 = Transfer::Raise {
            args: vec!["x".to_string(), "y".to_string()],
        };
        assert_eq!(t6.to_string().parse(), Ok(t6));

        let t7 = Transfer::Return {
            args: vec!["x".to_string(), "y".to_string()],
        };
        assert_eq!(t7.to_string().parse(), Ok(t7));
    }

    #[test]
    fn test_print_block_inverse() {
        let b = Block {
            label: "L57".to_string(),
            args: vec![Var {
                name: "x_37".to_string(),
                ty: SmlType::Vector(Box::new(SmlType::Word(WordSize::W8))),
            }],
            statements: vec![
                Statement {
                    var: None,
                    ty: SmlType::Tuple(vec![]),
                    exp: Exp::PrimApp {
                        prim: Prim::CFunction (
                            CFunction {
                                args: vec![SmlType::Vector(Box::new(SmlType::Word(WordSize::W8)))],
                                convention: CFunctionConvention::Cdecl,
                                inline: false,
                                kind: CFunctionKind::Impure,
                                prototype: (vec![CType::Objptr], None),
                                ret: SmlType::Tuple(vec![]),
                                symbol_scope: CFunctionSymbolScope::Private,
                                target: CFunctionTarget::Direct("Stdio_print".to_string()),
                            },
                        ),
                        targs: None,
                        args: vec!["global_37".to_string()],
                    },
                },
                Statement {
                    var: Some("global_0".to_string()),
                    ty: SmlType::Vector(Box::new(SmlType::Word(WordSize::W8))),
                    exp: Exp::Const(crate::ssa::Const::WordVector(
                        "unhandled exception: ".to_string(),
                    )),
                },
            ],
            transfer: Transfer::Bug,
        };
        assert_eq!(b.to_string().parse(), Ok(b));
    }

    #[test]
    fn test_print_function_inverse() {
        let f = Function {
            name: "main_0".to_string(),
            may_inline: false,
            args: vec![],
            start: "L_52".to_string(),
            returns: None,
            raises: None,
            blocks: vec![Block {
                label: "L_52".to_string(),
                args: vec![],
                statements: vec![],
                transfer: Transfer::Return { args: vec![] },
            }],
        };
        assert_eq!(f.to_string().parse(), Ok(f));
    }

    #[test]
    fn test_print_datatype_inverse() {
        let dt = Datatype {
            tycon: "list_0".to_string(),
            constrs: vec![
                (
                    "::_0".to_string(),
                    vec![
                        SmlType::Datatype("list_0".to_string()),
                        SmlType::Tuple(vec![
                            SmlType::Word(WordSize::W8),
                            SmlType::Word(WordSize::W8),
                        ]),
                    ],
                ),
                ("nil_1".to_string(), vec![]),
            ],
        };
        assert_eq!(dt.to_string().parse(), Ok(dt));
    }

    #[test]
    fn test_print_mlton_ssa_inverse() {
        let s = r#"mltonssa {
        datatypes = (
            datatype {tycon = "list_4", cons = (( dummy_0 ))},
            datatype {tycon = "list_3",
                      cons = (( nil_0 ),
                              ( ::_2, ((< Datatype( "list_3" ) >)) ))}
        ),
        globals = (
            statement {var = Some global_0,
                       type = (< Vector( Word( w8 ) ) >),
                       exp = exp::Const {const = const::WordVector {const = "unhandled exception: "}}},
            statement {var = Some global_1,
                       type = (< Vector( Word( w8 ) ) >),
                       exp = exp::Const {const = const::WordVector {const = "Overflow"}}}
        ),
        functions = (
            function {name = "main_0",
                      mayInline = false,
                      args = (),
                      start = L_52,
                      returns = None,
                      raises = None,
                      blocks = (block {label = L_52,
                                       args = (),
                                       statements = (),
                                       transfer = transfer::Goto {dst = loop_5,
                                                                  args = (global_41,
                                                                          global_8,
                                                                          global_9)}})},
            function {name = "exit_0",
                      mayInline = true,
                      args = (x_3: (< Word( w32 ) >),
                              x_2: (< Array( Word( w8 ) ) >),
                              x_1: (< Ref( Datatype( "bool" ) ) >),
                              x_0: (< Ref( Datatype( "bool" ) ) >)),
                      start = L_0,
                      returns = None,
                      raises = Some (),
                      blocks = (block {label = L_0,
                                       args = (),
                                       statements = (statement {var = Some x_76,
                                                                type = (< Datatype( "bool" ) >),
                                                                exp = exp::PrimApp {prim = prim::RefDeref {},
                                                                                    args = (x_0),
                                                                                    targs = ((< Datatype( "bool" ) >))}}),
                                       transfer = transfer::case::Con {test = x_76,
                                                                       cases =   (true => L_51,
                                                                                  false => L_50)}})}
        ),
        main = main_0,
        }"#;
        let ssa: MltonSsa = s.parse().unwrap();
        assert_eq!(ssa.to_string().parse(), Ok(ssa));
    }
}
