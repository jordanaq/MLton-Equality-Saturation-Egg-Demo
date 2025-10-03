use std::fmt::format;
use std::rc::Rc;
use std::{fmt::Display, vec};

use pretty::RcDoc;

use print_utils::*;

use crate::parse::*;
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
            SmlType::Datatype(tycon) => named_tuple_printer("Datatype", vec![string_printer(tycon)]),
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
                vec![("const", RcDoc::text(format!("{:#x}:{}", w, ws)))],
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

impl PrettyDoc for PrimKind {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            PrimKind::DependsOnState => RcDoc::text("DependsOnState"),
            PrimKind::Functional => RcDoc::text("Functional"),
            PrimKind::Moveable => RcDoc::text("Moveable"),
            PrimKind::SideEffect => RcDoc::text("SideEffect"),
        }
    }
}

impl Display for PrimKind {
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

impl PrettyDoc for PrimPrimitive {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            PrimPrimitive::CFunction {
                args,
                convention,
                inline,
                kind,
                prototype,
                ret,
                symbol_scope,
                target,
            } => named_object_printer(
                "CFunction",
                vec![
                    ("args", paren_list_printer(&args)),
                    ("convention", convention.to_doc()),
                    ("inline", RcDoc::text(inline.to_string())),
                    ("kind", kind.to_doc()),
                    (
                        "prototype",
                        named_object_printer(
                            "prototype",
                            vec![
                                ("args", paren_list_printer(&prototype.0)),
                                ("res", prototype.1.to_doc()),
                            ],
                        ),
                    ),
                    ("return", ret.to_doc()),
                    ("symbolScope", symbol_scope.to_doc()),
                    ("target", target.to_doc()),
                ],
            ),
            PrimPrimitive::SmlPrim(prim) => string_printer(prim),
        }
    }
}

impl Display for PrimPrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for Prim {
    fn to_doc(&self) -> RcDoc<'_> {
        named_object_printer(
            "primitive",
            vec![("prim", self.prim.to_doc()), ("kind", self.kind.to_doc())],
        )
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
                        ("targs", paren_list_printer(&ts)),
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
            Handler::Handle { label } =>
                named_object_printer("handler::Handle", vec![("label", label.to_doc())]),
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
            Cases::Con(items) =>
                RcDoc::text("(")
                    .append(RcDoc::softline())
                    .append(RcDoc::intersperse(
                        items.iter().map(|(con, lbl)| {
                            RcDoc::text(format!("{} => {}", con, lbl))
                        }),
                        RcDoc::text(",").append(RcDoc::line()),
                    ).nest(2))
                    .append(RcDoc::softline())
                    .append(RcDoc::text(")"))
                    .group(),
            Cases::Word(word_size, items) =>
                RcDoc::text("(")
                    .append(RcDoc::softline())
                    .append(RcDoc::intersperse(
                        items.iter().map(|(w, lbl)| {
                            RcDoc::text(format!("{:#x}:{} => {}", w, word_size, lbl))
                        }),
                        RcDoc::text(",").append(RcDoc::line()),
                    ).nest(2))
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
            Transfer::Call { func, args, ret } => 
                match ret {
                    Return::Dead =>
                        named_object_printer(
                            "transfer::call::Dead",
                            vec![("func", func.to_doc()), ("args", paren_list_printer(args))]
                        ),
                    Return::NonTail { cont, handler } =>
                        named_object_printer(
                            "transfer::call::NonTail",
                            vec![
                                ("func", func.to_doc()),
                                ("args", paren_list_printer(&args)),
                                ("cont", RcDoc::text(cont.clone())),
                                ("handler", handler.to_doc()),
                            ]
                        ),
                    Return::Tail =>
                        named_object_printer(
                            "transfer::call::Tail",
                            vec![("func", func.to_doc()), ("args", paren_list_printer(args))]
                        ),
                }
            Transfer::Case { test, cases, default } => {
                let (obj_name, mut key_values)  = match cases {
                    Cases::Con(_) => ("transfer::case::Con", vec![("test", test.to_doc()), ("cases", cases.to_doc())]),
                    Cases::Word(ws, _) => ("transfer::case::Word", vec![("test", test.to_doc()), ("size", ws.to_doc()), ("cases", cases.to_doc())]),
                };
                if let Some(d) = default {
                    key_values.push(("default", RcDoc::text(d)));
                }
                named_object_printer(
                    obj_name,
                    key_values
                )
            },
            Transfer::Goto { dst, args } =>
                named_object_printer(
                    "transfer::Goto",
                    vec![
                        ("dst", RcDoc::text(dst.clone())),
                        ("args", paren_list_printer(args)),
                    ]
                ),
            Transfer::Raise { args } => 
                named_object_printer(
                    "transfer::Raise",
                    vec![("args", paren_list_printer(args))]
                ),
            Transfer::Return { args } => 
                named_object_printer(
                    "transfer::Return",
                    vec![("args", paren_list_printer(args))]
                ),
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
                .append(RcDoc::text(","))
                .append(RcDoc::space())
                .append(paren_list_printer(&constr.1))
                .nest(2)
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
                        .append(RcDoc::intersperse(
                            self.constrs.iter().map(constr_printer),
                            RcDoc::text(",").append(RcDoc::line()),
                        ).nest(2))
                        .append(RcDoc::softline())
                        .append(RcDoc::text(")"))
                        .group()
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

#[cfg(test)]
mod tests {
    use crate::print;

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

        let c2 = Const::Real(RealSize::R32, 3.14);
        assert_eq!(pretty_print(&c2), "const::Real {\n  const = 3.14:r32\n}");

        let c3 = Const::Word(WordSize::W16, 255);
        assert_eq!(pretty_print(&c3), "const::Word {\n  const = 0xff:w16\n}");
    }

    #[test]
    fn test_print_const_inverse() {
        let c2 = Const::Real(RealSize::R32, 3.14);
        assert_eq!(c2.to_string().parse(), Ok(c2));

        let c3 = Const::Word(WordSize::W16, 255);
        assert_eq!(c3.to_string().parse(), Ok(c3));

        let c4 = Const::WordVector("hello".to_owned());
        assert_eq!(c4.to_string().parse(), Ok(c4));
    }

    #[test]
    fn test_print_prim_kind() {
        assert_eq!(pretty_print(&PrimKind::DependsOnState), "DependsOnState");
        assert_eq!(pretty_print(&PrimKind::Functional), "Functional");
        assert_eq!(pretty_print(&PrimKind::Moveable), "Moveable");
        assert_eq!(pretty_print(&PrimKind::SideEffect), "SideEffect");
    }

    #[test]
    fn test_print_prim_kind_inverse() {
        let k1: PrimKind = PrimKind::DependsOnState;
        let k2: PrimKind = PrimKind::Functional;
        let k3: PrimKind = PrimKind::Moveable;
        let k4: PrimKind = PrimKind::SideEffect;
        assert_eq!(k1.to_string().parse(), Ok(k1));
        assert_eq!(k2.to_string().parse(), Ok(k2));
        assert_eq!(k3.to_string().parse(), Ok(k3));
        assert_eq!(k4.to_string().parse(), Ok(k4));
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
    fn test_print_prim_primitive() {
        let expected = r#"
CFunction {
  args = ( (< Vector ( Word ( w8 ) ) >) ),
  convention = cdecl,
  inline = false,
  kind = Impure,
  prototype = prototype {
    args = ( (< Objptr >) ),
    res = None
  },
  return = (< Tuple (  ) >),
  symbolScope = private,
  target = target {
    type = Direct,
    name = "Stdio_print"
  }
}
        "#;
        let prim = PrimPrimitive::CFunction {
            args: vec![SmlType::Vector(Box::new(SmlType::Word(WordSize::W8)))],
            convention: CFunctionConvention::Cdecl,
            inline: false,
            kind: CFunctionKind::Impure,
            prototype: (vec![CType::Objptr], None),
            ret: SmlType::Tuple(vec![]),
            symbol_scope: CFunctionSymbolScope::Private,
            target: CFunctionTarget::Direct("Stdio_print".to_owned()),
        };
        assert_eq!(pretty_print(&prim), expected.trim());

        let prim2 = PrimPrimitive::SmlPrim("IntInf_add".to_owned());
        assert_eq!(pretty_print(&prim2), "\"IntInf_add\"");
    }

    #[test]
    fn test_print_prim() {
        let prim = Prim {
            prim: PrimPrimitive::SmlPrim("IntInf_add".to_owned()),
            kind: PrimKind::Functional,
        };
        let expected = r#"
primitive {
  prim = "IntInf_add",
  kind = Functional
}
        "#;
        assert_eq!(pretty_print(&prim), expected.trim());
    }

    #[test]
    fn test_print_prim_inverse() {
        let prim = Prim {
            prim: PrimPrimitive::SmlPrim("IntInf_add".to_owned()),
            kind: PrimKind::Functional,
        };
        assert_eq!(prim.to_string().parse(), Ok(prim));

        let prim = Prim {
            prim: PrimPrimitive::CFunction {
                args: vec![SmlType::Vector(Box::new(SmlType::Word(WordSize::W8)))],
                convention: CFunctionConvention::Cdecl,
                inline: false,
                kind: CFunctionKind::Impure,
                prototype: (vec![CType::Objptr], None),
                ret: SmlType::Tuple(vec![]),
                symbol_scope: CFunctionSymbolScope::Private,
                target: CFunctionTarget::Direct("Stdio_print".to_string()),
            },
            kind: PrimKind::Functional,
        };
        assert_eq!(prim.to_string().parse(), Ok(prim));
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
        let c1 = Cases::Con(vec![("::_1".to_string(), "L1".to_string()), ("::_2".to_string(), "L2".to_string())]);
        assert_eq!(c1.to_string().parse(), Ok(c1));

        let c2 = Cases::Word(WordSize::W8, vec![(0, "L1".to_string()), (255, "L2".to_string())]);
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
            cases: Cases::Con(vec![("::_1".to_string(), "L1".to_string()), ("::_2".to_string(), "L2".to_string())]),
            default: Some("L3".to_string()),
        };
        assert_eq!(t3.to_string().parse(), Ok(t3));

        let t4 = Transfer::Case {
            test: "x".to_string(),
            cases: Cases::Word(WordSize::W8, vec![(0, "L1".to_string()), (255, "L2".to_string())]),
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
                        prim: Prim {
                            prim: PrimPrimitive::CFunction {
                                args: vec![SmlType::Vector(Box::new(SmlType::Word(WordSize::W8)))],
                                convention: CFunctionConvention::Cdecl,
                                inline: false,
                                kind: CFunctionKind::Impure,
                                prototype: (vec![CType::Objptr], None),
                                ret: SmlType::Tuple(vec![]),
                                symbol_scope: CFunctionSymbolScope::Private,
                                target: CFunctionTarget::Direct("Stdio_print".to_string()),
                            },
                            kind: PrimKind::DependsOnState,
                        },
                        targs: None,
                        args: vec!["global_37".to_string()],
                    }
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
            blocks: vec![
                Block {
                    label: "L_52".to_string(),
                    args: vec![],
                    statements: vec![],
                    transfer: Transfer::Return { args: vec![] },
                } 
            ],
        };
        assert_eq!(f.to_string().parse(), Ok(f));
    }

    #[test]
    fn test_print_datatype_inverse() {
        let dt = Datatype {
            tycon: "list_0".to_string(),
            constrs: vec![
                ("::_0".to_string(), vec![SmlType::Datatype("list_0".to_string()), SmlType::Tuple(vec![SmlType::Word(WordSize::W8), SmlType::Word(WordSize::W8)])]),
                ("nil_1".to_string(), vec![]),
            ],
        };
        println!("{}", dt.to_string());
        assert_eq!(dt.to_string().parse(), Ok(dt));
    }
}
