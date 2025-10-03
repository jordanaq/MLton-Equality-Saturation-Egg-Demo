use std::{collections::{HashMap, HashSet}, str::FromStr};

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::{complete::tag, take_till, take_until, take_while1},
    character::multispace0,
    combinator::{complete, map, opt},
    sequence::{delimited, preceded, separated_pair},
};

use parse_utils::*;

use crate::ssa::*;

fn parse_sml_type_array(s: &str) -> IResult<&str, SmlType> {
    named_tuple_parser("Array", parse_sml_type_raw)
        .map(|t| SmlType::Array(Box::new(t)))
        .parse(s)
}

fn parse_sml_type_cpointer(s: &str) -> IResult<&str, SmlType> {
    tagify_parser(tag("CPointer"))
        .map(|_| SmlType::CPointer)
        .parse(s)
}

fn parse_sml_type_datatype(s: &str) -> IResult<&str, SmlType> {
    named_tuple_parser("Datatype", parse_string)
        .map(|s| s.trim().to_string())
        .map(SmlType::Datatype)
        .parse(s)
}

fn parse_sml_type_intinf(s: &str) -> IResult<&str, SmlType> {
    tagify_parser(tag("IntInf"))
        .map(|_| SmlType::IntInf)
        .parse(s)
}

fn parse_realsize(s: &str) -> IResult<&str, RealSize> {
    alt((tag("r32"), tag("r64")))
        .map(|sz| match sz {
            "r32" => RealSize::R32,
            "r64" => RealSize::R64,
            _ => unreachable!(),
        })
        .parse(s)
}

impl FromStr for RealSize {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_realsize(s) {
            Ok(("", sz)) => Ok(sz),
            _ => Err(()),
        }
    }
}

fn parse_wordsize(s: &str) -> IResult<&str, WordSize> {
    let (rest, sz) = alt((tag("w8"), tag("w16"), tag("w32"), tag("w64"))).parse(s)?;

    match sz {
        "w8" => Ok((rest, WordSize::W8)),
        "w16" => Ok((rest, WordSize::W16)),
        "w32" => Ok((rest, WordSize::W32)),
        "w64" => Ok((rest, WordSize::W64)),
        _ => unreachable!(),
    }
}

impl FromStr for WordSize {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_wordsize(s) {
            Ok(("", sz)) => Ok(sz),
            _ => Err(()),
        }
    }
}

fn parse_sml_type_real(s: &str) -> IResult<&str, SmlType> {
    named_tuple_parser("Real", parse_realsize)
        .map(|sz| SmlType::Real(sz))
        .parse(s)
}

fn parse_sml_type_ref(s: &str) -> IResult<&str, SmlType> {
    named_tuple_parser("Ref", parse_sml_type_raw)
        .map(|t| SmlType::Ref(Box::new(t)))
        .parse(s)
}

fn parse_sml_type_thread(s: &str) -> IResult<&str, SmlType> {
    tagify_parser(tag("Thread"))
        .map(|_| SmlType::Thread)
        .parse(s)
}

fn parse_sml_type_tuple(s: &str) -> IResult<&str, SmlType> {
    preceded(
        tagify_parser(tag("Tuple")),
        paren_list_parser(parse_sml_type_raw),
    )
    .map(|ts| SmlType::Tuple(ts))
    .parse(s)
}

fn parse_sml_type_vector(s: &str) -> IResult<&str, SmlType> {
    named_tuple_parser("Vector", parse_sml_type_raw)
        .map(|t| SmlType::Vector(Box::new(t)))
        .parse(s)
}

fn parse_sml_type_weak(s: &str) -> IResult<&str, SmlType> {
    named_tuple_parser("Weak", parse_sml_type_raw)
        .map(|t| SmlType::Weak(Box::new(t)))
        .parse(s)
}

fn parse_sml_type_word(s: &str) -> IResult<&str, SmlType> {
    named_tuple_parser("Word", parse_wordsize)
        .map(|sz| SmlType::Word(sz))
        .parse(s)
}

pub(crate) fn parse_sml_type_raw(s: &str) -> IResult<&str, SmlType> {
    alt((
        parse_sml_type_array,
        parse_sml_type_cpointer,
        parse_sml_type_datatype,
        parse_sml_type_intinf,
        parse_sml_type_real,
        parse_sml_type_ref,
        parse_sml_type_thread,
        parse_sml_type_tuple,
        parse_sml_type_vector,
        parse_sml_type_weak,
        parse_sml_type_word,
    ))
    .parse(s)
}

pub fn parse_sml_type(s: &str) -> IResult<&str, SmlType> {
    delimited(
        tagify_parser(tag("(<")),
        parse_sml_type_raw,
        tagify_parser(tag(">)")),
    )
    .parse(s)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SmlTypeParseErr;
impl FromStr for SmlType {
    type Err = SmlTypeParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_sml_type(s) {
            Ok(("", t)) => Ok(t),
            _ => Err(SmlTypeParseErr),
        }
    }
}

fn parse_sml_types(s: &str) -> IResult<&str, Vec<SmlType>> {
    paren_list_parser(parse_sml_type).parse(s)
}

fn parse_cons(s: &str) -> IResult<&str, (ConstructorId, Vec<SmlType>)> {
    let (rest, (constr_id, arg_ts)) = delimited(
        (tag("("), multispace0()),
        alt((
            map(word(), |id: &str| (id, vec![])),
            separated_pair(
                take_till(|c: char| c == ','),
                (tag(","), multispace0()),
                parse_sml_types,
            ),
        )),
        (multispace0(), tag(")")),
    )
    .parse(s)?;

    Ok((rest, (constr_id.trim().into(), arg_ts)))
}

fn parse_datatype(s: &str) -> IResult<&str, Datatype> {
    let (rest, (tycon, cons)) = named_object_parser(
        "datatype",
        (
            parse_key_field("tycon", parse_string),
            parse_key_field("cons", paren_list_parser(parse_cons)),
        ),
    )
    .parse(s)?;

    Ok((
        rest,
        Datatype {
            tycon: tycon.to_string(),
            constrs: cons,
        },
    ))
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParseDatatypeErr;
impl FromStr for Datatype {
    type Err = ParseDatatypeErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_datatype(s) {
            Ok(("", dt)) => Ok(dt),
            _ => Err(ParseDatatypeErr),
        }
    }
}

fn parse_datatypes(s: &str) -> IResult<&str, HashMap<SmlType, Datatype>> {
    let (rest, dtypes) = paren_list_parser(parse_datatype).parse(s)?;

    Ok((
        rest,
        dtypes
            .into_iter()
            .map(
                |dt| (SmlType::Datatype(dt.tycon.clone()), dt), /* TODO */
            )
            .collect(),
    ))
}

fn parse_var_name(s: &str) -> IResult<&str, VarId> {
    let (rest, var) = alt((
        complete(take_while1(|c: char| {
            c.is_alphanumeric() || "'_".contains(c)
        })),
        tag("_"),
    ))
    .parse(s)?;
    Ok((rest, var.to_string()))
}

fn parse_var_names(s: &str) -> IResult<&str, Vec<VarId>> {
    paren_list_parser(parse_var_name).parse(s)
}

fn parse_exp_conapp(s: &str) -> IResult<&str, Exp> {
    let (rest, (con, args)) = named_object_parser(
        "exp::ConApp",
        (
            parse_key_field("con", parse_string),
            parse_key_field("args", parse_var_names),
        ),
    )
    .parse(s)?;

    Ok((
        rest,
        Exp::ConApp {
            con: con.into(),
            args,
        },
    ))
}

fn parse_const_csymbol(s: &str) -> IResult<&str, Const> {
    todo!()
}

fn parse_const_intinf(s: &str) -> IResult<&str, Const> {
    todo!()
}

fn parse_const_null(s: &str) -> IResult<&str, Const> {
    let (rest, _) = tagify_parser((tag("const::Null {"), multispace0(), tag("}"))).parse(s)?;
    Ok((rest, Const::Null))
}

fn parse_real(s: &str) -> IResult<&str, (f64, RealSize)> {
    let (rest, (r, sz)) = (
        (
            take_while1(|c: char| c.is_digit(10)),
            opt(preceded(tag("."), take_while1(|c: char| c.is_digit(10)))),
        ),
        preceded(tag(":"), parse_realsize),
    )
        .parse(s)?;

    let r: f64 = match r {
        (int_part, Some(frac_part)) => format!("{}.{}", int_part, frac_part).parse().unwrap(),
        (int_part, None) => int_part.parse().unwrap(),
    };

    Ok((rest, (r, sz)))
}

fn parse_const_real(s: &str) -> IResult<&str, Const> {
    let (rest, (r, sz)) =
        named_object_parser("const::Real", parse_key_field("const", parse_real)).parse(s)?;
    Ok((rest, Const::Real(sz, r)))
}

pub fn parse_word(s: &str) -> IResult<&str, (u64, WordSize)> {
    let (rest, (w, sz)) = preceded(
        tag("0x"),
        (
            take_while1(|c: char| c.is_digit(16)),
            preceded(tag(":"), parse_wordsize),
        ),
    )
    .parse(s)?;

    let w: u64 = u64::from_str_radix(w, 16).unwrap();
    Ok((rest, (w, sz)))
}

fn parse_const_word(s: &str) -> IResult<&str, Const> {
    let (rest, (w, sz)) =
        named_object_parser("const::Word", parse_key_field("const", parse_word)).parse(s)?;
    Ok((rest, Const::Word(sz, w)))
}

fn parse_const_wordvector(s: &str) -> IResult<&str, Const> {
    let (rest, wv) =
        named_object_parser("const::WordVector", parse_key_field("const", parse_string))
            .parse(s)?;
    Ok((rest, Const::WordVector(wv.to_string())))
}

pub fn parse_const(s: &str) -> IResult<&str, Const> {
    alt((
        // parse_const_csymbol,
        // parse_const_intinf,
        parse_const_null,
        parse_const_real,
        parse_const_word,
        parse_const_wordvector,
    ))
    .parse(s)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParseConstErr;
impl FromStr for Const {
    type Err = ParseConstErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_const(s) {
            Ok(("", c)) => Ok(c),
            _ => Err(ParseConstErr),
        }
    }
}

pub fn parse_exp_const(s: &str) -> IResult<&str, Exp> {
    named_object_parser(
        "exp::Const",
        parse_key_field("const", parse_const).map(Exp::Const),
    )
    .parse(s)
}

fn parse_exp_profile(s: &str) -> IResult<&str, Exp> {
    todo!()
}

fn parse_exp_select(s: &str) -> IResult<&str, Exp> {
    let (rest, (tuple, offset)) = named_object_parser(
        "exp::Select",
        (
            parse_key_field("tuple", parse_var_name),
            parse_key_field(
                "offset",
                map(take_while1(|c: char| c.is_digit(10)), |s: &str| {
                    s.parse::<i128>().unwrap()
                }),
            ),
        ),
    )
    .parse(s)?;

    Ok((rest, Exp::Select { tuple, offset }))
}

fn parse_exp_tuple(s: &str) -> IResult<&str, Exp> {
    let (rest, args) =
        named_object_parser("exp::Tuple", parse_key_field("args", parse_var_names)).parse(s)?;

    Ok((rest, Exp::Tuple(args)))
}

fn parse_exp_var(s: &str) -> IResult<&str, Exp> {
    let (rest, var) = named_object_parser(
        "exp::Var",
        parse_key_field("var", tagify_parser(parse_var_name)),
    )
    .parse(s)?;

    Ok((rest, Exp::Var(var)))
}

fn parse_cfunction_convention(s: &str) -> IResult<&str, CFunctionConvention> {
    let (rest, conv) = alt((tag("cdecl"), tag("stdcall"))).parse(s)?;

    match conv {
        "cdecl" => Ok((rest, CFunctionConvention::Cdecl)),
        "stdcall" => Ok((rest, CFunctionConvention::Stdcall)),
        _ => unreachable!(),
    }
}

impl FromStr for CFunctionConvention {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_cfunction_convention(s) {
            Ok(("", c)) => Ok(c),
            _ => Err(()),
        }
    }
}

fn parse_cfunction_kind(s: &str) -> IResult<&str, CFunctionKind> {
    let (rest, kind) = alt((tag("Impure"), tag("Pure"), tag("Runtime"))).parse(s)?;

    match kind {
        "Impure" => Ok((rest, CFunctionKind::Impure)),
        "Pure" => Ok((rest, CFunctionKind::Pure)),
        "Runtime" => Ok((rest, CFunctionKind::Runtime)),
        _ => unreachable!(),
    }
}

impl FromStr for CFunctionKind {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_cfunction_kind(s) {
            Ok(("", k)) => Ok(k),
            _ => Err(()),
        }
    }
}

fn parse_cfunction_symbol_scope(s: &str) -> IResult<&str, CFunctionSymbolScope> {
    let (rest, scope) = alt((tag("external"), tag("private"), tag("public"))).parse(s)?;

    match scope {
        "external" => Ok((rest, CFunctionSymbolScope::External)),
        "private" => Ok((rest, CFunctionSymbolScope::Private)),
        "public" => Ok((rest, CFunctionSymbolScope::Public)),
        _ => unreachable!(),
    }
}

impl FromStr for CFunctionSymbolScope {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_cfunction_symbol_scope(s) {
            Ok(("", s)) => Ok(s),
            _ => Err(()),
        }
    }
}

fn parse_ctype_raw(s: &str) -> IResult<&str, CType> {
    alt((
        tag("CPointer").map(|_| CType::CPointer),
        tag("Int8").map(|_| CType::Int8),
        tag("Int16").map(|_| CType::Int16),
        tag("Int32").map(|_| CType::Int32),
        tag("Int64").map(|_| CType::Int64),
        tag("Objptr").map(|_| CType::Objptr),
        tag("Real32").map(|_| CType::Real32),
        tag("Real64").map(|_| CType::Real64),
        tag("Word8").map(|_| CType::Word8),
        tag("Word16").map(|_| CType::Word16),
        tag("Word32").map(|_| CType::Word32),
        tag("Word64").map(|_| CType::Word64),
    ))
    .parse(s)
}

fn parse_ctype(s: &str) -> IResult<&str, CType> {
    delimited(
        tagify_parser(tag("(<")),
        parse_ctype_raw,
        tagify_parser(tag(">)")),
    )
    .parse(s)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParseCTypeErr;
impl FromStr for CType {
    type Err = ParseCTypeErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_ctype(s) {
            Ok(("", t)) => Ok(t),
            _ => Err(ParseCTypeErr),
        }
    }
}

fn parse_ctypes(s: &str) -> IResult<&str, Vec<CType>> {
    paren_list_parser(parse_ctype).parse(s)
}

fn parse_cfunction_prototype(s: &str) -> IResult<&str, (Vec<CType>, Option<CType>)> {
    let (rest, (args, ret)) = named_object_parser(
        "prototype",
        (
            parse_key_field("args", parse_ctypes),
            parse_key_field("res", option_parser(parse_ctype)),
        ),
    )
    .parse(s)?;

    Ok((rest, (args, ret)))
}

fn parse_cfunction_target(s: &str) -> IResult<&str, CFunctionTarget> {
    named_object_parser(
        "target",
        alt((
            (
                parse_key_field("type", tag("Direct")),
                parse_key_field("name", parse_string),
            )
                .map(|(_, sym)| CFunctionTarget::Direct(sym.to_string())),
            (parse_key_field("type", tag("Indirect"))).map(|_| CFunctionTarget::Indirect),
        )),
    )
    .parse(s)
}

impl FromStr for CFunctionTarget {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_cfunction_target(s) {
            Ok(("", t)) => Ok(t),
            _ => Err(()),
        }
    }
}

fn parse_cfunction(s: &str) -> IResult<&str, PrimPrimitive> {
    let (rest, (args, convention, inline, kind, prototype, ret, symbol_scope, target)) =
        named_object_parser(
            "CFunction",
            (
                parse_key_field("args", parse_sml_types),
                parse_key_field("convention", parse_cfunction_convention),
                parse_key_field("inline", parse_true_false),
                parse_key_field("kind", parse_cfunction_kind),
                parse_key_field("prototype", parse_cfunction_prototype),
                parse_key_field("return", parse_sml_type),
                parse_key_field("symbolScope", parse_cfunction_symbol_scope),
                parse_key_field("target", parse_cfunction_target),
            ),
        )
        .parse(s)?;

    Ok((
        rest,
        PrimPrimitive::CFunction {
            args,
            convention,
            inline,
            kind,
            prototype,
            ret,
            symbol_scope,
            target,
        },
    ))
}

fn parse_prim_kind(s: &str) -> IResult<&str, PrimKind> {
    let (rest, kind) = alt((
        tag("DependsOnState"),
        tag("Functional"),
        tag("Moveable"),
        tag("SideEffect"),
    ))
    .parse(s)?;

    match kind {
        "DependsOnState" => Ok((rest, PrimKind::DependsOnState)),
        "Functional" => Ok((rest, PrimKind::Functional)),
        "Moveable" => Ok((rest, PrimKind::Moveable)),
        "SideEffect" => Ok((rest, PrimKind::SideEffect)),
        _ => unreachable!(),
    }
}

impl FromStr for PrimKind {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_prim_kind(s) {
            Ok(("", k)) => Ok(k),
            _ => Err(()),
        }
    }
}

fn parse_prim(s: &str) -> IResult<&str, Prim> {
    let (rest, (p, k)) = named_object_parser(
        "primitive",
        (
            parse_key_field(
                "prim",
                alt((
                    parse_cfunction,
                    map(parse_string, |id | {
                        PrimPrimitive::SmlPrim(id.trim().into())
                    }),
                )),
            ),
            parse_key_field("kind", parse_prim_kind),
        ),
    )
    .parse(s)?;

    Ok((rest, Prim { prim: p, kind: k }))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParsePrimErr;
impl FromStr for Prim {
    type Err = ParsePrimErr;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_prim(s) {
            Ok(("", p)) => Ok(p),
            _ => Err(ParsePrimErr),
        }
    }
}

fn parse_exp_primapp(s: &str) -> IResult<&str, Exp> {
    let (rest, (prim, args, targs)) = named_object_parser(
        "exp::PrimApp",
        (
            parse_key_field("prim", parse_prim),
            parse_key_field("args", parse_var_names),
            opt(parse_key_field("targs", parse_sml_types)),
        ),
    )
    .parse(s)?;

    Ok((
        rest,
        Exp::PrimApp {
            prim: prim,
            targs,
            args,
        },
    ))
}

fn parse_exp(s: &str) -> IResult<&str, Exp> {
    let (rest, exp) = alt((
        parse_exp_conapp,
        parse_exp_primapp,
        parse_exp_const,
        // TODO: parse_exp_profile,
        parse_exp_select,
        parse_exp_tuple,
        parse_exp_var,
    ))
    .parse(s)?;
    Ok((rest, exp))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParseExpErr;
impl FromStr for Exp {
    type Err = ParseExpErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_exp(s) {
            Ok(("", e)) => Ok(e),
            _ => Err(ParseExpErr),
        }
    }
}

fn parse_statement(s: &str) -> IResult<&str, Statement> {
    let (rest, (var, ty, exp)) = named_object_parser(
        "statement",
        (
            parse_key_field("var", option_parser(parse_var_name)),
            parse_key_field("type", parse_sml_type),
            parse_key_field("exp", parse_exp),
        ),
    )
    .parse(s)?;

    Ok((rest, Statement { var, ty, exp: exp }))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParseStatementErr;
impl FromStr for Statement {
    type Err = ParseStatementErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_statement(s) {
            Ok(("", stmt)) => Ok(stmt),
            _ => Err(ParseStatementErr),
        }
    }
}

fn parse_globals(s: &str) -> IResult<&str, Vec<Statement>> {
    let (rest, globals) = paren_list_parser(parse_statement).parse(s)?;

    Ok((rest, globals))
}

fn parse_transfer_bug(s: &str) -> IResult<&str, Transfer> {
    let (rest, _) = named_object_parser("transfer::Bug", multispace0()).parse(s)?;
    Ok((rest, Transfer::Bug))
}

fn parse_transfer_call_dead(s: &str) -> IResult<&str, Transfer> {
    let (rest, (func, args)) = named_object_parser(
        "transfer::call::Dead",
        (
            parse_key_field("func", parse_var_name),
            parse_key_field("args", parse_var_names),
        ),
    )
    .parse(s)?;
    Ok((
        rest,
        Transfer::Call {
            func,
            args,
            ret: Return::Dead,
        },
    ))
}

fn parse_transfer_call_non_tail_handler(s: &str) -> IResult<&str, Handler> {
    let (rest, h) = alt((
        named_object_parser("handler::Caller", multispace0()).map(|_| Handler::Caller),
        named_object_parser("handler::Dead", multispace0()).map(|_| Handler::Dead),
        named_object_parser("handler::Handle", parse_key_field("label", parse_var_name)).map(
            |label| Handler::Handle {
                label: label.to_string(),
            },
        ),
    ))
    .parse(s)?;

    Ok((rest, h))
}


#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParseHandlerErr;
impl FromStr for Handler {
    type Err = ParseHandlerErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_transfer_call_non_tail_handler(s) {
            Ok(("", h)) => Ok(h),
            _ => Err(ParseHandlerErr),
        }
    }
}

fn parse_transfer_call_non_tail(s: &str) -> IResult<&str, Transfer> {
    let (rest, (func, args, cont_id, handler)) = named_object_parser(
        "transfer::call::NonTail",
        (
            parse_key_field("func", parse_var_name),
            parse_key_field("args", parse_var_names),
            parse_key_field("cont", parse_var_name),
            parse_key_field("handler", parse_transfer_call_non_tail_handler),
        ),
    )
    .parse(s)?;
    Ok((
        rest,
        Transfer::Call {
            func,
            args,
            ret: Return::NonTail {
                cont: cont_id.to_string(),
                handler,
            },
        },
    ))
}

fn parse_transfer_call_tail(s: &str) -> IResult<&str, Transfer> {
    let (rest, (func, args)) = named_object_parser(
        "transfer::call::Tail",
        (
            parse_key_field("func", parse_var_name),
            parse_key_field("args", parse_var_names),
        ),
    )
    .parse(s)?;
    Ok((
        rest,
        Transfer::Call {
            func,
            args,
            ret: Return::Tail,
        },
    ))
}

fn parse_transfer_call(s: &str) -> IResult<&str, Transfer> {
    let (rest, t) = alt((
        parse_transfer_call_dead,
        parse_transfer_call_non_tail,
        parse_transfer_call_tail,
    ))
    .parse(s)?;

    Ok((rest, t))
}

fn parse_transfer_case_con_con(s: &str) -> IResult<&str, (ConstructorId, Label)> {
    separated_pair(
        map(take_until("=>"), |con: &str| con.trim().to_string()),
        tagify_parser(tag("=>")),
        parse_var_name,
    )
    .parse(s)
}

fn parse_cases_con(s: &str) -> IResult<&str, Cases> {
    paren_list_parser(parse_transfer_case_con_con).map(Cases::Con).parse(s)
}

fn parse_transfer_case_con(s: &str) -> IResult<&str, Transfer> {
    let (rest, (test, cases, default)) = named_object_parser(
        "transfer::case::Con",
        (
            parse_key_field("test", parse_var_name),
            parse_key_field("cases", parse_cases_con),
            opt(map(parse_key_field("default", parse_var_name), |l| {
                l.to_string()
            })),
        ),
    )
    .parse(s)?;

    Ok((
        rest,
        Transfer::Case {
            test,
            cases,
            default,
        },
    ))
}

fn parse_transfer_case_word_con(s: &str) -> IResult<&str, ((u64, WordSize), Label)> {
    separated_pair(parse_word, tagify_parser(tag("=>")), parse_var_name).parse(s)
}

fn parse_cases_word(s: &str) -> IResult<&str, Cases> {
    let (rest, cases) = paren_list_parser(parse_transfer_case_word_con).parse(s)?;
    
    assert!(cases.iter().map(|((_, sz), _)| sz.clone()).collect::<HashSet<WordSize>>().len() <= 1);

    if let Some(((_, ws), _)) = cases.first() {
        Ok((rest, Cases::Word(ws.clone(), cases.into_iter().map(|(w, l)| (w.0, l)).collect())))
    } else {
        Ok((rest, Cases::Word(WordSize::W8, vec![])))
    }
}

fn parse_cases(s: &str) -> IResult<&str, Cases> {
    alt((parse_cases_word, parse_cases_con)).parse(s)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParseCasesErr;
impl FromStr for Cases {
    type Err = ParseCasesErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_cases(s) {
            Ok(("", c)) => Ok(c),
            _ => Err(ParseCasesErr),
        }
    }
}

fn parse_transfer_case_word(s: &str) -> IResult<&str, Transfer> {
    let (rest, (test, ws, cases, default)) = named_object_parser(
        "transfer::case::Word",
        (
            parse_key_field("test", parse_var_name),
            parse_key_field("size", parse_wordsize),
            parse_key_field("cases", parse_cases_word),
            opt(map(parse_key_field("default", parse_var_name), |l| {
                l.to_string()
            })),
        ),
    )
    .parse(s)?;

    let Cases::Word(ws_cases, _) = cases.clone() else {
        unreachable!()
    };

    assert_eq!(ws, ws_cases);

    Ok((
        rest,
        Transfer::Case {
            test,
            cases,
            default,
        },
    ))
}

fn parse_transfer_case(s: &str) -> IResult<&str, Transfer> {
    alt((parse_transfer_case_con, parse_transfer_case_word)).parse(s)
}

fn parse_transfer_goto(s: &str) -> IResult<&str, Transfer> {
    map(
        named_object_parser(
            "transfer::Goto",
            (
                parse_key_field("dst", parse_var_name),
                parse_key_field("args", parse_var_names),
            ),
        ),
        |(dst, args)| Transfer::Goto { dst, args },
    )
    .parse(s)
}

fn parse_transfer_raise(s: &str) -> IResult<&str, Transfer> {
    map(
        named_object_parser("transfer::Raise", parse_key_field("args", parse_var_names)),
        |args| Transfer::Raise { args },
    )
    .parse(s)
}

fn parse_transfer_return(s: &str) -> IResult<&str, Transfer> {
    map(
        named_object_parser("transfer::Return", parse_key_field("args", parse_var_names)),
        |args| Transfer::Return { args },
    )
    .parse(s)
}

fn parse_transfer_runtime(s: &str) -> IResult<&str, Transfer> {
    unimplemented!()
}

fn parse_transfer(s: &str) -> IResult<&str, Transfer> {
    alt((
        parse_transfer_bug,
        parse_transfer_call,
        parse_transfer_case,
        parse_transfer_goto,
        parse_transfer_raise,
        parse_transfer_return,
        // parse_transfer_runtime,
    ))
    .parse(s)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParseTransferErr;
impl FromStr for Transfer {
    type Err = ParseTransferErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_transfer(s) {
            Ok(("", t)) => Ok(t),
            _ => Err(ParseTransferErr),
        }
    }
}

fn parse_block(s: &str) -> IResult<&str, Block> {
    let (rest, (label, args, statements, transfer)) = named_object_parser(
        "block",
        (
            parse_key_field("label", parse_var_name),
            parse_key_field("args", paren_list_parser(parse_typed_var)),
            parse_key_field("statements", paren_list_parser(parse_statement)),
            parse_key_field("transfer", parse_transfer),
        ),
    )
    .parse(s)?;

    Ok((
        rest,
        Block {
            args,
            label: label.to_string(),
            statements,
            transfer,
        },
    ))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParseBlockErr;
impl FromStr for Block {
    type Err = ParseBlockErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_block(s) {
            Ok(("", b)) => Ok(b),
            _ => Err(ParseBlockErr),
        }
    }
}

fn parse_typed_var(s: &str) -> IResult<&str, Var> {
    let (rest, (name, ty)) =
        separated_pair(parse_var_name, tagify_parser(tag(":")), parse_sml_type).parse(s)?;

    Ok((rest, Var { name, ty }))
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypedVarParseErr;
impl FromStr for Var {
    type Err = TypedVarParseErr;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_typed_var(s) {
            Ok(("", v)) => Ok(v),
            _ => Err(TypedVarParseErr),
        }
    }
}

fn parse_function(s: &str) -> IResult<&str, Function> {
    let (rest, (name, may_inline, args, start, returns, raises, blocks)) = named_object_parser(
        "function",
        (
            parse_key_field("name", parse_string),
            opt(parse_key_field("mayInline", parse_true_false)),
            parse_key_field("args", paren_list_parser(parse_typed_var)),
            parse_key_field("start", parse_var_name),
            parse_key_field("returns", option_parser(parse_sml_types)),
            parse_key_field("raises", option_parser(parse_sml_types)),
            parse_key_field("blocks", paren_list_parser(parse_block)),
        ),
    )
    .parse(s)?;

    Ok((
        rest,
        Function {
            args,
            blocks,
            may_inline: may_inline.unwrap_or(false),
            name: name.to_string(),
            raises,
            returns,
            start: start.to_string(),
        },
    ))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParseFunctionErr;
impl FromStr for Function {
    type Err = ParseFunctionErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_function(s) {
            Ok(("", f)) => Ok(f),
            _ => Err(ParseFunctionErr),
        }
    }
}

fn parse_main(s: &str) -> IResult<&str, FunctionId> {
    parse_var_name(s)
}

pub fn parse_ssa(s: &str) -> IResult<&str, MltonSsa> {
    let (rest, (datatypes, globals, functions, main)) = named_object_parser(
        "mltonssa",
        (
            parse_key_field("datatypes", parse_datatypes),
            parse_key_field("globals", parse_globals),
            parse_key_field("functions", paren_list_parser(parse_function)),
            parse_key_field("main", parse_main),
        ),
    )
    .parse(s)?;

    Ok((
        rest,
        MltonSsa {
            datatypes,
            globals,
            functions,
            main,
        },
    ))
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParseErrSsa;

impl std::str::FromStr for MltonSsa {
    type Err = ParseErrSsa;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_ssa(s) {
            Ok(("", ssa)) => Ok(ssa),
            _ => Err(ParseErrSsa),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_main() {
        let s = r#"main"#;
        let (_rest, main) = parse_main(s).unwrap();
        assert_eq!(main, "main".to_string());
    }

    #[test]
    fn test_parse_sml_type_raw() {
        let s = r#"Array(Word(w8))"#;
        let (rest, t) = parse_sml_type_raw(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(t, SmlType::Array(Box::new(SmlType::Word(WordSize::W8))));

        let s = r#"CPointer"#;
        let (rest, t) = parse_sml_type_raw(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(t, SmlType::CPointer);

        let s = r#"Datatype("list_0")"#;
        let (rest, t) = parse_sml_type_raw(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(t, SmlType::Datatype("list_0".to_string()));

        let s = r#"IntInf"#;
        let (rest, t) = parse_sml_type_raw(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(t, SmlType::IntInf);

        let s = r#"Real(r64)"#;
        let (rest, t) = parse_sml_type_raw(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(t, SmlType::Real(RealSize::R64));

        let s = r#"Ref(Datatype("list_0"))"#;
        let (rest, t) = parse_sml_type_raw(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            t,
            SmlType::Ref(Box::new(SmlType::Datatype("list_0".to_string())))
        );

        let s = r#"Thread"#;
        let (rest, t) = parse_sml_type_raw(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(t, SmlType::Thread);

        let s = r#"Tuple(Word(w8), Word(w16))"#;
        let (rest, t) = parse_sml_type_raw(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            t,
            SmlType::Tuple(vec![
                SmlType::Word(WordSize::W8),
                SmlType::Word(WordSize::W16)
            ])
        );

        let s = r#"Vector(Word(w8))"#;
        let (rest, t) = parse_sml_type_raw(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(t, SmlType::Vector(Box::new(SmlType::Word(WordSize::W8))));
    }

    #[test]
    fn test_parse_sml_type() {
        let s = r#"(< Array(Word(w8)) >)"#;
        let (rest, t) = parse_sml_type(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(t, SmlType::Array(Box::new(SmlType::Word(WordSize::W8))));
    }

    #[test]
    fn test_parse_sml_types() {
        let s = r#"( (< Datatype( "list_0" ) >), (< Tuple (Word(w8), Word( w8 )) >) )"#;
        let (rest, ts) = parse_sml_types(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(ts.len(), 2);
        assert_eq!(ts[0], SmlType::Datatype("list_0".to_string()));
        assert_eq!(
            ts[1],
            SmlType::Tuple(vec![
                SmlType::Word(WordSize::W8),
                SmlType::Word(WordSize::W8)
            ])
        );
    }

    #[test]
    fn test_parse_cons() {
        let s = r#"( ::_0, ((< Datatype( "list_0" ) >),
                                                  (< Tuple (Word( w8 ),
                                                            Word( w8 )) >)) )"#;
        let (rest, (constr_id, arg_ts)) = parse_cons(s).unwrap();
        assert_eq!(constr_id, "::_0".to_string());
        assert_eq!(rest, "");
        assert_eq!(arg_ts.len(), 2);

        let s = r#"( nil_1 )"#;
        let (rest, (constr_id, arg_ts)) = parse_cons(s).unwrap();
        assert_eq!(constr_id, "nil_1".to_string());
        assert_eq!(rest, "");
        assert_eq!(arg_ts.len(), 0);
    }

    #[test]
    fn test_parse_datatype() {
        let s = r#"
        datatype {tycon = "list_0",
                  cons = (( ::_0, ((< Datatype( "list_0" ) >),
                                   (< Tuple (Word( w8 ),
                                             Word( w8 )) >)) ),
                          ( nil_1 ))}
        "#;

        let (rest, dt) = parse_datatype(s).unwrap();
        assert_eq!(rest.trim(), "");
        assert_eq!(dt.tycon, "list_0".to_string());
        assert_eq!(dt.constrs.len(), 2);
        assert_eq!(dt.constrs[0].0, "::_0".to_string());
        assert_eq!(dt.constrs[0].1.len(), 2);
        assert_eq!(dt.constrs[1].0, "nil_1".to_string());
        assert_eq!(dt.constrs[1].1.len(), 0);

        let s = r#"
        datatype {
  tycon = "list_0",
  cons = ( ( ::_0, ( (< Datatype ( "list_0" ) >), (< Tuple ( Word ( w8 ), Word ( w8 ) ) >) ) ), ( nil_1, (  ) ) )
}
        "#;

        let (rest, dt) = parse_datatype(s).unwrap();
        assert_eq!(rest.trim(), "");
        assert_eq!(dt.tycon, "list_0".to_string());
        assert_eq!(dt.constrs.len(), 2);
        assert_eq!(dt.constrs[0].0, "::_0".to_string());
        assert_eq!(dt.constrs[0].1.len(), 2);
        assert_eq!(dt.constrs[1].0, "nil_1".to_string());
        assert_eq!(dt.constrs[1].1.len(), 0);
    }

    #[test]
    fn test_parse_datatypes() {
        let s = r#"
        (datatype {tycon = "list_4", cons = (( dummy_0 ))},
         datatype {tycon = "list_3",
                   cons = (( nil_0 ),
                           ( ::_2, ((< Datatype( "list_3" ) >)) ))},
         datatype {tycon = "list_2",
                   cons = (( nil_2 ),
                           ( ::_1, ((< Datatype( "list_2" ) >),
                                    (< Vector( Word( w8 ) ) >)) ))},
         datatype {tycon = "list_1", cons = (( nil_3 ))},
         datatype {tycon = "list_0",
                   cons = (( ::_0, ((< Datatype( "list_0" ) >),
                                    (< Tuple (Word( w8 ),
                                              Word( w8 )) >)) ),
                           ( nil_1 ))},
         datatype {tycon = "bool", cons = (( true ), ( false ))})
        "#;
        let (rest, dts) = parse_datatypes(s).unwrap();
        assert_eq!(rest.trim(), "");
        assert_eq!(dts.len(), 6);

        let key = SmlType::Datatype("list_0".to_string());
        assert!(dts.contains_key(&key));
        assert_eq!(dts.get(&key).unwrap().constrs.len(), 2);
    }

    #[test]
    fn test_parse_var_name() {
        let s = "x' , ";
        let (rest, var) = parse_var_name(s).unwrap();
        assert_eq!(rest, " , ");
        assert_eq!(var, "x'".to_string());
    }

    #[test]
    fn test_parse_var_names() {
        let s = "(x_0, y_1, z_2) , ";
        let (rest, vars) = parse_var_names(s).unwrap();
        assert_eq!(rest, " , ");
        assert_eq!(
            vars,
            vec!["x_0".to_string(), "y_1".to_string(), "z_2".to_string()]
        );

        let s = "(x_0) , ";
        let (rest, vars) = parse_var_names(s).unwrap();
        assert_eq!(rest, " , ");
        assert_eq!(vars, vec!["x_0".to_string()]);

        let s = "() , ";
        let (rest, vars) = parse_var_names(s).unwrap();
        assert_eq!(rest, " , ");
        assert_eq!(vars, Vec::<VarId>::new());
    }

    #[test]
    fn test_parse_exp_conapp() {
        let s = r#"exp::ConApp {
                                   con = "::_1",
                                   args = 
                                   (global_43, x_49) ,
                                 }"#;
        let (rest, exp) = parse_exp_conapp(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            exp,
            Exp::ConApp {
                con: "::_1".to_string(),
                args: vec!["global_43".to_string(), "x_49".to_string()]
            }
        );
    }

    #[test]
    fn test_parse_const_wordvector() {
        let s = r#"const::WordVector { const = "Overflow"}"#;
        let (rest, exp) = parse_const_wordvector(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(exp, Const::WordVector("Overflow".to_string()));
    }

    #[test]
    fn test_parse_wordsize() {
        let s = "w8";
        let (rest, sz) = parse_wordsize(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(sz, WordSize::W8);

        let s = "w16";
        let (rest, sz) = parse_wordsize(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(sz, WordSize::W16);

        let s = "w32";
        let (rest, sz) = parse_wordsize(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(sz, WordSize::W32);

        let s = "w64";
        let (rest, sz) = parse_wordsize(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(sz, WordSize::W64);
    }

    #[test]
    fn test_parse_const_word() {
        let s = r#"const::Word { const = 0x000000000000000A:w64 }"#;
        let (rest, exp) = parse_const_word(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(exp, Const::Word(WordSize::W64, 10));
    }

    #[test]
    fn test_parse_const_real() {
        let s = r#"const::Real { const = 3.14:r32 }"#;
        let (rest, exp) = parse_const_real(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(exp, Const::Real(RealSize::R32, 3.14));
    }

    #[test]
    fn test_parse_const_null() {
        let s = r#"const::Null {}"#;
        let (rest, exp) = parse_const_null(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(exp, Const::Null);
    }

    #[test]
    fn test_parse_exp_const() {
        let s = r#"exp::Const { const = const::Null {} }"#;
        let (rest, exp) = parse_exp_const(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(exp, Exp::Const(Const::Null));

        let s = r#"exp::Const { const = const::Word { const = 0x000000000000000A:w64 } }"#;
        let (rest, exp) = parse_exp_const(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(exp, Exp::Const(Const::Word(WordSize::W64, 10)));

        let s = r#"exp::Const { const = const::Real { const = 3.14:r32 } }"#;
        let (rest, exp) = parse_exp_const(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(exp, Exp::Const(Const::Real(RealSize::R32, 3.14)));

        let s = r#"exp::Const { const = const::WordVector { const = "Overflow"} }"#;
        let (rest, exp) = parse_exp_const(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            exp,
            Exp::Const(crate::ssa::Const::WordVector("Overflow".to_string()))
        );
    }

    #[test]
    fn test_parse_exp_var() {
        let s = r#"exp::Var { var = x_0, }"#;
        let (rest, exp) = parse_exp_var(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(exp, Exp::Var("x_0".to_string()));
    }

    #[test]
    fn test_parse_exp_tuple() {
        let s = r#"exp::Tuple { args = (global_15, global_14), } ,"#;
        let (rest, exp) = parse_exp_tuple(s).unwrap();
        assert_eq!(rest, " ,");
        assert_eq!(
            exp,
            Exp::Tuple(vec!["global_15".to_string(), "global_14".to_string()])
        );
    }

    #[test]
    fn test_parse_cfunction_convention() {
        let s = "cdecl";
        let (rest, conv) = parse_cfunction_convention(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(conv, CFunctionConvention::Cdecl);

        let s = "stdcall";
        let (rest, conv) = parse_cfunction_convention(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(conv, CFunctionConvention::Stdcall);
    }

    #[test]
    fn test_parse_cfunction_kind() {
        let s = "Impure";
        let (rest, kind) = parse_cfunction_kind(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(kind, CFunctionKind::Impure);

        let s = "Pure";
        let (rest, kind) = parse_cfunction_kind(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(kind, CFunctionKind::Pure);

        let s = "Runtime";
        let (rest, kind) = parse_cfunction_kind(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(kind, CFunctionKind::Runtime);
    }

    #[test]
    fn test_parse_cfunction_symbol_scope() {
        let s = "external";
        let (rest, scope) = parse_cfunction_symbol_scope(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(scope, CFunctionSymbolScope::External);

        let s = "private";
        let (rest, scope) = parse_cfunction_symbol_scope(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(scope, CFunctionSymbolScope::Private);

        let s = "public";
        let (rest, scope) = parse_cfunction_symbol_scope(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(scope, CFunctionSymbolScope::Public);
    }

    #[test]
    fn test_parse_ctype() {
        let s = r#"(< Objptr >)"#;
        let (rest, ty) = parse_ctype(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(ty, CType::Objptr);

        let s = r#"(< Word64 >)"#;
        let (rest, ty) = parse_ctype(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(ty, CType::Word64);
    }

    #[test]
    fn test_parse_cfunction_prototype() {
        let s = r#"prototype {args = ( (< Objptr >) ),
                                            res = None},"#;
        let (rest, (args, ret)) = parse_cfunction_prototype(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(args.len(), 1);
        assert_eq!(args[0], CType::Objptr);
        assert_eq!(ret, None);
    }

    #[test]
    fn test_parse_cfunction_target() {
        let s = r#"target {type = Direct, name = "Stdio_print"},"#;
        let (rest, target) = parse_cfunction_target(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(target, CFunctionTarget::Direct("Stdio_print".to_string()));

        let s = r#"target {type = Indirect},"#;
        let (rest, target) = parse_cfunction_target(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(target, CFunctionTarget::Indirect);
    }

    #[test]
    fn test_parse_cfunction() {
        let s = r#"
        CFunction {args = ((< Vector( Word( w8 ) ) >)),
                   convention = cdecl,
                   inline = false,
                   kind = Impure,
                   prototype = prototype {args = ((< Objptr >)),
                                          res = None},
                   return = (< Tuple () >),
                   symbolScope = private,
                   target = target {type = Direct,
                                    name = "Stdio_print"}},
        "#;
        let (rest, prim) = parse_cfunction(s).unwrap();
        assert_eq!(rest.trim(), ",");
        assert_eq!(
            prim,
            PrimPrimitive::CFunction {
                args: vec![SmlType::Vector(Box::new(SmlType::Word(WordSize::W8)))],
                convention: CFunctionConvention::Cdecl,
                inline: false,
                kind: CFunctionKind::Impure,
                prototype: (vec![CType::Objptr], None),
                ret: SmlType::Tuple(vec![]),
                symbol_scope: CFunctionSymbolScope::Private,
                target: CFunctionTarget::Direct("Stdio_print".to_string()),
            }
        );
    }

    #[test]
    fn test_parse_prim_kind() {
        let s = "DependsOnState";
        let (rest, kind) = parse_prim_kind(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(kind, PrimKind::DependsOnState);

        let s = "Functional";
        let (rest, kind) = parse_prim_kind(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(kind, PrimKind::Functional);

        let s = "Moveable";
        let (rest, kind) = parse_prim_kind(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(kind, PrimKind::Moveable);

        let s = "SideEffect";
        let (rest, kind) = parse_prim_kind(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(kind, PrimKind::SideEffect);
    }

    #[test]
    fn test_parse_prim() {
        let s = r#"primitive {prim = "Ref_ref",
                                   kind = DependsOnState,
                                 }"#;
        let (rest, prim) = parse_prim(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(prim.prim, PrimPrimitive::SmlPrim("Ref_ref".to_string()));
        assert_eq!(prim.kind, PrimKind::DependsOnState);

        let s = r#"
        primitive {prim = CFunction {args = ((< Vector( Word( w8 ) ) >)),
                                     convention = cdecl,
                                     inline = false,
                                     kind = Impure,
                                     prototype = prototype {args = ((< Objptr >)),
                                                            res = None},
                                     return = (< Tuple () >),
                                     symbolScope = private,
                                     target = target {type = Direct,
                                                      name = "Stdio_print"}},
                   kind = DependsOnState},
        "#;
        let (rest, prim) = parse_prim(s).unwrap();
        assert_eq!(rest.trim(), ",");
        assert_eq!(
            prim.prim,
            PrimPrimitive::CFunction {
                args: vec![SmlType::Vector(Box::new(SmlType::Word(WordSize::W8)))],
                convention: CFunctionConvention::Cdecl,
                inline: false,
                kind: CFunctionKind::Impure,
                prototype: (vec![CType::Objptr], None),
                ret: SmlType::Tuple(vec![]),
                symbol_scope: CFunctionSymbolScope::Private,
                target: CFunctionTarget::Direct("Stdio_print".to_string()),
            }
        );
        assert_eq!(prim.kind, PrimKind::DependsOnState);
    }

    #[test]
    fn test_parse_exp_primapp() {
        let s = r#"
        exp::PrimApp {prim = primitive {prim = CFunction {args = ((< Vector( Word( w8 ) ) >)),
                                                          convention = cdecl,
                                                          inline = false,
                                                          kind = Impure,
                                                          prototype = prototype {args = ((< Objptr >)),
                                                                                 res = None},
                                                          return = (< Tuple () >),
                                                          symbolScope = private,
                                                          target = target {type = Direct,
                                                                           name = "Stdio_print"}},
                                        kind = DependsOnState},
                      args = (global_37)}},
        "#;
        let (rest, exp) = parse_exp_primapp(s).unwrap();
        assert_eq!(rest.trim(), "},");
        assert_eq!(
            exp,
            Exp::PrimApp {
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
        );
    }

    #[test]
    fn test_parse_exp_select() {
        let s = r#"exp::Select {  tuple = x_108,
                                   offset = 0,}"#;
        let (rest, exp) = parse_exp_select(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            exp,
            Exp::Select {
                tuple: "x_108".to_string(),
                offset: 0
            }
        );
    }

    #[test]
    fn test_parse_exp() {
        let s = r#"exp::Var { var = x_0, }"#;
        let (rest, exp) = parse_exp(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(exp, Exp::Var("x_0".to_string()));

        let s = r#"exp::Tuple { args = (global_15, global_14), } ,"#;
        let (rest, exp) = parse_exp(s).unwrap();
        assert_eq!(rest, " ,");
        assert_eq!(
            exp,
            Exp::Tuple(vec!["global_15".to_string(), "global_14".to_string()])
        );

        let s = r#"exp::Select {  tuple = x_108,
                                   offset = 0,}"#;
        let (rest, exp) = parse_exp(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            exp,
            Exp::Select {
                tuple: "x_108".to_string(),
                offset: 0
            }
        );

        let s = r#"exp::ConApp {
                                   con = "::_1",
                                   args = 
                                   (global_43, x_49) ,
                                 }"#;
        let (rest, exp) = parse_exp(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            exp,
            Exp::ConApp {
                con: "::_1".to_string(),
                args: vec!["global_43".to_string(), "x_49".to_string()]
            }
        );

        let s = r#"exp::Const { const = const::Null {} }"#;
        let (rest, exp) = parse_exp(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(exp, Exp::Const(Const::Null));
    }

    #[test]
    fn test_parse_statement() {
        let s = r#"
            statement {var = Some global_0,
                       type = (< Vector( Word( w8 ) ) >),
                       exp = exp::Const { const = const::WordVector { const = "unhandled exception: "}}},
        "#;
        let (rest, stmt) = parse_statement(s).unwrap();
        assert_eq!(rest.trim(), ",");
        assert_eq!(stmt.var, Some("global_0".to_string()));
        assert_eq!(
            stmt.ty,
            SmlType::Vector(Box::new(SmlType::Word(WordSize::W8)))
        );
        assert_eq!(
            stmt.exp,
            Exp::Const(crate::ssa::Const::WordVector(
                "unhandled exception: ".to_string()
            ))
        );

        let s = r#"statement {
             var = None, 
             type = (< Vector( Word( w8 ) ) >),
             exp = exp::Const { const = const::WordVector { const = "unhandled exception: "}}}
        "#;
        let (rest, stmt) = parse_statement(s).unwrap();
        assert_eq!(rest.trim(), "");
        assert_eq!(
            stmt,
            Statement {
                var: None,
                ty: SmlType::Vector(Box::new(SmlType::Word(WordSize::W8))),
                exp: Exp::Const(crate::ssa::Const::WordVector(
                    "unhandled exception: ".to_string()
                ))
            }
        );
    }

    #[test]
    fn test_parse_globals() {
        let s = r#"
        (statement {var = Some global_0,
                                type = (< Vector( Word( w8 ) ) >),
                                exp = exp::Const {const = const::WordVector {const = "unhandled exception: "}}},
                     statement {var = Some global_1,
                                type = (< Vector( Word( w8 ) ) >),
                                exp = exp::Const {const = const::WordVector {const = "Overflow"}}})
        "#;
        let (rest, globals) = parse_globals(s).unwrap();
        assert_eq!(rest.trim(), "");
        assert_eq!(globals.len(), 2);
        assert_eq!(
            globals[0],
            Statement {
                var: Some("global_0".to_string()),
                ty: SmlType::Vector(Box::new(SmlType::Word(WordSize::W8))),
                exp: Exp::Const(crate::ssa::Const::WordVector(
                    "unhandled exception: ".to_string()
                ))
            }
        );
        assert_eq!(
            globals[1],
            Statement {
                var: Some("global_1".to_string()),
                ty: SmlType::Vector(Box::new(SmlType::Word(WordSize::W8))),
                exp: Exp::Const(crate::ssa::Const::WordVector("Overflow".to_string()))
            }
        );
    }

    #[test]
    fn test_parse_typed_var() {
        let s = "x_0 : (< Word( w32 ) >) ";
        let (rest, var) = parse_typed_var(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(var.name, "x_0".to_string());
        assert_eq!(var.ty, SmlType::Word(WordSize::W32));
    }

    #[test]
    fn test_parse_transfer_bug() {
        let s = r#"transfer::Bug { },"#;
        let (rest, transfer) = parse_transfer_bug(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(transfer, Transfer::Bug);
    }

    #[test]
    fn test_parse_transfer_call_dead() {
        let s = r#"transfer::call::Dead {
             func = call_0,
             args = (x_0, x_1) ,
           },"#;
        let (rest, transfer) = parse_transfer_call_dead(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Call {
                func: "call_0".to_string(),
                args: vec!["x_0".to_string(), "x_1".to_string()],
                ret: Return::Dead
            }
        );
    }

    #[test]
    fn test_parse_transfer_call_non_tail_handler() {
        let s = r#"handler::Caller { }"#;
        let (rest, handler) = parse_transfer_call_non_tail_handler(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(handler, Handler::Caller);

        let s = r#"handler::Dead { }"#;
        let (rest, handler) = parse_transfer_call_non_tail_handler(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(handler, Handler::Dead);

        let s = r#"handler::Handle { label = L_0, }"#;
        let (rest, handler) = parse_transfer_call_non_tail_handler(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            handler,
            Handler::Handle {
                label: "L_0".to_string()
            }
        );
    }

    #[test]
    fn test_parse_transfer_call_non_tail() {
        let s = r#"transfer::call::NonTail {
                                         func = exit_0,
                                         args = (global_4, x_87, x_86, exiting_0),
                                         cont = L_58,
                                         handler = handler::Handle { label = L_57, },
                                       },"#;
        let (rest, transfer) = parse_transfer_call_non_tail(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Call {
                func: "exit_0".to_string(),
                args: vec![
                    "global_4".to_string(),
                    "x_87".to_string(),
                    "x_86".to_string(),
                    "exiting_0".to_string()
                ],
                ret: Return::NonTail {
                    cont: "L_58".to_string(),
                    handler: Handler::Handle {
                        label: "L_57".to_string()
                    }
                }
            }
        );
    }

    #[test]
    fn test_parse_transfer_call_tail() {
        let s = r#"transfer::call::Tail {
             func = call_1,
             args = (x_2) ,
           },"#;
        let (rest, transfer) = parse_transfer_call_tail(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Call {
                func: "call_1".to_string(),
                args: vec!["x_2".to_string()],
                ret: Return::Tail
            }
        );
    }

    #[test]
    fn test_parse_transfer_call() {
        let s = r#"transfer::call::Dead {
             func = call_0,
             args = (x_0, x_1) ,
           },"#;
        let (rest, transfer) = parse_transfer_call(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Call {
                func: "call_0".to_string(),
                args: vec!["x_0".to_string(), "x_1".to_string()],
                ret: Return::Dead
            }
        );
    }

    #[test]
    fn test_parse_transfer_case_con_con() {
        let s = r#"SomeConstructorId => L_0"#;
        let (rest, (con_id, label)) = parse_transfer_case_con_con(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(con_id, "SomeConstructorId".to_string());
        assert_eq!(label, "L_0".to_string());
    }

    #[test]
    fn test_parse_transfer_case_con() {
        let s = r#"transfer::case::Con {
                                         test = x_82,
                                         cases = 
                                           (nil_0 => L_56, ::_2 => loop_6)
                                         ,
                                       },"#;
        let (rest, transfer) = parse_transfer_case_con(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Case {
                test: "x_82".to_string(),
                cases: Cases::Con(vec![
                    ("nil_0".to_string(), "L_56".to_string()),
                    ("::_2".to_string(), "loop_6".to_string())
                ]),
                default: None
            }
        );

        let s = r#"transfer::case::Con {
                                         test = x_82,
                                         cases = 
                                           (nil_0 => L_56, ::_2 => loop_6)
                                         ,
                                         default = L_0,
                                       },"#;
        let (rest, transfer) = parse_transfer_case_con(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Case {
                test: "x_82".to_string(),
                cases: Cases::Con(vec![
                    ("nil_0".to_string(), "L_56".to_string()),
                    ("::_2".to_string(), "loop_6".to_string())
                ]),
                default: Some("L_0".to_string())
            }
        );
    }

    #[test]
    fn test_parse_case_word_con() {
        let s = r#"0x0000000000000000:w64 => L_1"#;
        let (rest, ((word, sz), label)) = parse_transfer_case_word_con(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(word, 0);
        assert_eq!(sz, WordSize::W64);
        assert_eq!(label, "L_1".to_string());
    }

    #[test]
    fn test_parse_transfer_case_word() {
        let s = r#"transfer::case::Word {
                test = x_0,
                size = w64,
                cases = 
                  (0x0000000000000000:w64 => L_1, 0x0000000000000001:w64 => L_2)
                ,
              },"#;
        let (rest, transfer) = parse_transfer_case_word(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Case {
                test: "x_0".to_string(),
                cases: Cases::Word(
                    WordSize::W64,
                    vec![(0, "L_1".to_string()), (1, "L_2".to_string())]
                ),
                default: None
            }
        );
    }

    #[test]
    fn test_parse_transfer_case() {
        let s = r#"transfer::case::Con {
                test = x_82,
                cases = 
                (nil_0 => L_56, ::_2 => loop_6)
                ,
            },"#;

        let (rest, transfer) = parse_transfer_case(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Case {
                test: "x_82".to_string(),
                cases: Cases::Con(vec![
                    ("nil_0".to_string(), "L_56".to_string()),
                    ("::_2".to_string(), "loop_6".to_string())
                ]),
                default: None
            }
        );
    }

    #[test]
    fn test_parse_transfer_goto() {
        let s = r#"transfer::Goto {
                                         dst = loop_5,
                                         args = (x_77, x_79, global_6),
                                       },"#;
        let (rest, transfer) = parse_transfer_goto(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Goto {
                dst: "loop_5".to_string(),
                args: vec![
                    "x_77".to_string(),
                    "x_79".to_string(),
                    "global_6".to_string()
                ]
            }
        );
    }

    #[test]
    fn test_parse_transfer_raise() {
        let s = r#"transfer::Raise { args = (), }"#;
        let (rest, transfer) = parse_transfer_raise(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(transfer, Transfer::Raise { args: vec![] });
    }

    #[test]
    fn test_parse_transfer_return() {
        let s = r#"transfer::Return { args = (x_320), },"#;
        let (rest, transfer) = parse_transfer_return(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Return {
                args: vec!["x_320".to_string()]
            }
        );
    }

    #[test]
    fn test_parse_transfer() {
        let s = r#"transfer::Goto {
                                            dst = loop_5,
                                            args = (x_77, x_79, global_6),
                                            },"#;
        let (rest, transfer) = parse_transfer(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(
            transfer,
            Transfer::Goto {
                dst: "loop_5".to_string(),
                args: vec![
                    "x_77".to_string(),
                    "x_79".to_string(),
                    "global_6".to_string()
                ]
            }
        );
    }

    #[test]
    fn test_parse_block() {
        let s = r#"
        block {label = L_57,
               args = (),
               statements = (statement {var = None,
                                        type = (< Tuple () >),
                                        exp = exp::PrimApp {prim = primitive {prim = CFunction {args = ((< Vector( Word( w8 ) ) >)),
                                                                                                convention = cdecl,
                                                                                                inline = false,
                                                                                                kind = Impure,
                                                                                                prototype = prototype {args = ((< Objptr >)),
                                                                                                                       res = None},
                                                                                                return = (< Tuple () >),
                                                                                                symbolScope = private,
                                                                                                target = target {type = Direct,
                                                                                                                 name = "Stdio_print"}},
                                                                              kind = DependsOnState},
                                                            args = (global_37)}},
                             statement {var = None,
                                        type = (< Tuple () >),
                                        exp = exp::PrimApp {prim = primitive {prim = "MLton_halt",
                                                                              kind = DependsOnState},
                                                            args = (global_5)}},
                             statement {var = None,
                                        type = (< Tuple () >),
                                        exp = exp::PrimApp {prim = primitive {prim = "MLton_bug",
                                                                              kind = DependsOnState},
                                                            args = (global_28)}}),
               transfer = transfer::Bug {}},
        "#;
        let (rest, block) = parse_block(s).unwrap();
        assert_eq!(rest.trim(), ",");
        assert_eq!(block.label, "L_57".to_string());
        assert_eq!(block.args, vec![]);
        assert_eq!(block.statements.len(), 3);
        assert_eq!(
            block.statements[0],
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
            }
        );
        assert_eq!(block.transfer, Transfer::Bug);
    }

    #[test]
    fn test_parse_function() {
        let s = r#"
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
                                                                      global_9)}},
                            block {label = loop_5,
                                   args = (x_78:
                                             (< Datatype( "list_3" ) >),
                                           x_81:
                                             (< Word( w64 ) >),
                                           x_120:
                                             (< Word( w64 ) >)),
                                   statements = (statement {var = Some x_121,
                                                            type = (< Datatype( "bool" ) >),
                                                            exp = exp::PrimApp {prim = primitive {prim = "Word64_equal",
                                                                                                  kind = DependsOnState},
                                                                                args = (x_120,
                                                                                        global_6)}}),
                                   transfer = transfer::case::Con {test = x_121,
                                                                   cases =   (true => L_80,
                                                                              false => L_55)}})}
        "#;
        let (rest, func) = parse_function(s).unwrap();
        assert_eq!(rest.trim(), "");
        assert_eq!(func.name, "main_0".to_string());
        assert_eq!(func.may_inline, false);
        assert_eq!(func.args.len(), 0);
        assert_eq!(func.start, "L_52".to_string());
        assert_eq!(func.returns, None);
        assert_eq!(func.raises, None);
        assert_eq!(func.blocks.len(), 2);
        assert_eq!(func.blocks[0].label, "L_52".to_string());
    }

    #[test]
    fn test_parse_ssa() {
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
                                                                exp = exp::PrimApp {prim = primitive {prim = "Ref_deref",
                                                                                                      kind = DependsOnState},
                                                                                    args = (x_0),
                                                                                    targs = ((< Datatype( "bool" ) >))}}),
                                       transfer = transfer::case::Con {test = x_76,
                                                                       cases =   (true => L_51,
                                                                                  false => L_50)}})}
        ),
        main = main_0,
        }"#;

        let (rest, ssa) = parse_ssa(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(ssa.datatypes.len(), 2);
        assert_eq!(ssa.globals.len(), 2);
        assert_eq!(ssa.functions.len(), 2);
        assert_eq!(ssa.main, "main_0".to_string());
    }

    use std::fs::read_to_string;

    #[test]
    fn test_parse_file() {
        let s = read_to_string("tests/test.ssa").unwrap();

        let (rest, ssa) = parse_ssa(&s).unwrap();
        assert_eq!(rest.trim(), "");
        assert_eq!(ssa.datatypes.len(), 6);
        assert_eq!(ssa.globals.len(), 49);
        assert_eq!(ssa.functions.len(), 2);
        assert_eq!(ssa.main, "main_0".to_string());
    }
}
