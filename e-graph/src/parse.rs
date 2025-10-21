use std::str::FromStr;

use nom::{
    AsChar, IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::multispace0,
    sequence::{delimited, preceded, separated_pair, terminated},
};

use mlton_ssa::{
    parse::{parse_const, parse_prim, parse_sml_type, parse_sml_types, parse_var_name},
    ssa::SmlType,
};

use parse_utils::*;

use crate::fpeg::{Constr, FPeg, FPegL, PrimWrapper, Region};

pub fn parse_region(s: &str) -> IResult<&str, Region> {
    delimited(
        tag("Region<"),
        take_while1(|c: char| c.is_digit(10)).map(|s: &str| s.parse::<usize>().unwrap().into()),
        tag(">"),
    )
    .parse(s)
}

pub fn parse_regions(s: &str) -> IResult<&str, Vec<Region>> {
    paren_list_parser(parse_region).parse(s)
}

pub fn parse_constr(s: &str) -> IResult<&str, Constr> {
    named_object_parser(
        "Constr",
        (
            parse_key_field("constr_type", parse_sml_type),
            parse_key_field("tycon", parse_string),
        ),
    )
    .map(|(constr_type, tycon)| Constr { constr_type, tycon })
    .parse(s)
}

impl FromStr for Constr {
    type Err = nom::Err<nom::error::Error<String>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_constr(s) {
            Ok(("", constr)) => Ok(constr),
            Ok((_, _)) => Err(nom::Err::Error(nom::error::Error::new(
                s.to_string(),
                nom::error::ErrorKind::Eof,
            ))),
            Err(e) => Err(e.map_input(|input| input.to_string())),
        }
    }
}

fn parse_prim_wrapper(s: &str) -> IResult<&str, PrimWrapper> {
    named_object_parser(
        "PrimWrapper",
        (
            parse_key_field("prim", parse_prim),
            parse_key_field("targs", option_parser(parse_sml_types)),
        ),
    )
    .map(|(prim, targs)| PrimWrapper { prim, targs })
    .parse(s)
}

impl FromStr for PrimWrapper {
    type Err = nom::Err<nom::error::Error<String>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_prim_wrapper(s) {
            Ok(("", prim_wrapper)) => Ok(prim_wrapper),
            Ok((_, _)) => Err(nom::Err::Error(nom::error::Error::new(
                s.to_string(),
                nom::error::ErrorKind::Eof,
            ))),
            Err(e) => Err(e.map_input(|input| input.to_string())),
        }
    }
}

/*
fn parse_fpegl_prim_app(s: &str) -> IResult<&str, FPegL> {
    named_object_parser(
        "PrimApp",
        (
            parse_key_field("prim", parse_prim_wrapper),
            parse_key_field("args", parse_regions),
        ),
    )
    .map(|(prim, args)| FPegL::PrimApp(prim, args.into_boxed_slice()))
    .parse(s)
}

fn parse_fpegl_construct(s: &str) -> IResult<&str, FPegL> {
    named_object_parser(
        "Construct",
        (
            parse_key_field("constr", parse_constr),
            parse_key_field("args", parse_regions),
        ),
    )
    .map(|(constr, args)| FPegL::Construct(constr, args.into_boxed_slice()))
    .parse(s)
}

fn parse_fpegl_select(s: &str) -> IResult<&str, FPegL> {
    named_object_parser(
        "Select",
        (
            parse_key_field("tuple", parse_region),
            parse_key_field("offset", parse_region),
        )
    )
    .map(|(r1, r2)| FPegL::Select([r1, r2]))
    .parse(s)
}

fn parse_fpegl_tuple(s: &str) -> IResult<&str, FPegL> {
    named_object_parser(
        "Tuple",
        parse_key_field("args", parse_regions),
    )
    .map(|args| FPegL::Tuple(args.into_boxed_slice()))
    .parse(s)
}

fn parse_fpegl_literal(s: &str) -> IResult<&str, FPegL> {
    named_object_parser(
        "Literal",
        parse_key_field("const", parse_const),
    )
    .map(|c| FPegL::Literal(c))
    .parse(s)
}

fn parse_fpegl_arg(s: &str) -> IResult<&str, FPegL> {
    named_object_parser(
        "Arg",
        parse_key_field("name", parse_string),
    )
    .map(|v| FPegL::Arg(v))
    .parse(s)
}

pub fn parse_fpegl(s: &str) -> IResult<&str, FPegL> {
    alt((
        parse_fpegl_prim_app,
        parse_fpegl_construct,
        parse_fpegl_select,
        parse_fpegl_tuple,
        parse_fpegl_literal,
        parse_fpegl_arg,
    ))
    .parse(s)
}

pub fn parse_eclass(s: &str) -> IResult<&str, (Region, Vec<FPegL>)> {
    separated_pair(parse_region, multispace0(), paren_list_parser(parse_fpegl))
        .parse(s)
}

pub fn parse_fpeg(s: &str) -> IResult<&str, FPeg> {
    named_object_parser("fpeg", (
        parse_key_field("egraph", paren_list_parser(parse_eclass)),
    ))


}
*/

pub fn parse_fpeg(s: &str) -> IResult<&str, FPeg> {
    todo!()
}

#[cfg(test)]
mod tests {
    use mlton_ssa::ssa::{
        self, Cases, Const, FunctionId, Label, PrimPrimitive, Return, SmlType, VarId, WordSize,
    };

    use crate::fpeg::PrimWrapper;

    use super::*;

    #[test]
    fn test_parse_constr() {
        let s = r#"Constr { constr_type = (< Datatype("list_0") >), tycon = "nil_0" }"#;
        let (_, constr) = parse_constr(s).unwrap();
        assert_eq!(
            constr,
            Constr {
                constr_type: SmlType::Datatype("list_0".into()),
                tycon: "nil_0".into()
            }
        );
    }

    #[test]
    fn test_constr_from_str() {
        let s = r#"Constr { constr_type = (< Datatype("list_0") >), tycon = "nil_0" }"#;
        let constr: Constr = s.parse().unwrap();
        assert_eq!(
            constr,
            Constr {
                constr_type: SmlType::Datatype("list_0".into()),
                tycon: "nil_0".into()
            }
        );
    }

    #[test]
    fn test_parse_prim_wrapper() {
        let s = r#"PrimWrapper {
            prim = primitive { 
                prim = "add_w64",
                kind = Functional,
            },
            targs = Some ((< Word(w64) >), (< Word(w64) >) )
        }"#;

        let (_, prim_wrapper) = parse_prim_wrapper(s).unwrap();
        assert_eq!(
            prim_wrapper,
            PrimWrapper {
                prim: ssa::Prim::make_pure_sml("add_w64"),
                targs: Some(vec![
                    SmlType::Word(WordSize::W64),
                    SmlType::Word(WordSize::W64)
                ]),
            }
        );
    }

    #[test]
    fn test_prim_wrapper_from_str() {
        let s = r#"PrimWrapper {
            prim = primitive { 
                prim = "add_w64",
                kind = Functional,
            },
            targs = Some ((< Word(w64) >), (< Word(w64) >) )
        }"#;
        let prim_wrapper: PrimWrapper = s.parse().unwrap();
        assert_eq!(
            prim_wrapper,
            PrimWrapper {
                prim: ssa::Prim::make_pure_sml("add_w64"),
                targs: Some(vec![
                    SmlType::Word(WordSize::W64),
                    SmlType::Word(WordSize::W64)
                ]),
            }
        );
    }

    /*
    #[test]
    fn test_parse_fpegl() {
        let s = r#"PrimApp {
            prim = PrimWrapper { 
                prim = primitive { 
                    prim = "add_w64",
                    kind = Functional,
                },
                targs = Some ((< Word(w64) >), (< Word(w64) >) )
            },
            args = ( Region<1>, Region<2> )
        }"#;
        let (_, fpegl) = parse_fpegl(s).unwrap();
        assert_eq!(
            fpegl,
            FPegL::PrimApp(
                PrimWrapper {
                    prim: ssa::Prim::make_pure_sml("add_w64"),
                    targs: Some(vec![
                        SmlType::Word(WordSize::W64),
                        SmlType::Word(WordSize::W64)
                    ]),
                },
                vec![Region::from(1), Region::from(2)].into_boxed_slice()
            )
        );

        let s = r#"Construct {
            constr = Constr { constr_type = (< Datatype("list_0") >), tycon = "nil_0" },
            args = ( Region<1>, Region<2> )
        }"#;
        let (_, fpegl) = parse_fpegl(s).unwrap();
        assert_eq!(
            fpegl,
            FPegL::Construct(
                Constr {
                    constr_type: SmlType::Datatype("list_0".into()),
                    tycon: "nil_0".into(),
                },
                vec![Region::from(1), Region::from(2)].into_boxed_slice()
            )
        );

        let s = r#"Select {
            tuple = Region<1>,
            offset = Region<2>
        }"#;
        let (_, fpegl) = parse_fpegl(s).unwrap();
        assert_eq!(
            fpegl,
            FPegL::Select([Region::from(1), Region::from(2)])
        );

        let s = r#"Tuple {
            args = ( Region<1>, Region<2> )
        }"#;
        let (_, fpegl) = parse_fpegl(s).unwrap();
        assert_eq!(
            fpegl,
            FPegL::Tuple(vec![Region::from(1), Region::from(2)].into_boxed_slice())
        );

        let s = r#"Literal {
            const = const::Word {
                const = 0xFF:w16
            }
        }"#;
        let (_, fpegl) = parse_fpegl(s).unwrap();
        assert_eq!(fpegl, FPegL::Literal(Const::Word(WordSize::W16, 0xFF)));

        let s = r#"Arg {
            name = "x"
        }"#;
        let (_, fpegl) = parse_fpegl(s).unwrap();
        assert_eq!(fpegl, FPegL::Arg("x".into()));
    }
    */
}
