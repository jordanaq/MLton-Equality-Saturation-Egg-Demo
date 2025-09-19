use nom::{
    AsChar, IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::multispace0,
    sequence::{delimited, preceded, separated_pair, terminated},
};

use sml_utils::SmlType;

use parse_utils::word;

use crate::fpeg::{Constr, Lit, Param, Prim};

fn parse_hex(s: &str) -> IResult<&str, u64> {
    let (rest, hex) = preceded(tag("0x"), take_while1(AsChar::is_hex_digit)).parse(s)?;
    Ok((rest, u64::from_str_radix(hex, 16).unwrap()))
}

fn parse_w8(s: &str) -> IResult<&str, Lit> {
    let (rest, w8) = terminated(
        parse_hex,
        (
            multispace0(),
            tag(":"),
            multispace0(),
            alt((tag("word8"), tag("w8"))),
        ),
    )
    .parse(s)?;
    Ok((rest, Lit::Word8(u8::try_from(w8).unwrap())))
}

fn parse_w16(s: &str) -> IResult<&str, Lit> {
    let (rest, w16) = terminated(
        parse_hex,
        (
            multispace0(),
            tag(":"),
            multispace0(),
            alt((tag("word16"), tag("w16"))),
        ),
    )
    .parse(s)?;
    Ok((rest, Lit::Word16(u16::try_from(w16).unwrap())))
}

fn parse_w32(s: &str) -> IResult<&str, Lit> {
    let (rest, w32) = terminated(
        parse_hex,
        (
            multispace0(),
            tag(":"),
            multispace0(),
            alt((tag("word32"), tag("w32"))),
        ),
    )
    .parse(s)?;
    Ok((rest, Lit::Word32(u32::try_from(w32).unwrap())))
}

fn parse_w64(s: &str) -> IResult<&str, Lit> {
    let (rest, w64) = terminated(
        parse_hex,
        (
            multispace0(),
            tag(":"),
            multispace0(),
            alt((tag("word64"), tag("w64"))),
        ),
    )
    .parse(s)?;
    Ok((rest, Lit::Word64(u64::try_from(w64).unwrap())))
}

fn parse_unit(s: &str) -> IResult<&str, Lit> {
    let (rest, _unit) = tag("()").parse(s)?;
    Ok((rest, Lit::Unit))
}

pub fn parse_lit(s: &str) -> IResult<&str, Lit> {
    delimited(
        (tag("Lit<"), multispace0()),
        alt((parse_w8, parse_w16, parse_w32, parse_w64, parse_unit)),
        (multispace0(), tag(">")),
    )
    .parse(s)
}

fn parse_type(s: &str) -> IResult<&str, SmlType> {
    let (rest, s) = word().parse(s)?;

    Ok((rest, s.to_owned()))
}

pub fn parse_prim(s: &str) -> IResult<&str, Prim> {
    let (rest, prim_r) = delimited(tag("Prim<"), word(), tag(">")).parse(s)?;
    Ok((rest, Prim(prim_r.to_string())))
}

pub fn parse_constr(s: &str) -> IResult<&str, Constr> {
    let (rest, (type_s, constr_r)) = (delimited(
        tag("Constr<"),
        (parse_type, preceded(tag("::"), word())),
        tag(">"),
    ))
    .parse(s)?;

    Ok((
        rest,
        Constr {
            constr_type: type_s,
            name: constr_r.to_owned(),
        },
    ))
}

pub fn parse_param(s: &str) -> IResult<&str, Param> {
    let (rest, (param_r, t_s)) = delimited(
        tag("Param<"),
        separated_pair(word(), (multispace0(), tag(":"), multispace0()), parse_type),
        tag(">"),
    )
    .parse(s)?;
    Ok((rest, Param(param_r.to_owned(), t_s)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_hex() {
        assert_eq!(parse_hex("0x0"), Ok(("", 0)));
    }

    #[test]
    fn test_parse_w8() {
        assert_eq!(parse_w8("0x0:w8"), Ok(("", Lit::Word8(0))));
        assert_eq!(parse_w8("0xB:w8"), Ok(("", Lit::Word8(11))));
        assert_eq!(parse_w8("0xb:w8"), Ok(("", Lit::Word8(0xb))));
    }

    #[test]
    fn test_parse_w16() {
        assert_eq!(parse_w16("0x0:w16"), Ok(("", Lit::Word16(0))));
        assert_eq!(parse_w16("0x100:w16"), Ok(("", Lit::Word16(0x100))));
    }

    #[test]
    fn test_parse_w32() {
        assert_eq!(parse_w32("0x0:w32"), Ok(("", Lit::Word32(0))));
        assert_eq!(parse_w32("0x10000:w32"), Ok(("", Lit::Word32(0x10000))));
    }

    #[test]
    fn test_parse_w64() {
        assert_eq!(parse_w64("0x0:w64"), Ok(("", Lit::Word64(0))));
        assert_eq!(
            parse_w64("0x100000000:w64"),
            Ok(("", Lit::Word64(0x100000000)))
        );
    }

    #[test]
    fn test_parse_unit() {
        assert_eq!(parse_unit("()"), Ok(("", Lit::Unit)));
    }

    #[test]
    fn test_parse_lit() {
        assert_eq!(parse_lit("Lit<0x0:w8>"), Ok(("", Lit::Word8(0))));
        assert_eq!(parse_lit("Lit<0x0:w16>"), Ok(("", Lit::Word16(0))));
        assert_eq!(parse_lit("Lit<0x0:w32>"), Ok(("", Lit::Word32(0))));
        assert_eq!(parse_lit("Lit<0x0:w64>"), Ok(("", Lit::Word64(0))));
        assert_eq!(parse_lit("Lit<()>"), Ok(("", Lit::Unit)));
    }

    #[test]
    fn test_parse_prim() {
        let add = "Add".to_owned();
        assert_eq!(parse_prim("Prim<Add>"), Ok(("", Prim(add))));
    }

    #[test]
    fn test_parse_constr() {
        let add = "Add".to_owned();
        let t = "T".to_owned();
        assert_eq!(
            parse_constr("Constr<T::Add>"),
            Ok((
                "",
                Constr {
                    constr_type: t,
                    name: add
                }
            ))
        );
    }

    #[test]
    fn test_parse_param() {
        let x = "x".to_owned();
        let t = "T".to_owned();
        assert_eq!(parse_param("Param<x : T>"), Ok(("", Param(x, t))));
    }
}
