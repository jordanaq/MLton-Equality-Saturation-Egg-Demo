use u64;

use nom::{
    AsChar,
    branch::{
        alt,
    },
    bytes::complete::{
        tag,
        take_while1,
    },
    sequence::{
        preceded,
        terminated,
    },
    IResult,
    Parser,
};

use crate::fpeg::{
    Lit,
    Prim,
};


fn parse_hex(s: &str) -> IResult<&str, u64> {
    let (rest, hex) = preceded(tag("0x"), take_while1(AsChar::is_hex_digit)).parse(s)?;
    Ok((rest, u64::from_str_radix(hex, 16).unwrap()))
}

fn parse_w8(s: &str) -> IResult<&str, Lit> {
    let (rest, w64): (&str, u64) = terminated(parse_hex, tag(":w8")).parse(s)?;
    Ok((rest, Lit::Word8(u8::try_from(w64).unwrap())))
}

fn parse_w32(s: &str) -> IResult<&str, Lit> {
    let (rest, w64) = terminated(parse_hex, tag(":w32")).parse(s)?;
    Ok((rest, Lit::Word32(u32::try_from(w64).unwrap())))
}

fn parse_w64(s: &str) -> IResult<&str, Lit> {
    let (rest, w64) = terminated(parse_hex, tag(":w64")).parse(s)?;
    Ok((rest, Lit::Word64(w64)))
}

pub fn parse_lit(s: &str) -> IResult<&str, Lit> {
    alt((parse_w8, parse_w32, parse_w64)).parse(s)
}

pub fn parse_prim(s: &str) -> IResult<&str, Prim> {
    todo!()
}

pub fn parse_constr(s: &str) -> IResult<&str, Prim> {
    todo!()
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
    fn test_parse_w32() {
        assert_eq!(parse_w32("0x0:w32"), Ok(("", Lit::Word32(0))));
        assert_eq!(parse_w32("0x10000:w32"), Ok(("", Lit::Word32(0x10000))));
    }

    #[test]
    fn test_parse_w64() {
        assert_eq!(parse_w64("0x0:w64"), Ok(("", Lit::Word64(0))));
        assert_eq!(parse_w64("0x100000000:w64"), Ok(("", Lit::Word64(0x100000000))));
    }

    #[test]
    fn parse_lits() {
        assert_eq!(parse_lit("0x0:w8"), Ok(("", Lit::Word8(0))));
        assert_eq!(parse_lit("0x0:w32"), Ok(("", Lit::Word32(0))));
        assert_eq!(parse_w64("0x0:w64"), Ok(("", Lit::Word64(0))));
    }
}
