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
    number::complete::{
        u8 as parse_u8,
        u32 as parse_u32,
    },
    sequence::{
        preceded,
        terminated,
    },
    IResult,
    Parser,
};

use crate::fpeg::{
    FPeg,
    Prim,
};


fn parse_hex(s: &str) -> IResult<&str, u64> {
    let (rest, hex) = preceded(tag("0x"), take_while1(AsChar::is_hex_digit)).parse(s)?;
    Ok((rest, u64::from_str_radix(hex, 16).unwrap()))
}

fn parse_w8(s: &str) -> IResult<&str, Prim> {
    let (rest, w64): (&str, u64) = terminated(parse_hex, tag(":w8")).parse(s)?;
    Ok((rest, Prim::Word8(u8::try_from(w64).unwrap())))
}

fn parse_w32(s: &str) -> IResult<&str, Prim> {
    let (rest, w64) = terminated(parse_hex, tag(":w32")).parse(s)?;
    Ok((rest, Prim::Word32(u32::try_from(w64).unwrap())))
}

pub fn parse_prim(s: &str) -> IResult<&str, Prim> {
    alt((parse_w8, parse_w32)).parse(s)
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
        assert_eq!(parse_w8("0x0:w8"), Ok(("", Prim::Word8(0))));
        assert_eq!(parse_w8("0xB:w8"), Ok(("", Prim::Word8(11))));
        assert_eq!(parse_w8("0xb:w8"), Ok(("", Prim::Word8(0xb))));
    }

    #[test]
    fn test_parse_w32() {
        assert_eq!(parse_w32("0x0:w32"), Ok(("", Prim::Word32(0))));
        assert_eq!(parse_w32("0x10000:w32"), Ok(("", Prim::Word32(0x10000))));
    }

    #[test]
    fn parse_prims() {
        assert_eq!(parse_prim("0x0:w8"), Ok(("", Prim::Word8(0))));
        assert_eq!(parse_prim("0x0:w32"), Ok(("", Prim::Word32(0))));
    }
}
