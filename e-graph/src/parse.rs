use u64;

use nom::{
    branch::alt, bytes::complete::{
        tag,
        take_while1,
    }, character::multispace0,
    combinator::recognize,
    error::ParseError,
    multi::{
        many1,
        separated_list0,
    }, sequence::{
        pair,
        preceded,
        terminated,
    }, AsChar, IResult, Parser
};

use crate::{fpeg::{
    Lit,
    Prim,
    Type,
}, Constr};


fn surrounded<'a, O1, O2, O3, E>(
    before: impl Parser<&'a str, Output=O1, Error=E>,
    inner: impl Parser<&'a str, Output=O2, Error=E>,
    after: impl Parser<&'a str, Output=O3, Error=E>
) -> impl Parser<&'a str, Output=O2, Error=E>
where
    E: nom::error::ParseError<&'a str>
{
    preceded(before, terminated(inner, after))
}

fn word<'a, E>() -> impl Parser<&'a str, Output=&'a str, Error=E>
where
    E: ParseError<&'a str>
{
    take_while1(|c| AsChar::is_alphanum(c) || "-_".contains(c))
}

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

fn parse_type(s: &str) -> IResult<&str, Type> {
    let (rest, s) = word().parse(s)?;

    Ok((rest, s.to_owned()))
}

fn paren_list<'a, E>() -> impl Parser<&'a str, Output=Vec<&'a str>, Error=E>
where
    E: ParseError<&'a str>
{
    surrounded(tag("("),
               separated_list0(pair(tag(","), multispace0()), word()),
               tag(")"))
}

pub fn parse_prim(s: &str) -> IResult<&str, Prim> {
    let (rest, (prim_s, xs)) =
        (surrounded(tag("Prim<"),
                    take_while1(AsChar::is_alpha),
                    tag(">")),
         paren_list()).parse(s)?;
    let xs_p = xs.into_iter().map(String::from).collect();
                                    
    Ok((rest, Prim(prim_s.to_string(), xs_p)))
}

pub fn parse_constr(s: &str) -> IResult<&str, Constr> {
    let (rest, (constr_s, xs, type_s,)) =
         (surrounded(tag("Constr<"), word(), tag(">")),
          paren_list(),
          preceded((multispace0(), tag(":"), multispace0()), parse_type)
         ).parse(s)?;
    let xs_p = xs.into_iter().map(String::from).collect();

    Ok((rest, Constr(constr_s.to_owned(), type_s, xs_p)))
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
    fn test_parse_lit() {
        assert_eq!(parse_lit("0x0:w8"), Ok(("", Lit::Word8(0))));
        assert_eq!(parse_lit("0x0:w32"), Ok(("", Lit::Word32(0))));
        assert_eq!(parse_w64("0x0:w64"), Ok(("", Lit::Word64(0))));
    }

    #[test]
    fn test_parse_prim() {
        let add = "Add".to_owned();
        let xs = vec!["a".to_owned(), "b".to_owned(),];
        assert_eq!(parse_prim("Prim<Add>(a, b)"), Ok(("", Prim(add, xs))));
    }


    #[test]
    fn test_parse_constr() {
        let add = "Add".to_owned();
        let xs = vec!["a".to_owned(), "b".to_owned(),];
        let t = "T".to_owned();
        assert_eq!(parse_constr("Constr<Add>(a, b) : T"), Ok(("", Constr(add, t, xs))));
    }
}
