use nom::{
    AsChar, Parser,
    bytes::complete::{tag, take_while1},
    character::multispace0,
    error::ParseError,
    multi::separated_list0,
    sequence::{delimited, pair},
};

pub fn word<'a, E>() -> impl Parser<&'a str, Output = &'a str, Error = E>
where
    E: ParseError<&'a str>,
{
    take_while1(|c| AsChar::is_alphanum(c) || "-_".contains(c))
}

pub fn paren_list<'a, E>() -> impl Parser<&'a str, Output = Vec<&'a str>, Error = E>
where
    E: ParseError<&'a str>,
{
    delimited(
        tag("("),
        separated_list0(pair(tag(","), multispace0()), word()),
        tag(")"),
    )
}
