use std::collections::HashMap;

use nom::{
    AsChar, IResult, Parser,
    bytes::{complete::tag, take_until, take_while},
    character::multispace0,
    error::ParseError,
    multi::separated_list0,
    sequence::{delimited, preceded, separated_pair},
};

use sml_utils::SmlType;

use parse_utils::word;

use crate::ssa::{ConstructorId, Datatype, Function, FunctionId, MltonSsa, Statement, VarId};

type Error<'a> = nom::error::Error<&'a str>;

fn surrounded_parser<'a, O, O1, O2, F, F1, F2>(
    left: F1,
    inner: F,
    right: F2,
) -> impl Parser<&'a str, Output = O, Error = Error<'a>>
where
    F: Parser<&'a str, Output = O, Error = Error<'a>>,
    F1: Parser<&'a str, Output = O1, Error = Error<'a>>,
    F2: Parser<&'a str, Output = O2, Error = Error<'a>>,
{
    delimited(
        preceded(multispace0(), left),
        inner,
        preceded(multispace0(), right),
    )
}

fn parse_key_value<'a, K: 'a, V: 'a, FK, FV>(
    key_parser: FK,
    value_parser: FV,
) -> impl Parser<&'a str, Output = (K, V), Error = Error<'a>>
where
    FK: Parser<&'a str, Output = K, Error = Error<'a>>,
    FV: Parser<&'a str, Output = V, Error = Error<'a>>,
{
    separated_pair(
        preceded(multispace0(), key_parser),
        preceded(multispace0(), tag(":")),
        preceded(multispace0(), value_parser),
    )
}

fn key_parser<'a, E>(key: &'a str) -> impl Parser<&'a str, Output = &'a str, Error = E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("\""), tag(key), tag("\""))
}

fn parse_string<'a>(s: &'a str) -> IResult<&'a str, &'a str> {
    delimited(tag("\""), word(), tag("\"")).parse(s)
}

fn parse_key_field<'a, V: 'a, FV>(
    key: &'a str,
    value_parser: FV,
) -> impl Parser<&'a str, Output = V, Error = Error<'a>>
where
    FV: Parser<&'a str, Output = V, Error = Error<'a>>,
{
    preceded(
        (
            preceded(multispace0(), key_parser(key)),
            preceded(multispace0(), tag(":")),
        ),
        preceded(multispace0(), value_parser),
    )
}

fn parse_sml_type(s: &str) -> IResult<&str, SmlType> {
    take_while(|c| AsChar::is_alphanum(c) || "-_ ".contains(c))
        .parse(s)
        .map(|(rest, s)| (rest, s.into()))
}

fn parse_cons(s: &str) -> IResult<&str, (ConstructorId, Vec<SmlType>)> {
    let (rest, (constr_name, arg_ts)) = surrounded_parser(
        tag("("),
        (
            parse_string,
            surrounded_parser(
                tag("(<"),
                separated_list0(tag(","), parse_sml_type), /* TODO */
                tag(">)"),
            ),
        ),
        tag(")"),
    )
    .parse(s)?;

    Ok((rest, (constr_name.to_string(), arg_ts)))
}

fn parse_datatype(s: &str) -> IResult<&str, Datatype> {
    let (rest, (tycon, constrs)) = surrounded_parser(
        tag("("),
        (
            parse_string,
            surrounded_parser(tag("["), parse_cons, tag("]")),
        ),
        tag(")"),
    )
    .parse(s)?;
    todo!();
    /*Ok((
        rest,
        Datatype {
            tycon: tycon.to_string(),
            constrs: vec![(tycon.into(), constrs)], // Placeholder
        },
    ))*/
}

fn parse_datatypes(s: &str) -> IResult<&str, HashMap<SmlType, Datatype>> {
    let (rest, dtypes) = separated_list0(tag(","), parse_datatype).parse(s)?;
    let dtype_map = dtypes
        .into_iter()
        .map(|dt| (dt.tycon.clone(), dt))
        .collect();
    Ok((rest, dtype_map))
}

fn parse_globals(s: &str) -> IResult<&str, HashMap<VarId, Statement>> {
    todo!()
}

fn parse_functions(s: &str) -> IResult<&str, HashMap<String, Function>> {
    todo!()
}

fn parse_main(s: &str) -> IResult<&str, FunctionId> {
    let (rest, main) = parse_string.parse(s)?;

    Ok((rest, main.to_string()))
}

fn parse_ssa(s: &str) -> IResult<&str, MltonSsa> {
    let (rest, (datatypes, globals, functions, main)) = surrounded_parser(
        tag("{"),
        (
            parse_key_field("datatypes", parse_datatypes),
            parse_key_field("globals", parse_globals),
            parse_key_field("functions", parse_functions),
            parse_key_field("main", parse_main),
        ),
        tag("}"),
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_main() {
        let s = r#""main""#;
        let (_rest, main) = parse_main(s).unwrap();
        assert_eq!(main, "main".to_string());
    }

    #[test]
    fn test_parse_string() {
        let s = r#""hello""#;
        let (_rest, res) = parse_string(s).unwrap();
        assert_eq!(res, "hello");
    }

    #[test]
    fn test_parse_key_value() {
        let s = r#""key": "x""#;
        let (_rest, (k, v)) = parse_key_value(key_parser("key"), parse_string)
            .parse(s)
            .unwrap();
        assert_eq!(k, "key");
        assert_eq!(v, "x");
    }

    #[test]
    fn test_parse_sml_type() {
        let s = "(<(word8) vector)>";
        let (_rest, t) = parse_sml_type(s).unwrap();
        assert_eq!(t, "w32".to_string());
    }

    #[test]
    fn test_parse_cons() {
        let s = r#"("Cons", (<w32, w32)>)"#;
        let (_rest, (constr_id, arg_ts)) = parse_cons(s).unwrap();
        assert_eq!(constr_id, "Cons".to_string());
        assert_eq!(arg_ts.len(), 2);
        assert_eq!(arg_ts[0], "w32".to_string());
        assert_eq!(arg_ts[1], "w32".to_string());
    }
}
