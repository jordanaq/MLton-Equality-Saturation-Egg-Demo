use nom::{
    AsChar, IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::multispace0,
    sequence::{delimited, preceded, separated_pair, terminated},
};

use mlton_ssa::{parse::parse_sml_type, ssa::SmlType};

use parse_utils::*;

use crate::fpeg::{Constr, Param, Prim};

pub fn parse_constr(s: &str) -> IResult<&str, Constr> {
    let (rest, (type_s, constr_r)) = (delimited(
        tag("Constr<"),
        (parse_sml_type, preceded(tag("::"), word())),
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
        separated_pair(
            word(),
            (multispace0(), tag(":"), multispace0()),
            parse_sml_type,
        ),
        tag(">"),
    )
    .parse(s)?;
    Ok((rest, Param(param_r.to_owned(), t_s)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_constr() {
        let add = "Add".to_owned();
        let t = "T".to_owned();
        assert_eq!(
            parse_constr("Constr<T::Add>"),
            Ok((
                "",
                Constr {
                    constr_type: SmlType::Datatype(t),
                    name: add
                }
            ))
        );
    }

    #[test]
    fn test_parse_param() {
        let x = "x".to_owned();
        let t = "T".to_owned();
        assert_eq!(
            parse_param("Param<x : T>"),
            Ok(("", Param(x, SmlType::Datatype(t))))
        );
    }
}
