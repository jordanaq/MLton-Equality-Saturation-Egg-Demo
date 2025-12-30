use nom::{
    AsChar, IResult, Parser,
    branch::alt,
    bytes::{complete::tag, escaped_transform, is_not, take_until, take_while1},
    character::multispace0,
    combinator::{complete, map},
    error::ParseError,
    multi::separated_list0,
    sequence::{delimited, preceded},
};

pub type Error<'a> = nom::error::Error<&'a str>;

pub fn word<'a, E>() -> impl Parser<&'a str, Output = &'a str, Error = E>
where
    E: ParseError<&'a str>,
{
    take_while1(|c| AsChar::is_alphanum(c) || "-_".contains(c))
}

pub fn parse_true_false(s: &str) -> IResult<&str, bool> {
    alt((tag("true"), tag("false")))
        .map(|s| s == "true")
        .parse(s)
}

pub fn parse_string(input: &str) -> IResult<&str, String> {
    alt((
        tag(r#""""#).map(|_| "".to_string()),
        delimited(
            tag("\""),
            escaped_transform(
                is_not("\\\""),
                '\\',
                alt((
                    tag("\"").map(|_| "\""),
                    tag("n").map(|_| "\n"),
                    tag("t").map(|_| "\t"),
                    tag("\\").map(|_| "\\"),
                    tag("r").map(|_| "\r"),
                    tag("0").map(|_| "\0"),
                )),
            ),
            tag("\""),
        ),
    ))
    .parse(input)
}

pub fn parse_key_field<'a, V: 'a, FV>(
    key: &'a str,
    value_parser: FV,
) -> impl Parser<&'a str, Output = V, Error = Error<'a>>
where
    FV: Parser<&'a str, Output = V, Error = Error<'a>>,
{
    delimited(
        (
            preceded(multispace0(), tag(key)),
            delimited(multispace0(), tag("="), multispace0()),
        ),
        value_parser,
        alt((preceded(multispace0(), tag(",")), take_until("}"))),
    )
}

pub fn surrounded_parser<'a, O, O1, O2, F, F1, F2>(
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

pub fn paren_bracket_parser<'a, O, F>(
    inner: F,
) -> impl Parser<&'a str, Output = O, Error = Error<'a>>
where
    F: Parser<&'a str, Output = O, Error = Error<'a>>,
{
    surrounded_parser((tag("("), multispace0()), inner, (multispace0(), tag(")")))
}

pub fn tagify_parser<'a, O, F>(inner: F) -> impl Parser<&'a str, Output = O, Error = Error<'a>>
where
    F: Parser<&'a str, Output = O, Error = Error<'a>>,
{
    complete(delimited(multispace0(), inner, multispace0()))
}

pub fn named_object_parser<'a, O, F>(
    name: &'static str,
    inner: F,
) -> impl Parser<&'a str, Output = O, Error = Error<'a>>
where
    F: Parser<&'a str, Output = O, Error = Error<'a>>,
{
    surrounded_parser(
        (tag(name), multispace0(), tag("{")),
        tagify_parser(inner),
        tag("}"),
    )
}

pub fn paren_list_parser<'a, O, F>(
    item_parser: F,
) -> impl Parser<&'a str, Output = Vec<O>, Error = Error<'a>>
where
    F: Parser<&'a str, Output = O, Error = Error<'a>>,
{
    surrounded_parser(
        (tag("("), multispace0()),
        separated_list0((multispace0(), tag(","), multispace0()), item_parser),
        (multispace0(), tag(")")),
    )
}

pub fn named_tuple_parser<'a, O, F>(
    name: &'static str,
    inner: F,
) -> impl Parser<&'a str, Output = O, Error = Error<'a>>
where
    F: Parser<&'a str, Output = O, Error = Error<'a>>,
{
    surrounded_parser(
        (tag(name), multispace0(), tag("(")),
        tagify_parser(inner),
        (multispace0(), tag(")")),
    )
}

pub fn option_parser<'a, O, F>(
    inner: F,
) -> impl Parser<&'a str, Output = Option<O>, Error = Error<'a>>
where
    F: Parser<&'a str, Output = O, Error = Error<'a>>,
{
    alt((
        map(preceded((tag("Some"), multispace0()), inner), |v| Some(v)),
        map(tag("None"), |_| None),
    ))
}

#[cfg(test)]
mod tests {
    use nom::sequence::separated_pair;

    use super::*;

    #[test]
    fn test_tagify_parser() {
        let s = "  hello  ";
        let (_rest, res) = tagify_parser(tag("hello")).parse(s).unwrap();
        assert_eq!(res, "hello");

        let s = "  )  ";
        let (_rest, res) = tagify_parser(tag(")")).parse(s).unwrap();
        assert_eq!(res, ")");
    }

    #[test]
    fn test_parse_string() {
        let s = r#""""#;
        let (rest, res) = parse_string(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(res, "");

        let s = r#""hello""#;
        let (_rest, res) = parse_string(s).unwrap();
        assert_eq!(res, "hello");

        let s = r#""\n""#;
        let (_rest, res) = parse_string(s).unwrap();
        assert_eq!(res, "\n");

        let s = r#""\t""#;
        let (_rest, res) = parse_string(s).unwrap();
        assert_eq!(res, "\t");

        let s = r#""\\""#;
        let (_rest, res) = parse_string(s).unwrap();
        assert_eq!(res, "\\");

        let s = r#""\\""#;
        let (_rest, res) = parse_string(s).unwrap();
        assert_eq!(res, "\\");

        let s = r#""","#;
        let (rest, res) = parse_string(s).unwrap();
        assert_eq!(rest, ",");
        assert_eq!(res, "");
    }

    #[test]
    fn test_parse_key_field() {
        let s = r#"key = "x","#;
        let (_rest, v) = parse_key_field("key", parse_string).parse(s).unwrap();
        assert_eq!(v, "x");

        let s = r#"key
            =
            "x"
            ,"#;
        let (_rest, v) = parse_key_field("key", parse_string).parse(s).unwrap();
        assert_eq!(v, "x");
    }

    #[test]
    fn test_paren_list_parser() {
        let s = "(x, y, z)";
        let (_rest, res) = paren_list_parser(word()).parse(s).unwrap();
        assert_eq!(res, vec!["x", "y", "z"]);

        let s = "( x , y , z )";
        let (_rest, res) = paren_list_parser(word()).parse(s).unwrap();
        assert_eq!(res, vec!["x", "y", "z"]);

        let s = "(x)";
        let (_rest, res) = paren_list_parser(word()).parse(s).unwrap();
        assert_eq!(res, vec!["x"]);

        let s = "()";
        let (_rest, res) = paren_list_parser(word()).parse(s).unwrap();
        assert_eq!(res, Vec::<&str>::new());
    }

    #[test]
    fn test_option_parser() {
        let s = "None";
        let (rest, v) = option_parser(word()).parse(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(v, None);

        let s = "Some (x, y, z)";
        let (rest, v) = option_parser(paren_list_parser(word())).parse(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(v, Some(vec!["x", "y", "z"]));
    }

    #[test]
    fn test_paren_bracket_parser() {
        let s = " (  x , y ) ";
        let (rest, res) =
            paren_bracket_parser(separated_pair(word(), tagify_parser(tag(",")), word()))
                .parse(s)
                .unwrap();
        assert_eq!(rest, " ");
        assert_eq!(res, ("x", "y"));
    }

    #[test]
    fn test_named_object_parser() {
        let s = r#"
            Object {
                field1 = "value1"  ,
                field2 = 42
            }
        "#;
        let (_rest, (f1, f2)) = named_object_parser(
            "Object",
            (
                parse_key_field("field1", parse_string),
                parse_key_field("field2", word()),
            ),
        )
        .parse(s)
        .unwrap();
        assert_eq!(f1, "value1");
        assert_eq!(f2, "42");
    }
}
