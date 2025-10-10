use std::str::FromStr;

use e_graph::{
    fpeg::Region,
    parse::{parse_region, parse_regions},
};
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::{
        complete::{tag, take_until},
        take_while, take_while1,
    },
    character::complete::{multispace0, multispace1},
    combinator::opt,
    multi,
    sequence::{delimited, preceded},
};

use mlton_ssa::{
    parse::{
        self, parse_cases_con, parse_cases_word, parse_datatypes, parse_sml_type, parse_sml_types,
        parse_transfer_call_non_tail_handler, parse_var_name, parse_wordsize,
    },
    ssa::{Cases, Handler as MltHandler, Return, SmlType, VarId},
};
use parse_utils::{named_object_parser, option_parser, paren_list_parser, parse_key_field};

use crate::{Block, Function, SkTransfer};

fn parse_sk_transfer_bug(s: &str) -> IResult<&str, SkTransfer> {
    named_object_parser("skTransfer::Bug", multispace0)
        .map(|_| SkTransfer::Bug)
        .parse(s)
}

fn parse_sk_transfer_call_dead(s: &str) -> IResult<&str, SkTransfer> {
    named_object_parser(
        "skTransfer::call::Dead",
        (
            parse_key_field("func", parse_var_name),
            parse_key_field("args", parse_regions),
        ),
    )
    .map(|(func, args)| SkTransfer::Call {
        func,
        args,
        ret: Return::Dead,
    })
    .parse(s)
}

fn parse_sk_transfer_call_non_tail(s: &str) -> IResult<&str, SkTransfer> {
    named_object_parser(
        "skTransfer::call::NonTail",
        (
            parse_key_field("func", parse_var_name),
            parse_key_field("args", parse_regions),
            parse_key_field("cont_id", parse_var_name),
            parse_key_field("handler", parse_transfer_call_non_tail_handler),
        ),
    )
    .map(|(func, args, cont_id, handler)| SkTransfer::Call {
        func,
        args,
        ret: Return::NonTail {
            cont: cont_id,
            handler,
        },
    })
    .parse(s)
}

fn parse_sk_transfer_call_tail(s: &str) -> IResult<&str, SkTransfer> {
    named_object_parser(
        "skTransfer::call::Tail",
        (
            parse_key_field("func", parse_var_name),
            parse_key_field("args", parse_regions),
        ),
    )
    .map(|(func, args)| SkTransfer::Call {
        func,
        args,
        ret: Return::Tail,
    })
    .parse(s)
}

fn parse_sk_transfer_call(s: &str) -> IResult<&str, SkTransfer> {
    alt((
        parse_sk_transfer_call_dead,
        parse_sk_transfer_call_non_tail,
        parse_sk_transfer_call_tail,
    ))
    .parse(s)
}

fn parse_sk_transfer_case_con(s: &str) -> IResult<&str, SkTransfer> {
    named_object_parser(
        "skTransfer::case::Con",
        (
            parse_key_field("test", parse_region),
            parse_key_field("cases", parse_cases_con),
            opt(parse_key_field("default", parse_var_name).map(|l| l.to_string())),
        ),
    )
    .map(|(test, cases, default)| SkTransfer::Case {
        test,
        cases,
        default,
    })
    .parse(s)
}

fn parse_sk_transfer_case_word(s: &str) -> IResult<&str, SkTransfer> {
    named_object_parser(
        "skTransfer::case::Word",
        (
            parse_key_field("test", parse_region),
            parse_key_field("size", parse_wordsize),
            parse_key_field("cases", parse_cases_word),
            opt(parse_key_field("default", parse_var_name).map(|l| l.to_string())),
        ),
    )
    .map(|(test, ws, cases, default)| {
        let Cases::Word(ws_cases, _) = &cases else {
            unreachable!()
        };

        assert_eq!(ws, *ws_cases);

        SkTransfer::Case {
            test,
            cases,
            default,
        }
    })
    .parse(s)
}

fn parse_sk_transfer_case(s: &str) -> IResult<&str, SkTransfer> {
    alt((parse_sk_transfer_case_con, parse_sk_transfer_case_word)).parse(s)
}

fn parse_sk_transfer_goto(s: &str) -> IResult<&str, SkTransfer> {
    named_object_parser(
        "skTransfer::goto",
        (
            parse_key_field("dst", parse_var_name),
            parse_key_field("args", parse_regions),
        ),
    )
    .map(|(dst, args)| SkTransfer::Goto { dst, args })
    .parse(s)
}

fn parse_sk_transfer_raise(s: &str) -> IResult<&str, SkTransfer> {
    named_object_parser("skTransfer::raise", parse_key_field("args", parse_regions))
        .map(|args| SkTransfer::Raise { args })
        .parse(s)
}

fn parse_sk_transfer_return(s: &str) -> IResult<&str, SkTransfer> {
    named_object_parser("skTransfer::return", parse_key_field("args", parse_regions))
        .map(|args| SkTransfer::Return { args })
        .parse(s)
}

fn parse_sk_transfer_runtime(s: &str) -> IResult<&str, SkTransfer> {
    todo!()
}

fn parse_sk_transfer(s: &str) -> IResult<&str, SkTransfer> {
    alt((
        parse_sk_transfer_bug,
        parse_sk_transfer_call,
        parse_sk_transfer_case,
        parse_sk_transfer_goto,
        parse_sk_transfer_raise,
        parse_sk_transfer_return,
        // parse_sk_transfer_runtime,
    ))
    .parse(s)
}

impl FromStr for SkTransfer {
    type Err = nom::Err<nom::error::Error<String>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (rest, transfer) = parse_sk_transfer(s).map_err(|e| e.map_input(|i| i.to_string()))?;
        if !rest.trim().is_empty() {
            Err(nom::Err::Error(nom::error::Error::new(
                rest.to_string(),
                nom::error::ErrorKind::NonEmpty,
            )))
        } else {
            Ok(transfer)
        }
    }
}

fn parse_sk_block(s: &str) -> IResult<&str, Block> {
    named_object_parser(
        "skBlock",
        (
            parse_key_field("label", parse_var_name),
            parse_key_field("args", parse_regions),
            parse_key_field("transfer", parse_sk_transfer),
        ),
    )
    .map(|(label, args, transfer)| Block {
        label,
        args,
        transfer,
    })
    .parse(s)
}

impl FromStr for Block {
    type Err = nom::Err<nom::error::Error<String>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (rest, block) = parse_sk_block(s).map_err(|e| e.map_input(|i| i.to_string()))?;
        if !rest.trim().is_empty() {
            Err(nom::Err::Error(nom::error::Error::new(
                rest.to_string(),
                nom::error::ErrorKind::NonEmpty,
            )))
        } else {
            Ok(block)
        }
    }
}

fn parse_sk_function(s: &str) -> IResult<&str, Function> {
    named_object_parser(
        "skFunction",
        (
            parse_key_field("args", parse_regions),
            parse_key_field("blocks", paren_list_parser(parse_sk_block)),
            parse_key_field(
                "may_inline",
                alt((tag("true").map(|_| true), tag("false").map(|_| false))),
            ),
            parse_key_field("name", parse_var_name),
            parse_key_field("raises", option_parser(parse_sml_types)),
            parse_key_field("returns", option_parser(parse_sml_types)),
            parse_key_field("start", parse_var_name),
        ),
    )
    .map(
        |(args, blocks, may_inline, name, raises, returns, start)| Function {
            args,
            blocks,
            may_inline,
            name,
            raises,
            returns,
            start,
        },
    )
    .parse(s)
}

impl FromStr for Function {
    type Err = nom::Err<nom::error::Error<String>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (rest, function) = parse_sk_function(s).map_err(|e| e.map_input(|i| i.to_string()))?;
        if !rest.trim().is_empty() {
            Err(nom::Err::Error(nom::error::Error::new(
                rest.to_string(),
                nom::error::ErrorKind::NonEmpty,
            )))
        } else {
            Ok(function)
        }
    }
}

pub fn parse_skeleton(s: &str) -> IResult<&str, crate::Skeleton> {
    named_object_parser(
        "skeleton",
        (
            parse_key_field("datatypes", parse_datatypes),
            parse_key_field("globals", parse_regions), // TODO: stateful globals
            parse_key_field("functions", paren_list_parser(parse_sk_function)),
            parse_key_field("main", parse_var_name),
            parse_key_field("fpeg", e_graph::parse::parse_fpeg),
        ),
    )
    .map(
        |(datatypes, globals, functions, main, graph)| crate::Skeleton {
            datatypes,
            globals,
            functions,
            graph,
            main,
        },
    )
    .parse(s)
}

impl FromStr for crate::Skeleton {
    type Err = nom::Err<nom::error::Error<String>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (rest, skeleton) = parse_skeleton(s).map_err(|e| e.map_input(|i| i.to_string()))?;
        if !rest.trim().is_empty() {
            Err(nom::Err::Error(nom::error::Error::new(
                rest.to_string(),
                nom::error::ErrorKind::NonEmpty,
            )))
        } else {
            Ok(skeleton)
        }
    }
}

#[cfg(test)]
mod test {
    use mlton_ssa::ssa::{Cases, WordSize};

    use super::*;

    #[test]
    fn test_parse_region() {
        assert_eq!(parse_region("Region<42>"), Ok(("", 42.into())));
        assert_eq!(parse_region("Region<0>"), Ok(("", 0.into())));
        assert!(parse_region("Region<>").is_err());
        assert!(parse_region("Region<abc>").is_err());
    }

    #[test]
    fn test_parse_regions() {
        assert_eq!(
            parse_regions("(Region<1>, Region<2>, Region<3>)"),
            Ok(("", vec![1.into(), 2.into(), 3.into()]))
        );
        assert_eq!(parse_regions("()"), Ok(("", vec![])));
        assert!(parse_regions("(Region<1>, Region<abc>)").is_err());
    }

    #[test]
    fn test_parse_sk_transfer_bug() {
        assert_eq!(
            parse_sk_transfer_bug("skTransfer::Bug {}"),
            Ok(("", SkTransfer::Bug))
        );
    }

    #[test]
    fn test_parse_sk_transfer_call_dead() {
        assert_eq!(
            parse_sk_transfer_call_dead(
                "skTransfer::call::Dead { func = f, args = (Region<1>, Region<2>) }"
            ),
            Ok((
                "",
                SkTransfer::Call {
                    func: "f".into(),
                    args: vec![1.into(), 2.into()],
                    ret: Return::Dead,
                }
            ))
        );
    }

    #[test]
    fn test_parse_sk_transfer_call_non_tail() {
        let s = r#"skTransfer::call::NonTail { func = f, args = (Region<1>, Region<2>), cont_id = L, handler = handler::Handle { label = h } }"#;
        let (rest, transfer) = parse_sk_transfer_call_non_tail(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            transfer,
            SkTransfer::Call {
                func: "f".into(),
                args: vec![1.into(), 2.into()],
                ret: Return::NonTail {
                    cont: "L".into(),
                    handler: MltHandler::Handle { label: "h".into() }
                },
            }
        );
    }

    #[test]
    fn test_parse_sk_transfer_call_tail() {
        let s = r#"skTransfer::call::Tail { func = f, args = (Region<1>, Region<2>) }"#;
        let (rest, transfer) = parse_sk_transfer_call_tail(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            transfer,
            SkTransfer::Call {
                func: "f".into(),
                args: vec![1.into(), 2.into()],
                ret: Return::Tail,
            }
        );
    }

    #[test]
    fn test_parse_sk_transfer_call() {
        let s = r#"skTransfer::call::Dead { func = f, args = (Region<1>, Region<2>) }"#;
        let (rest, transfer) = parse_sk_transfer_call(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            transfer,
            SkTransfer::Call {
                func: "f".into(),
                args: vec![1.into(), 2.into()],
                ret: Return::Dead,
            }
        );
    }

    #[test]
    fn test_parse_sk_transfer_case_con() {
        let s = r#"skTransfer::case::Con {
            test = Region<1>,
            cases = 
                (nil_0 => L1,
                 ::_2 => L2),
            default = L3
        }"#;
        let (rest, transfer) = parse_sk_transfer_case_con(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            transfer,
            SkTransfer::Case {
                test: 1.into(),
                cases: Cases::Con(vec![
                    ("nil_0".into(), "L1".into()),
                    ("::_2".into(), "L2".into()),
                ]),
                default: Some("L3".into()),
            }
        );
    }

    #[test]
    fn test_parse_sk_transfer_case_word() {
        let s = r#"skTransfer::case::Word {
            test = Region<1>,
            size = w64,
            cases = 
                (0x0:w64 => L1,
                 0x1:w64 => L2),
            default = L3
        }"#;
        let (rest, transfer) = parse_sk_transfer_case_word(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            transfer,
            SkTransfer::Case {
                test: 1.into(),
                cases: Cases::Word(WordSize::W64, vec![(0, "L1".into()), (1, "L2".into()),]),
                default: Some("L3".into()),
            }
        );
    }

    #[test]
    fn test_parse_sk_transfer_case() {
        let s = r#"skTransfer::case::Con {
            test = Region<1>,
            cases = 
                (nil_0 => L1,
                 ::_2 => L2),
            default = L3
        }"#;
        let (rest, transfer) = parse_sk_transfer_case(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            transfer,
            SkTransfer::Case {
                test: 1.into(),
                cases: Cases::Con(vec![
                    ("nil_0".into(), "L1".into()),
                    ("::_2".into(), "L2".into()),
                ]),
                default: Some("L3".into()),
            }
        );
    }

    #[test]
    fn test_parse_sk_transfer_goto() {
        let s = r#"skTransfer::goto { dst = L, args = (Region<1>, Region<2>) }"#;
        let (rest, transfer) = parse_sk_transfer_goto(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            transfer,
            SkTransfer::Goto {
                dst: "L".into(),
                args: vec![1.into(), 2.into()],
            }
        );
    }

    #[test]
    fn test_parse_sk_transfer_raise() {
        let s = r#"skTransfer::raise { args = (Region<1>, Region<2>) }"#;
        let (rest, transfer) = parse_sk_transfer_raise(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            transfer,
            SkTransfer::Raise {
                args: vec![1.into(), 2.into()],
            }
        );
    }

    #[test]
    fn test_parse_sk_transfer_return() {
        let s = r#"skTransfer::return { args = (Region<1>, Region<2>) }"#;
        let (rest, transfer) = parse_sk_transfer_return(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            transfer,
            SkTransfer::Return {
                args: vec![1.into(), 2.into()],
            }
        );
    }

    #[test]
    fn test_parse_sk_transfer() {
        let s = r#"skTransfer::call::Dead { func = f, args = (Region<1>, Region<2>) }"#;
        let (rest, transfer) = parse_sk_transfer(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            transfer,
            SkTransfer::Call {
                func: "f".into(),
                args: vec![1.into(), 2.into()],
                ret: Return::Dead,
            }
        );

        let s = r#"skTransfer::Bug {}"#;
        let (rest, transfer) = parse_sk_transfer(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(transfer, SkTransfer::Bug);
    }

    #[test]
    fn test_sk_transfer_from_str() {
        let s = r#"skTransfer::call::Dead { func = f, args = (Region<1>, Region<2>) }"#;
        let transfer: SkTransfer = s.parse().unwrap();
        assert_eq!(
            transfer,
            SkTransfer::Call {
                func: "f".into(),
                args: vec![1.into(), 2.into()],
                ret: Return::Dead,
            }
        );

        let s = r#"skTransfer::Bug {}  "#;
        let transfer: SkTransfer = s.parse().unwrap();
        assert_eq!(transfer, SkTransfer::Bug);
    }

    #[test]
    fn test_parse_sk_block() {
        let s = r#"skBlock {
            label = L,
            args = (Region<1>, Region<2>),
            transfer = skTransfer::Bug {}
        }"#;
        let (rest, block) = parse_sk_block(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            block,
            crate::Block {
                label: "L".into(),
                args: vec![1.into(), 2.into()],
                transfer: SkTransfer::Bug,
            }
        );
    }

    #[test]
    fn test_sk_block_from_str() {
        let s = r#"skBlock {
            label = L,
            args = (Region<1>, Region<2>),
            transfer = skTransfer::Bug {}
        }"#;
        let block: Block = s.parse().unwrap();
        assert_eq!(
            block,
            crate::Block {
                label: "L".into(),
                args: vec![1.into(), 2.into()],
                transfer: SkTransfer::Bug,
            }
        );
    }

    #[test]
    fn test_parse_sk_function() {
        let s = r#"skFunction {
            args = (Region<1>, Region<2>),
            blocks = (skBlock {
                label = L1,
                args = (Region<1>),
                transfer = skTransfer::Bug {}
            }, skBlock {
                label = L2,
                args = (Region<2>),
                transfer = skTransfer::Bug {}
            }),
            may_inline = true,
            name = f,
            raises = Some ((< Word(w64) >), (< Datatype("bool") >)),
            returns = Some ((< Word(w64) >)),
            start = L1
        }"#;
        let (rest, function) = parse_sk_function(s).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            function,
            Function {
                args: vec![1.into(), 2.into()],
                blocks: vec![
                    crate::Block {
                        label: "L1".into(),
                        args: vec![1.into()],
                        transfer: SkTransfer::Bug,
                    },
                    crate::Block {
                        label: "L2".into(),
                        args: vec![2.into()],
                        transfer: SkTransfer::Bug,
                    },
                ],
                may_inline: true,
                name: "f".into(),
                raises: Some(vec![
                    SmlType::Word(WordSize::W64),
                    SmlType::Datatype("bool".into())
                ]),
                returns: Some(vec![SmlType::Word(WordSize::W64)]),
                start: "L1".into(),
            }
        );
    }

    #[test]
    fn test_sk_function_from_str() {
        let s = r#"skFunction {
            args = (Region<1>, Region<2>),
            blocks = (skBlock {
                label = L1,
                args = (Region<1>),
                transfer = skTransfer::Bug {}
            }, skBlock {
                label = L2,
                args = (Region<2>),
                transfer = skTransfer::Bug {}
            }),
            may_inline = true,
            name = f,
            raises = Some ((< Word(w64) >), (< Datatype("bool") >)),
            returns = Some ((< Word(w64) >)),
            start = L1
        }"#;
        let function: Function = s.parse().unwrap();
        assert_eq!(
            function,
            Function {
                args: vec![1.into(), 2.into()],
                blocks: vec![
                    crate::Block {
                        label: "L1".into(),
                        args: vec![1.into()],
                        transfer: SkTransfer::Bug,
                    },
                    crate::Block {
                        label: "L2".into(),
                        args: vec![2.into()],
                        transfer: SkTransfer::Bug,
                    },
                ],
                may_inline: true,
                name: "f".into(),
                raises: Some(vec![
                    SmlType::Word(WordSize::W64),
                    SmlType::Datatype("bool".into())
                ]),
                returns: Some(vec![SmlType::Word(WordSize::W64)]),
                start: "L1".into(),
            }
        );
    }

    #[test]
    fn test_parse_skeleton() {
        todo!()
    }

    #[test]
    fn test_skeleton_from_str() {
        todo!()
    }
}
