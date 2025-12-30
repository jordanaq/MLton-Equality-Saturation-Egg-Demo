use std::collections::HashMap;

use e_graph::fpeg::Region;
use mlton_ssa::ssa::{
    Block as MltBlock, Exp as MltExp, Function as MltFunction, MltonSsa as MltSsa, SmlType,
    Statement as MltStatement, Transfer as MltTransfer, VarId,
};

use crate::{
    SkTransfer,
    skeleton::{Block as SkBlock, Function as SkFunction, Skeleton},
};

fn sk_block_from_mltblock(
    MltBlock {
        args,
        label,
        statements,
        transfer,
    }: &MltBlock,
    sk: &mut Skeleton,
    scope: &HashMap<VarId, SmlType>,
) -> SkBlock {
    let arg_rs: Vec<Region> = args.iter().map(|var| sk.insert_arg(var)).collect();

    let state_r = sk.insert_state_arg(label.as_str());
    let mut state_prev = None;

    for stmt @ MltStatement { ty, exp, .. } in statements {
        if !stmt.is_pure() {
            let state = state_prev.unwrap_or(state_r);
            state_prev = Some(
                sk.insert_stateful_exp(scope, &exp, &state, Some(ty))
                    .unwrap_or_else(|| {
                        panic!(
                            "Failed to convert stateful expression `{:?}` in block {}",
                            exp, label
                        )
                    }),
            );
        } else {
            sk.insert_exp(&scope, exp, Some(ty)).unwrap_or_else(|| {
                panic!(
                    "Failed to convert pure expression `{:?}` in block {}",
                    exp, label
                )
            });
        }
    }

    let sk_transfer = match transfer {
        MltTransfer::Bug => SkTransfer::Bug,
        MltTransfer::Call { func, args, ret } => SkTransfer::Call {
            func: func.clone(),
            state: state_prev.unwrap_or(state_r),
            args: args
                .iter()
                .map(|v| {
                    let ty = scope
                        .get(v)
                        .unwrap_or_else(|| panic!("Variable {} not found in scope {:?}", v, scope));
                    sk.insert_exp(&scope, &MltExp::Var(v.clone()), Some(ty))
                        .unwrap()
                })
                .collect::<Vec<Region>>(),
            ret: ret.clone(),
        },
        MltTransfer::Case {
            test,
            cases,
            default,
        } => {
            let ty = scope
                .get(test)
                .unwrap_or_else(|| panic!("Variable {} not found in scope {:?}", test, scope));
            SkTransfer::Case {
                test: sk
                    .insert_exp(&scope, &MltExp::Var(test.clone()), Some(ty))
                    .unwrap(),
                state: state_prev.unwrap_or(state_r),
                cases: cases.clone(),
                default: default.clone(),
            }
        }
        MltTransfer::Goto { dst, args } => SkTransfer::Goto {
            dst: dst.clone(),
            state: state_prev.unwrap_or(state_r),
            args: args
                .iter()
                .map(|v| {
                    let ty = scope
                        .get(v)
                        .unwrap_or_else(|| panic!("Variable {} not found in scope {:?}", v, scope));
                    sk.insert_exp(&scope, &MltExp::Var(v.clone()), Some(ty))
                        .unwrap()
                })
                .collect::<Vec<Region>>(),
        },
        MltTransfer::Raise { args } => SkTransfer::Raise {
            state: state_prev.unwrap_or(state_r),
            args: args
                .iter()
                .map(|v| {
                    let ty = scope
                        .get(v)
                        .unwrap_or_else(|| panic!("Variable {} not found in scope {:?}", v, scope));
                    sk.insert_exp(&scope, &MltExp::Var(v.clone()), Some(ty))
                        .unwrap()
                })
                .collect::<Vec<Region>>(),
        },
        MltTransfer::Return { args } => SkTransfer::Return {
            state: state_prev.unwrap_or(state_r),
            args: args
                .iter()
                .map(|v| {
                    let ty = scope
                        .get(v)
                        .unwrap_or_else(|| panic!("Variable {} not found in scope {:?}", v, scope));
                    sk.insert_exp(&scope, &MltExp::Var(v.clone()), Some(ty))
                        .unwrap()
                })
                .collect::<Vec<Region>>(),
        },
        MltTransfer::Runtime => SkTransfer::Runtime,
    };

    SkBlock {
        label: label.clone(),
        state: state_r,
        args: arg_rs,
        transfer: sk_transfer,
    }
}

fn sk_function_from_mltfunction(
    mlt_func: &MltFunction,
    sk: &mut Skeleton,
    global_scope: &HashMap<VarId, SmlType>,
) -> SkFunction {
    // State going into the function, unused in e-graph as it is the same as the state going in to the first block
    let state = sk.insert_state_arg(mlt_func.name.as_str());

    let arg_rs: Vec<Region> = mlt_func.args.iter().map(|var| sk.insert_arg(var)).collect();

    let scopes = mlt_func.get_scopes();

    let blocks: Vec<SkBlock> = mlt_func
        .blocks
        .iter()
        .map(|b| {
            sk_block_from_mltblock(
                b,
                sk,
                &global_scope
                    .iter()
                    .chain(scopes.get(&b.label).unwrap())
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<HashMap<VarId, SmlType>>(),
            )
        })
        .collect();

    SkFunction {
        state,
        args: arg_rs,
        blocks,
        may_inline: mlt_func.may_inline,
        name: mlt_func.name.clone(),
        raises: mlt_func.raises.clone(),
        returns: mlt_func.returns.clone(),
        start: mlt_func.start.clone(),
    }
}

pub fn sk_from_mltonssa(mlton: &MltSsa) -> Skeleton {
    let mut sk = Skeleton::default();

    sk.datatypes = mlton.datatypes.clone();

    let mut globals: Vec<Region> = vec![];

    let mut scope = HashMap::<VarId, SmlType>::new();

    // Initial state is the same as the state arg to the main function
    // let mut state_r: Option<Region> = None;

    for MltStatement { var, ty, exp } in mlton.globals.clone() {
        let exp_r = sk.insert_exp(&scope, &exp, Some(&ty)).unwrap_or_else(|| {
            panic!(
                "Failed to convert exp `{:?}` in statement for `{:?}`",
                exp, var
            )
        });

        // TODO: Handle stateful operations
        let var = var.unwrap();
        scope.insert(var.clone(), ty.clone());
        globals.push(exp_r);
    }

    sk.globals = globals;

    sk.functions = mlton
        .functions
        .iter()
        .map(|f| sk_function_from_mltfunction(f, &mut sk, &scope))
        .collect();

    sk.main = mlton.main.clone();

    sk
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use mlton_ssa::ssa::{Datatype as MltDatatype, WordSize as MltWordSize};

    use super::*;

    #[test]
    fn test_sk_block_from_mltblock() {
        let s = r#"
        block {label = L_57,
               args = (),
               statements = (statement {var = None,
                                        type = (< Tuple () >),
                                        exp = exp::PrimApp {prim = prim::CFunction {func = CFunction {args = ((< Vector( Word( w8 ) ) >)),
                                                                                                      convention = cdecl,
                                                                                                      inline = false,
                                                                                                      kind = Impure,
                                                                                                      prototype = prototype {args = ((< Objptr >)),
                                                                                                                             res = None},
                                                                                                      return = (< Tuple () >),
                                                                                                      symbolScope = private,
                                                                                                      target = target {type = Direct,
                                                                                                                       name = "Stdio_print"}}},
                                                            args = (global_37)}},
                             statement {var = None,
                                        type = (< Tuple () >),
                                        exp = exp::PrimApp {prim = prim::MLtonHalt {},
                                                            args = (global_5)}},
                             statement {var = None,
                                        type = (< Tuple () >),
                                        exp = exp::PrimApp {prim = prim::MLtonBug {},
                                                            args = (global_28)}}),
               transfer = transfer::Bug {}}
        "#;
        let mlt_block: MltBlock = FromStr::from_str(s.trim()).unwrap();

        let mut sk = Skeleton::default();
        let scope = HashMap::<VarId, SmlType>::from([
            ("global_5".into(), SmlType::Word(MltWordSize::W64)),
            ("global_28".into(), SmlType::Datatype("list_0".into())),
            ("global_37".into(), SmlType::Datatype("list_0".into())),
        ]);
        let sk_block = sk_block_from_mltblock(&mlt_block, &mut sk, &scope);
        assert_eq!(sk_block.label, "L_57");
        assert_eq!(sk_block.args.len(), 0);
        assert_eq!(sk_block.transfer, SkTransfer::Bug);
    }

    #[test]
    fn test_sk_function_from_mltfunction() {
        let s = r#"
        function {name = "main_0",
                  mayInline = false,
                  args = (),
                  start = L_52,
                  returns = None,
                  raises = None,
                  blocks = (block {label = L_52,
                                   args = (),
                                   statements = (),
                                   transfer = transfer::Goto {dst = loop_5,
                                                              args = (global_41,
                                                                      global_8,
                                                                      global_9)}},
                            block {label = loop_5,
                                   args = (x_78:
                                             (< Datatype( "list_3" ) >),
                                           x_81:
                                             (< Word( w64 ) >),
                                           x_120:
                                             (< Word( w64 ) >)),
                                   statements = (statement {var = Some x_121,
                                                            type = (< Datatype( "bool" ) >),
                                                            exp = exp::PrimApp {prim = prim::WordEqual {size = w64},
                                                                                args = (x_120,
                                                                                        global_6)}}),
                                   transfer = transfer::case::Con {test = x_121,
                                                                   cases =   (true => L_80,
                                                                              false => L_55)}})}
       "#;
        let mlt_function: MltFunction = FromStr::from_str(s.trim()).unwrap();
        let mut sk = Skeleton::default();
        let scope = HashMap::<VarId, SmlType>::from([
            ("global_6".into(), SmlType::Word(MltWordSize::W64)),
            ("global_8".into(), SmlType::Word(MltWordSize::W64)),
            ("global_9".into(), SmlType::Word(MltWordSize::W64)),
            ("global_41".into(), SmlType::Datatype("list_3".into())),
        ]);
        sk.datatypes = vec![
            MltDatatype {
                tycon: "list_3".into(),
                constrs: vec![
                    (
                        "cons_3".into(),
                        vec![
                            SmlType::Word(MltWordSize::W64),
                            SmlType::Datatype("list_3".into()),
                        ],
                    ),
                    ("nil_3".into(), vec![]),
                ],
            },
            MltDatatype {
                tycon: "bool".into(),
                constrs: vec![("true".into(), vec![]), ("false".into(), vec![])],
            },
        ];
        let sk_function = sk_function_from_mltfunction(&mlt_function, &mut sk, &scope);
        assert_eq!(sk_function.name, "main_0");
        assert_eq!(sk_function.args.len(), 0);
        assert_eq!(sk_function.start, "L_52");
        assert_eq!(sk_function.blocks.len(), 2);
        assert_eq!(sk_function.blocks[0].label, "L_52");
        assert_eq!(sk_function.blocks[0].args.len(), 0);
        assert_eq!(
            sk_function.blocks[0].transfer,
            SkTransfer::Goto {
                dst: "loop_5".to_string(),
                state: sk.insert_state_arg("L_52".into()),
                args: vec![
                    sk.insert_exp(
                        &scope,
                        &MltExp::Var("global_41".into()),
                        Some(&SmlType::Datatype("list_3".into()))
                    )
                    .unwrap(),
                    sk.insert_exp(
                        &scope,
                        &MltExp::Var("global_8".into()),
                        Some(&SmlType::Word(MltWordSize::W64))
                    )
                    .unwrap(),
                    sk.insert_exp(
                        &scope,
                        &MltExp::Var("global_9".into()),
                        Some(&SmlType::Word(MltWordSize::W64))
                    )
                    .unwrap(),
                ]
            }
        );
        println!("sk_function: {:#?}", sk_function);
        println!("e_graph: {:#?}", sk.graph);
    }

    #[test]
    fn test_sk_from_mltonssa() {
        let s = r#"mltonssa {
        datatypes = (
            datatype {tycon = "bool", cons = (( true ), ( false ))},
            datatype {tycon = "list_4", cons = (( dummy_0 ))},
            datatype {tycon = "list_3",
                      cons = (( nil_0 ),
                              ( ::_2, ((< Datatype( "list_3" ) >)) ))}
        ),
        globals = (
            statement {var = Some global_0,
                       type = (< Vector( Word( w8 ) ) >),
                       exp = exp::Const {const = const::WordVector {const = "unhandled exception: "}}},
            statement {var = Some global_1,
                       type = (< Vector( Word( w8 ) ) >),
                       exp = exp::Const {const = const::WordVector {const = "Overflow"}}},
            statement {var = Some global_7,
                       type = (< Datatype( "list_3" ) >),
                       exp = exp::ConApp {con = "nil_0", args = ()}},
            statement {var = Some global_8,
                       type = (< Word( w64 ) >),
                       exp = exp::Const {const = const::Word {const = 0x1:w64}}},
            statement {var = Some global_9,
                       type = (< Word( w64 ) >),
                       exp = exp::Const {const = const::Word {const = 0x4000000000000000:w64}}},
            statement {var = Some global_40,
                       type = (< Datatype( "list_3" ) >),
                       exp = exp::Var {var = global_7}},
            statement {var = Some global_41,
                       type = (< Datatype( "list_3" ) >),
                       exp = exp::ConApp {con = "::_2",
                       args = (global_40)}}
        ),
        functions = (
            function {name = "main_0",
                      mayInline = false,
                      args = (),
                      start = L_52,
                      returns = None,
                      raises = None,
                      blocks = (block {label = L_52,
                                       args = (),
                                       statements = (),
                                       transfer = transfer::Goto {dst = loop_5,
                                                                  args = (global_41,
                                                                          global_8,
                                                                          global_9)}})},
            function {name = "exit_0",
                      mayInline = true,
                      args = (x_3: (< Word( w32 ) >),
                              x_2: (< Array( Word( w8 ) ) >),
                              x_1: (< Ref( Datatype( "bool" ) ) >),
                              x_0: (< Ref( Datatype( "bool" ) ) >)),
                      start = L_0,
                      returns = None,
                      raises = Some (),
                      blocks = (block {label = L_0,
                                       args = (),
                                       statements = (statement {var = Some x_76,
                                                                type = (< Datatype( "bool" ) >),
                                                                exp = exp::PrimApp {prim = prim::RefDeref {},
                                                                                    args = (x_0),
                                                                                    targs = ((< Datatype( "bool" ) >))}}),
                                       transfer = transfer::case::Con {test = x_76,
                                                                       cases =   (true => L_51,
                                                                                  false => L_50)}})}
        ),
        main = main_0,
        }"#;
        let mlton_ssa: MltSsa = FromStr::from_str(s.trim()).unwrap();
        let sk = sk_from_mltonssa(&mlton_ssa);
        assert_eq!(sk.datatypes.len(), 3);
        assert_eq!(sk.globals.len(), 7);
        assert_eq!(sk.functions.len(), 2);
        assert_eq!(sk.main, "main_0");
        assert_eq!(sk.functions[0].name, "main_0");
        assert_eq!(sk.functions[1].name, "exit_0");
        assert_eq!(sk.functions[0].blocks.len(), 1);
        assert_eq!(sk.functions[1].blocks.len(), 1);
    }
}
