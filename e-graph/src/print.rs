use std::fmt::Display;

use pretty::RcDoc;

use print_utils::*;

use crate::fpeg::{Constr, FPeg, FPegL, PrimWrapper, Region};

impl PrettyDoc for Constr {
    fn to_doc(&self) -> RcDoc<'_> {
        named_object_printer(
            "Constr",
            vec![
                ("constr_type", RcDoc::text(self.constr_type.to_string())),
                ("tycon", string_printer(&self.tycon)),
            ],
        )
    }
}

impl Display for Constr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

impl PrettyDoc for PrimWrapper {
    fn to_doc(&self) -> RcDoc<'_> {
        let items = vec![
            ("prim", self.prim.to_doc()),
            (
                "targs",
                option_printer_apply(|v| paren_list_printer(v), &self.targs),
            ),
            ("ty", option_printer(&self.ty)),
        ];
        named_object_printer("PrimWrapper", items)
    }
}

impl Display for PrimWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

fn pretty_region(r: &Region) -> RcDoc<'_> {
    RcDoc::text("Region<")
        .append(RcDoc::text(format!("{}", r)))
        .append(RcDoc::text(">"))
}

impl PrettyDoc for FPegL {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            FPegL::PrimApp(prim_wrapper, ids) => {
                let items = vec![
                    ("prim", prim_wrapper.to_doc()),
                    ("args", paren_list_printer_apply(|r| pretty_region(r), ids)),
                ];
                named_object_printer("PrimApp", items)
            }
            FPegL::Construct(constr, ids) => {
                let items = vec![
                    ("constr", constr.to_doc()),
                    ("args", paren_list_printer_apply(|r| pretty_region(r), ids)),
                ];
                named_object_printer("Construct", items)
            }
            FPegL::Select([tuple_r, offset_r]) => {
                let items = vec![
                    ("tuple", pretty_region(tuple_r)),
                    ("offset", pretty_region(offset_r)),
                ];
                named_object_printer("Select", items)
            }
            FPegL::Tuple(ids) => {
                named_tuple_printer("Tuple", ids.iter().map(|r| pretty_region(r)).collect())
            }
            FPegL::Literal(c) => named_object_printer("Literal", vec![("const", c.to_doc())]),
            FPegL::Arg(v) => named_object_printer("Arg", vec![("name", string_printer(v))]),
        }
    }
}

impl PrettyDoc for FPeg {
    fn to_doc(&self) -> RcDoc<'_> {
        let items = vec![("egraph", paren_list_printer(self.egraph.nodes()))];
        named_object_printer("FPeg", items)
    }
}

#[cfg(test)]
mod tests {
    use mlton_ssa::ssa::{Prim, SmlType, WordSize};

    use super::*;

    #[test]
    fn test_constr_print_inverse() {
        let constr = Constr {
            constr_type: SmlType::Datatype("list_0".into()),
            tycon: "nil_0".into(),
        };
        let s = format!("{}", constr);
        let parsed: Constr = s.parse().unwrap();
        assert_eq!(constr, parsed);
    }

    #[test]
    fn test_prim_wrapper_inverse() {
        let prim = PrimWrapper {
            prim: Prim::WordAdd(WordSize::W64),
            targs: Some(vec![
                SmlType::Word(WordSize::W64),
                SmlType::Word(WordSize::W64),
            ]),
            ty: Some(SmlType::Word(WordSize::W64)),
        };
        let s = format!("{}", pretty_print(&prim));
        let parsed: PrimWrapper = s.parse().unwrap();
        assert_eq!(prim, parsed);
    }

    /*
    #[test]
    fn test_fpegl_inverse() {
        let fpegl = FPegL::PrimApp(
            PrimWrapper {
                prim: mlton_ssa::ssa::Prim::make_pure_sml("add_w64"),
                targs: Some(vec![
                    SmlType::Word(WordSize::W64),
                    SmlType::Word(WordSize::W64),
                ]),
            },
            vec![Region::from(1), Region::from(2)].into_boxed_slice(),
        );
        let s = format!("{}", pretty_print(&fpegl));
        let (_, parsed) = parse_fpegl(&s).unwrap();
        assert_eq!(fpegl, parsed);

        let fpegl = FPegL::Literal(Const::Word(WordSize::W64, 0xA));
        let s = format!("{}", pretty_print(&fpegl));
        let (_, parsed) = parse_fpegl(&s).unwrap();
        assert_eq!(fpegl, parsed);
    }
    */
}
