use std::{fmt, str::FromStr};

use egg::{Id, define_language};

use sml_utils::SmlType;

use crate::parse::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lit {
    Word8(u8),
    Word16(u16),
    Word32(u32),
    Word64(u64),
    Unit,
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Word8(v) => write!(f, "Lit<{:#x} : word8>", v),
            Lit::Word16(v) => write!(f, "Lit<{:#x} : word16>", v),
            Lit::Word32(v) => write!(f, "Lit<{:#x} : word32>", v),
            Lit::Word64(v) => write!(f, "Lit<{:#x} : word64>", v),
            Lit::Unit => write!(f, "Lit<()>"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseLitErr;

impl FromStr for Lit {
    type Err = ParseLitErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_lit(s) {
            Ok(("", lit)) => Ok(lit),
            _ => Err(ParseLitErr),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prim(pub String);

#[derive(Debug, PartialEq, Eq)]
pub struct ParsePrimErr;

impl FromStr for Prim {
    type Err = ParsePrimErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_prim(s) {
            Ok(("", prim)) => Ok(prim),
            _ => Err(ParsePrimErr),
        }
    }
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Prim<{}>", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constr {
    pub constr_type: SmlType,
    pub name: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseConstrErr;

impl FromStr for Constr {
    type Err = ParseConstrErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_constr(s) {
            Ok(("", constr)) => Ok(constr),
            _ => Err(ParseConstrErr),
        }
    }
}

impl fmt::Display for Constr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Constr<{}::{}>", self.constr_type, self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Param(pub String, pub SmlType);

#[derive(Debug, PartialEq, Eq)]
pub struct ParseParamErr;

impl FromStr for Param {
    type Err = ParseConstrErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_param(s) {
            Ok(("", param)) => Ok(param),
            _ => Err(ParseConstrErr),
        }
    }
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Param<{} : {}>", self.0, self.1)
    }
}

define_language! {
    pub enum FPeg {
        // A literal of an SML primitive type
        Literal(Lit),

        // Represents a block argument
        Parameter(Param),

        // Represents a call of an SML primitive function
        CallPrim(Prim, Box<[Id]>),

        // Construct a datatype given a constructor and an arbitrary number of args
        Construct(Constr, Box<[Id]>),

        // Deconstruct given a constructor and a field
        Deconstruct(Constr, [Id; 2]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use egg::{EGraph, RecExpr};

    #[test]
    fn test_lit_to_str() {
        assert_eq!(format!("{}", Lit::Word8(0)), "Lit<0x0 : word8>");
        assert_eq!(format!("{}", Lit::Word16(0)), "Lit<0x0 : word16>");
        assert_eq!(format!("{}", Lit::Word32(0)), "Lit<0x0 : word32>");
        assert_eq!(format!("{}", Lit::Word64(0)), "Lit<0x0 : word64>");
        assert_eq!(format!("{}", Lit::Unit), "Lit<()>");
    }

    #[test]
    fn test_prim_to_str() {
        let add = "Add".to_owned();
        let prim = Prim(add);
        assert_eq!(format!("{}", prim), "Prim<Add>");
    }

    #[test]
    fn test_constr_to_str() {
        let add = "Add".to_owned();
        let t = "T".to_owned();
        let constr = Constr {
            constr_type: t,
            name: add,
        };
        assert_eq!(format!("{}", constr), "Constr<T::Add>");
    }

    #[test]
    fn test_fpeg() {
        let w32 = "word32";
        let mut e: RecExpr<FPeg> = RecExpr::default();
        let x_e = e.add(FPeg::Parameter(Param("x".to_owned(), w32.to_owned())));
        let y_e = e.add(FPeg::Parameter(Param("y".to_owned(), w32.to_owned())));
        let add_e = e.add(FPeg::CallPrim(Prim("add".to_owned()), Box::new([x_e, y_e])));
        let lit2_e = e.add(FPeg::Literal(Lit::Word32(2)));
        let _ = e.add(FPeg::CallPrim(
            Prim("mul".to_owned()),
            Box::new([lit2_e, add_e]),
        ));

        assert_eq!(
            e.to_string(),
            "(Prim<mul> \"Lit<0x2 : word32>\" (Prim<add> \"Param<x : word32>\" \"Param<y : word32>\"))"
        );

        let mut egraph: EGraph<FPeg, ()> = EGraph::default();
        let e_id = egraph.add_expr(&e);
        egraph.rebuild();

        assert_eq!(Some(e_id), egraph.lookup_expr(&e));

        let e_parsed: RecExpr<FPeg> = "(Prim<mul> \"Lit<0x2 : word32>\" (Prim<add> \"Param<x : word32>\" \"Param<y : word32>\"))".parse().unwrap();
        assert_eq!(Some(e_id), egraph.lookup_expr(&e_parsed));
    }
}
