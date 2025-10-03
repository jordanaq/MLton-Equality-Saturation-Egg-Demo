use std::{collections::HashMap, fmt, str::FromStr};

use egg::{EGraph, Id, RecExpr, define_language};

use mlton_ssa::ssa::{Const, SmlType};

use crate::parse::*;

/// Represents a primitive SML function in the FPeg IR
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prim(pub String);

#[derive(Debug, PartialEq, Eq)]
pub struct ParsePrimErr;

impl FromStr for Prim {
    type Err = ParsePrimErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        todo!()
        /*
        match parse_prim(s) {
            Ok(("", prim)) => Ok(prim),
            _ => Err(ParsePrimErr),
        }
        */
    }
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Prim<{}>", self.0)
    }
}

/// Represents a constructor use in the FPeg IR
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

/// Represents a parameter in the FPeg IR
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
    /// Defines the FPeg IR for the e-graph
    pub enum FPegL {
        // A literal of an SML primitive type
        Literal(Const),

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

type Analysis = (); // TODO:
pub type Region = egg::Id;

/// Wrapper for FPeg e-graph
#[derive(Debug, Clone)]
pub struct FPeg {
    egraph: EGraph<FPegL, Analysis>,
    region_map: HashMap<String, Region>,
}

impl FPeg {
    pub fn find_region_by_name(&self, name: &str) -> Option<Region> {
        self.region_map.get(name).copied()
    }
}

impl Default for FPeg {
    fn default() -> Self {
        FPeg {
            egraph: EGraph::<FPegL, Analysis>::default(),
            region_map: HashMap::<String, Region>::default(),
        }
    }
}

impl FPeg {
    pub fn construct_region(&mut self, code: String) -> Region {
        if let Some(region) = self.region_map.get(&code) {
            return *region;
        }

        let code_expr: RecExpr<FPegL> = code.parse().unwrap();
        let region = self.egraph.add_expr(&code_expr);
        self.region_map.insert(code, region);
        region
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use egg::RecExpr;
    use mlton_ssa::ssa::WordSize;

    #[test]
    fn test_prim_to_str() {
        let add = "Add".to_owned();
        let prim = Prim(add);
        assert_eq!(format!("{}", prim), "Prim<Add>");
    }

    #[test]
    fn test_fpeg() {
        let w32 = "Word32".parse::<SmlType>().unwrap();
        let mut e: RecExpr<FPegL> = RecExpr::default();
        let x_e = e.add(FPegL::Parameter(Param("x".to_owned(), w32.clone())));
        let y_e = e.add(FPegL::Parameter(Param("y".to_owned(), w32.clone())));
        let add_e = e.add(FPegL::CallPrim(
            Prim("add".to_owned()),
            Box::new([x_e, y_e]),
        ));
        let lit2_e = e.add(FPegL::Literal(Const::Word(WordSize::W32, 2)));
        let _ = e.add(FPegL::CallPrim(
            Prim("mul".to_owned()),
            Box::new([lit2_e, add_e]),
        ));

        assert_eq!(
            e.to_string(),
            "(Prim<mul> \"Lit<0x2 : word32>\" (Prim<add> \"Param<x : word32>\" \"Param<y : word32>\"))"
        );

        let mut egraph: EGraph<FPegL, ()> = EGraph::default();
        let e_id = egraph.add_expr(&e);
        egraph.rebuild();

        assert_eq!(Some(e_id), egraph.lookup_expr(&e));

        let e_parsed: RecExpr<FPegL> =
            "(Prim<mul> \"Lit<0x2 : word32>\" (Prim<add> \"Param<x : word32>\" \"Param<y : word32>\"))"
                .parse()
                .unwrap();
        assert_eq!(Some(e_id), egraph.lookup_expr(&e_parsed));
    }
}
