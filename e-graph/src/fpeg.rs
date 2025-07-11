use std::{fmt, str::FromStr};

use egg::{
    define_language,
    Id
};

use crate::parse::*;


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lit {
    Word8(u8),
    Word32(u32),
    Word64(u64),
    Unit,
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Word8(v)   => write!(f, "Word8({})", v),
            Lit::Word32(v)  => write!(f, "Word32({})", v),
            Lit::Word64(v)  => write!(f, "Word64({})", v),
            Lit::Unit       => write!(f, "Unit()"),
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


pub type Type = String;


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prim(pub String, pub Vec<Type>);

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
        write!(f, "Prim<{}>({})", self.0, self.1.join(", "))
    }
}


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constr(pub String, pub Type, pub Vec<Type>);

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
        write!(f, "Constr<{}>({}) : {}", self.0, self.2.join(", "), self.1)
    }
}


define_language! {
    pub enum FPeg {
        // A literal of an SML primitive type
        Literal(Lit),

        // Represents a block argument
        Arg(String),

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

    #[test]
    fn test_lit_to_str() {
        assert_eq!(format!("{}", Lit::Word8(0)), "Word8(0)");
        assert_eq!(format!("{}", Lit::Word32(0)), "Word32(0)");
        assert_eq!(format!("{}", Lit::Word64(0)), "Word64(0)");
        assert_eq!(format!("{}", Lit::Unit), "Unit()");
    }

    #[test]
    fn test_prim_to_str() {
        let add = "Add".to_owned();
        let xs = vec!["a".to_owned(), "b".to_owned(),];
        let prim = Prim(add, xs);
        assert_eq!(format!("{}", prim), "Prim<Add>(a, b)");
    }

    #[test]
    fn test_constr_to_str() {
        let add = "Add".to_owned();
        let xs = vec!["a".to_owned(), "b".to_owned(),];
        let t = "T".to_owned();
        let prim = Constr(add, t, xs);
        assert_eq!(format!("{}", prim), "Constr<Add>(a, b) : T");
    }
}
