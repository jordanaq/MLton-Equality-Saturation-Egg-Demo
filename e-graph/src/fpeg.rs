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
            Ok(_) => Err(ParseLitErr),
            Err(_) => Err(ParseLitErr),
        }
    }
}


type Type = String;


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prim(String, Vec<Type>);

#[derive(Debug, PartialEq, Eq)]
pub struct ParsePrimErr;

impl FromStr for Prim {
    type Err = ParsePrimErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_prim(s) {
            Ok(_) => todo!(),
            Err(_) => todo!()
        }
   }
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constr(String, Vec<Type>);

#[derive(Debug, PartialEq, Eq)]
pub struct ParseConstrErr;

impl FromStr for Constr {
    type Err = ParseConstrErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_constr(s) {
            Ok(_) => todo!(),
            Err(_) => todo!()
        }
   }
}

impl fmt::Display for Constr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}


define_language! {
    pub enum FPeg {
        Lit(Lit),
        Arg(String),
        Prim(Prim, Vec<Id>),
        Constr(Constr, Vec<Id>),
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
}
