use std::fmt;

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
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Word8(v)   => write!(f, "Word8({})", v),
            Lit::Word32(v)  => write!(f, "Word32({})", v),
            Lit::Word64(v)  => write!(f, "Word64({})", v),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseLitErr;

impl std::str::FromStr for Lit {
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
pub struct Prim(String, Vec<Type>);

define_language! {
    pub enum FPeg {
        Lit(Lit),
        Arg(String),
        "Prim" = Prim(Box<[Id]>),
    }
}
