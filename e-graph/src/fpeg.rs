use std::fmt;

use egg::*;

use crate::parse::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Prim {
    Word8(u8),
    Word32(u32),
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Prim::Word8(v)   => write!(f, "Word8({})", v),
            Prim::Word32(v)  => write!(f, "Word32({})", v),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParsePrimErr;

impl std::str::FromStr for Prim {
    type Err = ParsePrimErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_prim(s) {
            Ok(("", prim)) => Ok(prim),
            Ok(_) => Err(ParsePrimErr),
            Err(_) => Err(ParsePrimErr),
        }
    }
}

define_language! {
    pub enum FPeg {
        Prim(Prim),
    }
}
