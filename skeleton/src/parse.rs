use nom::{
    IResult, Parser,
    bytes::complete::{tag, take_until},
    character::complete::{multispace0, multispace1},
    sequence::{delimited, preceded},
};

use mlton_ssa::ssa::SmlType;

mod test {
    use super::*;
}
