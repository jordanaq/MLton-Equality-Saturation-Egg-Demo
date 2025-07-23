use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{
        multispace0,
        multispace1
    },
    sequence::{delimited, preceded},
    IResult,
    Parser
};

use crate::skeleton::{
    Block,
    FuncId,
    Skeleton,
    Transfer
};

use parse_utils::{
    paren_list,
    word,
};

// use e_graph::{ FPeg, Region };

impl Skeleton {
    fn parse_comment<'a>(&'a self, s: &'a str) -> IResult<&'a str, &'a str> {
        delimited(tag("/*"), take_until("*/"), tag("*/")).parse(s)
    }
    
    fn parse_transfer_goto<'a>(&'a mut self, fun_id: FuncId, s: &'a str) -> IResult<&'a str, Transfer> {
        let (rest, (b, rs)) : (&str, (&str, Vec<&str>)) =
            preceded(
                (tag("goto"), multispace1),
                (word(), preceded(multispace0, paren_list()))
            ).parse(s)?;

        let b_id = match self.find_block_by_name(b) {
            Some(b) => b.id,
            None => {
                let b_id = self.fresh_id();
                self.add_block(
                    fun_id, 
                    Block {
                        id: b_id,
                        name: b.to_string(),
                        inputs: vec![],
                        transfer: Transfer::Bug,
                    }
                ).unwrap();
                b_id
            }
        };

        let r_ids = rs.iter().map(|r_s| self.find_region_by_name(r_s).unwrap()).collect();
        
        Ok((rest, Transfer::Goto(b_id, r_ids)))
    }
}
