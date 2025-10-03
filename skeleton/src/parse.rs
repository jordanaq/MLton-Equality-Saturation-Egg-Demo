use nom::{
    IResult, Parser,
    bytes::complete::{tag, take_until},
    character::complete::{multispace0, multispace1},
    sequence::{delimited, preceded},
};

use crate::skeleton::{Block, Func, FuncArg, FuncId, Skeleton, Transfer};

use parse_utils::{paren_list, word};

use sml_utils::SmlType;

impl Skeleton {
    fn parse_comment<'a>(&'a self, s: &'a str) -> IResult<&'a str, &'a str> {
        delimited(tag("/*"), take_until("*/"), tag("*/")).parse(s)
    }

    fn parse_transfer_goto<'a>(
        &'a mut self,
        fun_id: FuncId,
        s: &'a str,
    ) -> IResult<&'a str, Transfer> {
        let (rest, (b, rs)): (&str, (&str, Vec<&str>)) = preceded(
            (tag("goto"), multispace1),
            (word(), preceded(multispace0, paren_list())),
        )
        .parse(s)?;

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
                    },
                )
                .unwrap();
                b_id
            }
        };

        let r_ids = rs
            .iter()
            .map(|r_s| self.find_region_by_name(r_s).unwrap())
            .collect();

        Ok((rest, Transfer::Goto(b_id, r_ids)))
    }
}

mod test {
    use crate::skeleton::BlockId;
    use bimap::BiMap;
    use std::collections::HashMap;

    use super::*;

    fn create_test_skeleton() -> (Skeleton, FuncId) {
        let mut sk = Skeleton::new();
        let fid: FuncId = sk.fresh_id();

        let fargs = vec![
            FuncArg::new("x".into(), "w32".into()),
            FuncArg::new("acc".into(), "w32".into()),
        ];

        let mut m: BiMap<String, BlockId> = BiMap::new();

        ["L0", "L1", "L2", "L3", "L4", "L5"]
            .iter()
            .for_each(|&name| {
                let b_id = sk.fresh_id();
                m.insert(name.into(), b_id);
            });

        let l0 = Block::new(
            m.get_by_left("L0".into()).unwrap().clone(),
            "L0".into(),
            Vec::new(),
            Transfer::make_goto(m.get_by_left("L1".into()).unwrap().clone(), vec![]),
        );

        let l1 = Block::new(
            m.get_by_left("L1".into()).unwrap().clone(),
            "L1".into(),
            vec![("x0".into(), "w32".into()), ("acc0".into(), "w32".into())],
            Transfer::make_goto(m.get_by_left("L2".into()).unwrap().clone(), vec![]),
        );

        let ret_t: SmlType = "w32".to_string();

        let entry = todo!();

        todo!()
    }

    #[test]
    fn test_parse_transfer_goto() {
        todo!()
    }
}
