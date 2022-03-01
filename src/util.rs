use crate::{error::CustomBuilder as Cb, ICont, IOk, IResult, Input, ParserOnce};

pub(crate) fn run_drop<I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>, T>(
    p: P, cont: ICont<I, C, S, M>, dropped: T,
) -> (IResult<P::Output, I, S, M>, Option<T>) {
    let ICont { ok, config, drop } = cont;
    let mut dropped = Some(dropped);
    let res = p.run_once(ICont {
        ok: IOk { cutted: false, ..ok },
        config,
        drop: &mut || {
            if dropped.is_some() {
                dropped = None;
                drop()
            }
        },
    });
    (res, dropped)
}

use crate::*;
fn te() {
    let num = one_of('0'..='9').many::<String>().and_then(|str| str.parse::<u32>().map_err(prim::Error::Message));
    let prod = num.sep1(1, |a, b| a * b, char('*'));
    let sum = prod.sep1(0, |a, b| a + b, char('+'));
    assert_eq!(sum.parse_easy("1+2*3+4".chars()).ok(), Some(11));
}
