use std::fmt::Display;

use crate::{
    error::{Builder as Eb, CustomBuilder as Cb},
    ICont, IOk, IResult, Input, LazyError, ParserOnce,
};

#[inline]
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

#[inline(always)]
pub fn run_satisfy<I: Input<Item = impl Display + 'static>, M: Cb, O>(
    input: &mut I, drop: &mut dyn FnMut(), cutted: bool, f: impl FnOnce(I::Item) -> Result<O, I::Item>,
) -> Result<O, LazyError<I, M>> {
    if cutted {
        drop()
    }
    let (index, pos) = (input.index(), input.pos());
    match input.next() {
        None => Err(Eb::unexpected_eoi().at::<I>(index, pos, None)),
        Some(Err(e)) => Err(Eb::message(e).at::<I>(index, pos, None)),
        Some(Ok(item)) => match f(item) {
            Ok(o) => {
                drop();
                Ok(o)
            },
            Err(item) => Err(Eb::unexpected(item).at::<I>(input.index(), pos, Some(input.pos()))),
        },
    }
}
