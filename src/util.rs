use std::fmt::Display;

use crate::{
    error::{Builder as Eb, CustomBuilder as Cb},
    input::IntoChars,
    prim, ICont, IOk, IResult, Input, LazyError, Parser, ParserOnce,
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

/// Currently, Range cannot be copied, so one_of cannot be copied either. Countermeasure.
pub trait CharsOrRange<Item> {
    type To;
    fn to(self) -> Self::To;
}
impl<I: IntoChars> CharsOrRange<<I as IntoChars>::Item> for I {
    type To = I;
    #[inline]
    fn to(self) -> Self {
        self
    }
}
macro_rules! chars_as_range {
    ($item:ident, $t:ty) =>{
        impl<$item: ranges::Domain> CharsOrRange<$item> for $t {
            type To = ranges::GenericRange<$item>;
            #[inline]
            fn to(self) -> Self::To {
                ranges::GenericRange::from(self)
            }
        }
    };
    ($iter:ident, $t:ty, $($ts:ty),+) => {
        chars_as_range!($iter,$t);
        chars_as_range!($iter,$($ts),+);
    }
}
chars_as_range!(
    Item,
    std::ops::Range<Item>,
    std::ops::RangeFrom<Item>,
    std::ops::RangeTo<Item>,
    std::ops::RangeInclusive<Item>,
    std::ops::RangeToInclusive<Item>
);
impl<Item: ranges::Domain> CharsOrRange<Item> for std::ops::RangeFull {
    type To = ranges::GenericRange<Item>;
    #[inline]
    fn to(self) -> Self::To {
        ranges::GenericRange::from(self)
    }
}

#[inline(always)]
pub fn run<I: Input, C, S, M: Cb, P: Parser<I, C, S, M>>(parser: P) -> impl Parser<I, C, S, M, Output = P::Output> {
    prim::parser(move |k| k.then(parser.to_ref()))
}

#[inline(always)]
pub fn run_mv<I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>>(
    parser: P,
) -> impl ParserOnce<I, C, S, M, Output = P::Output> {
    prim::parser_mv(move |k| k.then(parser))
}
