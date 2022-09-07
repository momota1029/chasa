use super::error::ParseError;

use {
    super::{
        input::InputOnce,
        parser::{Args, Parser, ParserOnce},
    },
    std::ops::Bound,
};

pub struct Consume<'a>(ResourceList<'a>);
impl<'a> Consume<'a> {
    #[inline(always)]
    pub fn new() -> Self {
        Self(ResourceList::Nil)
    }
    #[inline(always)]
    pub fn cons<T, O>(&mut self, item: T, f: impl FnOnce(Consume) -> O) -> (O, Option<T>) {
        let mut some_drop = (Some(item), &mut self.0);
        (f(Consume(ResourceList::Cons(&mut some_drop))), some_drop.0)
    }
    #[inline(always)]
    pub fn is_dropped<O>(&mut self, f: impl FnOnce(Consume) -> O) -> (O, bool) {
        let mut some_drop = (false, &mut self.0);
        (f(Consume(ResourceList::Cons(&mut some_drop))), some_drop.0)
    }
    #[inline(always)]
    pub fn drop(&mut self) {
        self.0.drop()
    }
}

trait SomeDrop {
    fn some_drop(&mut self);
}
impl<'a, 'b, T> SomeDrop for (Option<T>, &'a mut ResourceList<'b>) {
    fn some_drop(&mut self) {
        if self.0.is_some() {
            self.0 = None;
            self.1.drop();
        }
    }
}
impl<'a, 'b> SomeDrop for (bool, &'a mut ResourceList<'b>) {
    fn some_drop(&mut self) {
        if self.0 == false {
            self.0 = true;
            self.1.drop();
        }
    }
}
enum ResourceList<'a> {
    Nil,
    Cons(&'a mut dyn SomeDrop),
}
impl<'a> ResourceList<'a> {
    fn drop(&mut self) {
        match self {
            Self::Nil => (),
            Self::Cons(some_drop) => some_drop.some_drop(),
        }
    }
}

pub trait IntoChars {
    type Item;
    type Iterator: Iterator<Item = Self::Item>;
    fn into_chars(self) -> Self::Iterator;
}
impl<'a> IntoChars for &'a String {
    type Item = char;
    type Iterator = std::str::Chars<'a>;
    #[inline]
    fn into_chars(self) -> Self::Iterator {
        self.chars()
    }
}
impl<'a> IntoChars for &'a str {
    type Item = char;
    type Iterator = std::str::Chars<'a>;
    #[inline]
    fn into_chars(self) -> Self::Iterator {
        self.chars()
    }
}
impl<'a, T: Clone> IntoChars for &'a [T] {
    type Item = T;
    type Iterator = std::iter::Cloned<std::slice::Iter<'a, T>>;
    #[inline]
    fn into_chars(self) -> Self::Iterator {
        self.into_iter().cloned()
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
        impl<$item> CharsOrRange<$item> for $t {
            type To = (Bound<$item>, Bound<$item>);
            #[inline]
            fn to(self) -> Self::To {
                self.to_pair()
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

/// Currently, Range cannot be copied, so [`Repeat`][`crate::many::Repeat`] cannot be copied either. Countermeasure.
pub trait RangeWithOrd<T> {
    fn to_pair(self) -> (Bound<T>, Bound<T>);
}
impl RangeWithOrd<usize> for usize {
    fn to_pair(self) -> (Bound<usize>, Bound<usize>) {
        (Bound::Included(self.clone()), Bound::Included(self))
    }
}
impl<T> RangeWithOrd<T> for std::ops::Range<T> {
    fn to_pair(self) -> (Bound<T>, Bound<T>) {
        (Bound::Included(self.start), Bound::Excluded(self.end))
    }
}
impl<T> RangeWithOrd<T> for std::ops::RangeInclusive<T> {
    fn to_pair(self) -> (Bound<T>, Bound<T>) {
        let (start, end) = self.into_inner();
        (Bound::Included(start), Bound::Included(end))
    }
}
impl<T> RangeWithOrd<T> for std::ops::RangeFrom<T> {
    fn to_pair(self) -> (Bound<T>, Bound<T>) {
        (Bound::Excluded(self.start), Bound::Unbounded)
    }
}
impl<T> RangeWithOrd<T> for std::ops::RangeTo<T> {
    fn to_pair(self) -> (Bound<T>, Bound<T>) {
        (Bound::Unbounded, Bound::Excluded(self.end))
    }
}
impl<T> RangeWithOrd<T> for std::ops::RangeToInclusive<T> {
    fn to_pair(self) -> (Bound<T>, Bound<T>) {
        (Bound::Unbounded, Bound::Included(self.end))
    }
}
impl<T> RangeWithOrd<T> for std::ops::RangeFull {
    fn to_pair(self) -> (Bound<T>, Bound<T>) {
        (Bound::Unbounded, Bound::Unbounded)
    }
}

struct FromFunc<F>(F);
impl<I: InputOnce, O, E: ParseError<I>, C, S, F: FnOnce(Args<I, E, C, S>) -> Option<O>> ParserOnce<I, O, E, C, S>
    for FromFunc<F>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        self.0(args)
    }
}
impl<I: InputOnce, O, E: ParseError<I>, C, S, F: FnMut(Args<I, E, C, S>) -> Option<O>> Parser<I, O, E, C, S>
    for FromFunc<F>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.0(args)
    }
}

/// Hides recursive parser types and size ambiguity.
#[inline(always)]
pub fn run<I: InputOnce, O, E: ParseError<I>, C, S, P: Parser<I, O, E, C, S>>(
    mut parser: P,
) -> impl Parser<I, O, E, C, S> {
    FromFunc(move |k: Args<I, E, C, S>| parser.run(k))
}

/// Hides recursive parser types and size ambiguity. It does not matter if this parser has ownership.
#[inline(always)]
pub fn run_once<I: InputOnce, O, E: ParseError<I>, C, S, P: ParserOnce<I, O, E, C, S>>(
    parser: P,
) -> impl ParserOnce<I, O, E, C, S> {
    FromFunc(move |k: Args<I, E, C, S>| parser.run_once(k))
}
