use super::error::ParseError;

use {
    super::{
        input::Input,
        parser::{Args, Parser, ParserOnce},
    },
    std::{ops::Bound, ptr::NonNull},
};

pub struct Consume<'a, T>(ResourceList<'a, T>); // 実装の隠蔽
impl<'a, T> Consume<'a, T> {
    #[inline]
    pub fn new() -> Self {
        Self(ResourceList::Nil)
    }
    #[inline]
    pub fn cons<O>(&mut self, item: T, f: impl FnOnce(&mut Consume<T>) -> O) -> (O, Option<T>) {
        let hd = Some(item);
        let res = f(&mut Consume(ResourceList::ResourceCons { hd: (&hd).into(), tl: (&self.0).into() }));
        (res, hd)
    }
    #[inline]
    pub fn drop_test<O>(&mut self, f: impl FnOnce(&mut Consume<T>) -> O) -> (O, bool) {
        let flag = false;
        let res = f(&mut Consume(ResourceList::DropTest { hd: (&flag).into(), tl: (&self.0).into() }));
        (res, flag)
    }
    #[inline]
    pub fn wrap<S, O>(&mut self, f: impl FnOnce(&mut Consume<S>) -> O) -> O {
        f(&mut Consume(ResourceList::DropWrap(&mut move || self.drop())))
    }
    #[inline]
    pub fn drop(&mut self) {
        self.0.drop()
    }
}

// DropWrapの型を詳細にしたような動きしかしないので安全だが、各要素を隠蔽する必要があったのかは疑問
enum ResourceList<'a, T> {
    Nil,
    DropWrap(&'a mut dyn FnMut()),
    ResourceCons { hd: NonNull<Option<T>>, tl: NonNull<ResourceList<'a, T>> },
    DropTest { hd: NonNull<bool>, tl: NonNull<ResourceList<'a, T>> },
}
impl<'a, T> ResourceList<'a, T> {
    fn drop(&mut self) {
        match std::mem::replace(self, Self::Nil) {
            Self::Nil => (),
            Self::DropWrap(f) => f(),
            Self::ResourceCons { mut hd, mut tl } => unsafe {
                if hd.as_ref().is_some() {
                    *hd.as_mut() = None;
                    tl.as_mut().drop()
                }
            },
            Self::DropTest { mut hd, mut tl } => unsafe {
                if !*hd.as_ref() {
                    *hd.as_mut() = true;
                    tl.as_mut().drop()
                }
            },
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
impl<I: Input, O, E: ParseError<I>, C, S: Clone, F: FnOnce(Args<I, E, C, S>) -> Option<O>> ParserOnce<I, O, E, C, S>
    for FromFunc<F>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        self.0(args)
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, F: FnMut(Args<I, E, C, S>) -> Option<O>> Parser<I, O, E, C, S>
    for FromFunc<F>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.0(args)
    }
}

/// Hides recursive parser types and size ambiguity.
#[inline(always)]
pub fn run<I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>>(
    mut parser: P,
) -> impl Parser<I, O, E, C, S> {
    FromFunc(move |k: Args<I, E, C, S>| parser.run(k))
}

/// Hides recursive parser types and size ambiguity. It does not matter if this parser has ownership.
#[inline(always)]
pub fn run_once<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>>(
    parser: P,
) -> impl ParserOnce<I, O, E, C, S> {
    FromFunc(move |k: Args<I, E, C, S>| parser.run_once(k))
}
