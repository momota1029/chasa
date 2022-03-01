use std::fmt::{Debug, Display};

use crate::error::Nil;

pub trait Input {
    type Item;
    type Error: std::error::Error + 'static;
    type Pos: Ord;
    fn index(&self) -> usize;
    fn pos(&self) -> Self::Pos;
    fn next(&mut self) -> Option<Result<Self::Item, Self::Error>>;
}
pub trait Counter<T> {
    type Pos: Display + Debug + Copy + Ord;
    type Error: std::error::Error + 'static;
    fn new() -> Self;
    fn next(&mut self, item: &T) -> Result<(), Self::Error>;
    fn pos(&self) -> Self::Pos;

    fn with_iter<I: Iterator<Item = T>>(iter: I) -> FromIterator<I, Self>
    where
        Self: Sized,
    {
        FromIterator(iter, Self::new(), 0)
    }
}
impl<T> Counter<T> for usize {
    type Pos = usize;
    type Error = Nil;
    #[inline]
    fn new() -> usize {
        0
    }
    #[inline]
    fn next(&mut self, _: &T) -> Result<(), Self::Error> {
        *self += 1;
        Ok(())
    }
    #[inline]
    fn pos(&self) -> usize {
        *self
    }
}
#[derive(Clone)]
pub struct FromIterator<I, C>(I, C, usize);
impl<I: Iterator, C: Counter<I::Item>> Input for FromIterator<I, C> {
    type Item = I::Item;
    type Error = C::Error;
    type Pos = C::Pos;

    fn index(&self) -> usize {
        self.2
    }
    fn pos(&self) -> Self::Pos {
        self.1.pos()
    }

    #[inline]
    fn next(&mut self) -> Option<Result<I::Item, C::Error>> {
        let item = self.0.next()?;
        Some(self.1.next(&item).map(move |_| item))
    }
}

pub trait IntoInput {
    type Item;
    type Error;
    type Pos;
    type IntoI: Input<Item = Self::Item, Error = Self::Error, Pos = Self::Pos>;
    fn into_input(self) -> Self::IntoI;
}
impl<I: Input> IntoInput for I {
    type Item = <I as Input>::Item;
    type Error = <I as Input>::Error;
    type Pos = <I as Input>::Pos;
    type IntoI = Self;
    fn into_input(self) -> Self::IntoI {
        self
    }
}
impl<'a> IntoInput for std::str::Chars<'a> {
    type Item = char;
    type Error = Nil;
    type Pos = usize;
    type IntoI = FromIterator<std::str::Chars<'a>, usize>;
    fn into_input(self) -> Self::IntoI {
        usize::with_iter(self)
    }
}
impl<'a> IntoInput for std::str::Bytes<'a> {
    type Item = u8;
    type Error = Nil;
    type Pos = usize;
    type IntoI = FromIterator<std::str::Bytes<'a>, usize>;
    fn into_input(self) -> Self::IntoI {
        usize::with_iter(self)
    }
}
