//! Item-level parsers.
//!
//! These are the "primitive token" parsers that consume exactly one item from the input.
//! They are typically used as building blocks for higher-level grammars.
//!
//! ## Soft vs hard failure
//!
//! Most item parsers follow this convention:
//!
//! - On **success**, they return the consumed item (no automatic `cut`).
//! - On **mismatch**, they push an error and return failure *without* cutting
//!   (so callers may backtrack if they choose).
//! - On **end-of-input**, they push an error and return failure.
//!
//! This means that combinators like [`crate::parser::choice::Choice`] can still try alternatives
//! after a mismatch.
//!
//! If you want a successful match to commit, wrap the parser with [`crate::parser::ParserOnce::cut`]
//! or explicitly insert [`crate::parser::prim::cut`] at the appropriate boundary.

use ::std::{fmt, marker::PhantomData};

use reborrow_generic::short::Rb;

use crate::{
    Back as _,
    input::{
        In,
        error::{
            ErrSink,
            std::{Unexpected, UnexpectedEndOfInput, UnexpectedItem},
        },
        inner::{Input, RbBack},
    },
    parser::{Parser, ParserMut, ParserOnce},
};

pub mod set;
use set::{Complement, ItemSet};

/// Expect end-of-input.
///
/// Note: on failure this calls `next()` to observe the unexpected item. If you need the failure
/// to be reversible, run it under a combinator that rolls back (e.g. `choice` / `maybe`).
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "";
/// let out = parse_ok_once(&mut input, eoi).unwrap();
/// assert_eq!(out, ());
/// assert_eq!(input, "");
/// ```
pub fn eoi<I: Input, E, N: Rb, L: RbBack>(mut i: In<I, E, N, L>) -> E::Out<(), UnexpectedItem<I::Item>>
where
    E: ErrSink<I, UnexpectedItem<I::Item>>,
{
    let p0 = i.input.pos();
    match i.input.next() {
        None => E::value(()),
        Some(item) => {
            let p1 = i.input.pos();
            i.push_err(p0..p1, UnexpectedItem(item))
        }
    }
}

/// Consume any single item.
///
/// - On success, this returns the consumed item (no automatic cut).
/// - On end-of-input, this pushes [`UnexpectedEndOfInput`] and returns failure.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let out = parse_ok_once(&mut input, any).unwrap();
/// assert_eq!(out, 'a');
/// assert_eq!(input, "b");
/// ```
pub fn any<I: Input, E, N: Rb, L: RbBack>(mut i: In<I, E, N, L>) -> E::Out<I::Item, UnexpectedEndOfInput>
where
    E: ErrSink<I, UnexpectedEndOfInput>,
{
    let p0 = i.input.pos();
    match i.input.next() {
        Some(item) => E::value(item),
        None => {
            let p1 = i.input.pos();
            i.push_err(p0..p1, UnexpectedEndOfInput)
        }
    }
}

fn run_satisfy<I: Input, E, N: Rb, L: RbBack>(
    mut i: In<I, E, N, L>, f: impl FnOnce(&I::Item) -> bool,
) -> E::Out<I::Item, Unexpected<I::Item>>
where
    E: ErrSink<I, Unexpected<I::Item>>,
{
    let p0 = i.input.pos();
    let checkpoint = i.checkpoint();
    match i.input.next() {
        Some(item) if f(&item) => E::value(item),
        Some(item) => {
            let p1 = i.input.pos();
            i.push_err(p0..p1, Unexpected::Item(item))
        }
        None => {
            let p1 = i.input.pos();
            if E::rollback_on_soft_failure() {
                i.rollback(checkpoint);
            }
            i.push_err(p0..p1, Unexpected::EndOfInput)
        }
    }
}

/// Create a parser that consumes one item and checks it with a predicate.
///
/// Note: this does not cut by itself.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let out = parse_ok_once(&mut input, satisfy(|c| c == 'a')).unwrap();
/// assert_eq!(out, 'a');
/// assert_eq!(input, "b");
/// ```
pub fn satisfy<F, It, A>(f: F) -> Satisfy<F, It, A> {
    Satisfy(f, PhantomData)
}

/// Parser returned by [`satisfy`].
pub struct Satisfy<F, It, A = ()>(F, PhantomData<fn() -> (It, A)>);
impl<F: Clone, It, A> Clone for Satisfy<F, It, A> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<F: Copy, It, A> Copy for Satisfy<F, It, A> {}
impl<F: fmt::Debug, It, A> fmt::Debug for Satisfy<F, It, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<I, E, N, L, F, It> ParserOnce<I, E, N, L> for Satisfy<F, It, (I, E, N, L)>
where
    I: Input<Item = It>,
    E: ErrSink<I, Unexpected<It>>,
    N: Rb,
    L: RbBack,
    F: FnOnce(It) -> bool,
    It: Clone,
{
    type Out = E::Out<It, Unexpected<It>>;

    fn run_once(self, i: In<I, E, N, L>) -> Self::Out {
        run_satisfy(i, |item| (self.0)(item.clone()))
    }
}
impl<I, E, N, L, F, It> ParserMut<I, E, N, L> for Satisfy<F, It, (I, E, N, L)>
where
    I: Input<Item = It>,
    E: ErrSink<I, Unexpected<It>>,
    N: Rb,
    L: RbBack,
    F: FnMut(It) -> bool,
    It: Clone,
{
    fn run_mut(&mut self, i: In<I, E, N, L>) -> Self::Out {
        run_satisfy(i, |item| (self.0)(item.clone()))
    }
}
impl<I, E, N, L, F, It> Parser<I, E, N, L> for Satisfy<F, It, (I, E, N, L)>
where
    I: Input<Item = It>,
    E: ErrSink<I, Unexpected<It>>,
    N: Rb,
    L: RbBack,
    F: Fn(It) -> bool,
    It: Clone,
{
    fn run(&self, i: In<I, E, N, L>) -> Self::Out {
        run_satisfy(i, |item| (self.0)(item.clone()))
    }
}

/// Create a parser that consumes one item and requires it to equal `item`.
///
/// Note: this does not cut by itself.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let out = parse_ok_once(&mut input, item('a')).unwrap();
/// assert_eq!(out, 'a');
/// assert_eq!(input, "b");
/// ```
pub fn item<T, A>(item: T) -> Item<T, T, A> {
    Item(item, PhantomData)
}

/// Create an `item` parser with an explicit input item type.
pub fn item_of<T, It, A>(item: T) -> Item<T, It, A> {
    Item(item, PhantomData)
}

/// Parser returned by [`item`].
pub struct Item<T, It = T, A = ()>(T, PhantomData<fn() -> (It, A)>);
impl<T: Clone, It, A> Clone for Item<T, It, A> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<T: Copy, It, A> Copy for Item<T, It, A> {}
impl<T: fmt::Debug, It, A> fmt::Debug for Item<T, It, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<I, E, N, L, T, It> ParserOnce<I, E, N, L> for Item<T, It, (I, E, N, L)>
where
    I: Input<Item = It>,
    E: ErrSink<I, Unexpected<It>>,
    N: Rb,
    L: RbBack,
    T: PartialEq<It>,
    It: Clone,
{
    type Out = E::Out<It, Unexpected<It>>;

    fn run_once(self, i: In<I, E, N, L>) -> Self::Out {
        run_satisfy(i, |it| self.0 == it.clone())
    }
}
impl<I, E, N, L, T, It> ParserMut<I, E, N, L> for Item<T, It, (I, E, N, L)>
where
    I: Input<Item = It>,
    E: ErrSink<I, Unexpected<It>>,
    N: Rb,
    L: RbBack,
    T: PartialEq<It>,
    It: Clone,
{
    fn run_mut(&mut self, i: In<I, E, N, L>) -> Self::Out {
        run_satisfy(i, |it| self.0 == it.clone())
    }
}
impl<I, E, N, L, T, It> Parser<I, E, N, L> for Item<T, It, (I, E, N, L)>
where
    I: Input<Item = It>,
    E: ErrSink<I, Unexpected<It>>,
    N: Rb,
    L: RbBack,
    T: PartialEq<It>,
    It: Clone,
{
    fn run(&self, i: In<I, E, N, L>) -> Self::Out {
        run_satisfy(i, |it| self.0 == it.clone())
    }
}

/// Create a parser that consumes one item and requires it to be in `items`.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let out = parse_ok_once(&mut input, one_of("ab")).unwrap();
/// assert_eq!(out, 'a');
/// assert_eq!(input, "b");
/// ```
pub fn one_of<S, It, A>(items: S) -> OneOf<S::Copyable, It, A>
where
    S: ItemSet<It>,
{
    OneOf(items.into_copyable(), PhantomData)
}

/// Create a parser that consumes one item and requires it to NOT be in `items`.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ba";
/// let out = parse_ok_once(&mut input, none_of("a")).unwrap();
/// assert_eq!(out, 'b');
/// assert_eq!(input, "a");
/// ```
pub fn none_of<S, It, A>(items: S) -> OneOf<Complement<S::Copyable>, It, A>
where
    S: ItemSet<It>,
{
    OneOf(items.into_copyable().complement(), PhantomData)
}

/// Parser returned by [`one_of`] / [`none_of`].
pub struct OneOf<S, It, A = ()>(S, PhantomData<fn() -> (It, A)>);
impl<S: Clone, It, A> Clone for OneOf<S, It, A> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<S: Copy, It, A> Copy for OneOf<S, It, A> {}
impl<S: fmt::Debug, It, A> fmt::Debug for OneOf<S, It, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<S, I, E, N, L, It> ParserOnce<I, E, N, L> for OneOf<S, It, (I, E, N, L)>
where
    S: ItemSet<It>,
    I: Input<Item = It>,
    E: ErrSink<I, Unexpected<It>>,
    N: Rb,
    L: RbBack,
{
    type Out = E::Out<It, Unexpected<It>>;

    fn run_once(self, i: In<I, E, N, L>) -> Self::Out {
        run_satisfy(i, |item| self.0.has(item))
    }
}
impl<S, I, E, N, L, It> ParserMut<I, E, N, L> for OneOf<S, It, (I, E, N, L)>
where
    S: ItemSet<It>,
    I: Input<Item = It>,
    E: ErrSink<I, Unexpected<It>>,
    N: Rb,
    L: RbBack,
{
    fn run_mut(&mut self, i: In<I, E, N, L>) -> Self::Out {
        run_satisfy(i, |item| self.0.has(item))
    }
}
impl<S, I, E, N, L, It> Parser<I, E, N, L> for OneOf<S, It, (I, E, N, L)>
where
    S: ItemSet<It>,
    I: Input<Item = It>,
    E: ErrSink<I, Unexpected<It>>,
    N: Rb,
    L: RbBack,
{
    fn run(&self, i: In<I, E, N, L>) -> Self::Out {
        run_satisfy(i, |item| self.0.has(item))
    }
}
