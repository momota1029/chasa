//! Tag parsers (exact sequence matching).
//!
//! This module provides `tag` to match a fixed sequence of input items.
//! The argument can be either:
//! - a string literal (`&str`) for `char` inputs, or
//! - a slice/array of expected tokens (`&[T]` / `&[T; N]`).

use ::std::marker::PhantomData;

use reborrow_generic::short::Rb;

use crate::{
    input::{
        In,
        error::ErrSink,
        error::std::Unexpected,
        inner::{Back as _, Input, RbBack},
    },
    parser::{
        Parser, ParserMut, ParserOnce,
        seq::{ExpectedItem as _, ItemSeq},
    },
};

/// Match an exact sequence.
///
/// - On success: consumes the whole sequence and returns `()`.
/// - On failure: pushes an `Unexpected` error at the mismatch location.
///
/// This does not cut by itself.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "hello!";
/// let out = parse_ok_once(&mut input, tag("hello")).unwrap();
/// assert_eq!(out, ());
/// assert_eq!(input, "!");
/// ```
pub fn tag<S, A>(expected: S) -> Tag<S, A> {
    Tag(expected, PhantomData)
}

/// Parser that matches a fixed sequence of items.
#[derive(Clone, Copy, Debug, Hash)]
pub struct Tag<S, A>(pub(crate) S, PhantomData<fn() -> A>);

fn run_tag<I, E, N, L, S>(mut i: In<I, E, N, L>, expected: S) -> E::Out<(), Unexpected<I::Item>>
where
    I: Input,
    E: ErrSink<I, Unexpected<I::Item>>,
    N: Rb,
    L: RbBack,
    S: ItemSeq<I::Item>,
{
    for expected in expected.iter() {
        let checkpoint = i.checkpoint();
        let p0 = i.pos();
        match i.input.next() {
            Some(actual) if expected.eq_item(&actual) => {}
            Some(actual) => {
                let p1 = i.pos();
                if E::rollback_on_soft_failure() {
                    i.rollback(checkpoint);
                }
                return i.push_err(p0..p1, Unexpected::Item(actual));
            }
            None => {
                let p1 = i.pos();
                return i.push_err(p0..p1, Unexpected::EndOfInput);
            }
        }
    }
    E::value(())
}

impl<I, E, N, L, S> ParserOnce<I, E, N, L> for Tag<S, (I, E, N, L)>
where
    I: Input,
    E: ErrSink<I, Unexpected<I::Item>>,
    N: Rb,
    L: RbBack,
    S: ItemSeq<I::Item>,
{
    type Out = E::Out<(), Unexpected<I::Item>>;

    fn run_once(self, i: In<I, E, N, L>) -> Self::Out {
        run_tag(i, self.0)
    }
}

impl<I, E, N, L, S> ParserMut<I, E, N, L> for Tag<S, (I, E, N, L)>
where
    I: Input,
    E: ErrSink<I, Unexpected<I::Item>>,
    N: Rb,
    L: RbBack,
    S: ItemSeq<I::Item>,
{
    fn run_mut(&mut self, i: In<I, E, N, L>) -> Self::Out {
        run_tag(i, self.0.clone())
    }
}

impl<I, E, N, L, S> Parser<I, E, N, L> for Tag<S, (I, E, N, L)>
where
    I: Input,
    E: ErrSink<I, Unexpected<I::Item>>,
    N: Rb,
    L: RbBack,
    S: ItemSeq<I::Item>,
{
    fn run(&self, i: In<I, E, N, L>) -> Self::Out {
        run_tag(i, self.0.clone())
    }
}
