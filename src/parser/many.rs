//! `many`-family combinators for `OutOf` parsers.
//!
//! These combinators repeat a parser using `In::maybe` (via `or_not`) and accumulate values:
//!
//! - **Soft failure** ends the loop and rolls back the terminating attempt.
//! - **Hard failure** is reported via `ManyErr` (summary + inner error).
//!
//! Rollback is controlled explicitly by `cut`; these combinators do not infer consumption.

use std::marker::PhantomData;

use reborrow_generic::short::Rb;

use crate::{
    Input,
    input::{
        In,
        error::{ErrMode, ErrSink, OutOf},
        inner::{Back, RbBack},
    },
    parser::{ErrOf, Parser, ParserMut, ParserOnce, ValueOf, prim::RefOrMutParser},
};

/// Iterator driving the `many` family.
pub struct ManyIterator<'a, P, I: Input, E: ErrMode<I>, N: Rb, L: RbBack>
where
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    parser: P,
    i: In<'a, I, E, N, L>,
    is_end: bool,
    err: &'a mut Option<ErrOf<P, I, E, N, L>>,
}
impl<'a, P, I, E, N, L> Iterator for ManyIterator<'a, P, I, E, N, L>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    type Item = ValueOf<P, I, E, N, L>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.is_end || self.err.is_some() {
            None
        } else {
            let checkpoint = self.i.checkpoint();
            match <P::Out>::project_result(self.parser.by_mut().or_not().run_once(self.i.rb())) {
                Ok(Some(o)) => Some(o),
                Ok(None) => {
                    self.i.rollback(checkpoint);
                    self.is_end = true;
                    None
                }
                Err(e) => {
                    *self.err = Some(e);
                    None
                }
            }
        }
    }
}
/// Repeat a parser zero or more times and collect outputs.
pub struct Many<P, O>(P, PhantomData<fn() -> O>);
/// Create a [`Many`] combinator.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "aaab";
/// let out: String = parse_ok_once(&mut input, many(item('a'))).unwrap();
/// assert_eq!(out, "aaa");
/// assert_eq!(input, "b");
/// ```
pub fn many<P, O>(parser: P) -> Many<P, O> {
    Many(parser, PhantomData)
}
/// Error emitted by `many` when the inner parser fails with a cut.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ManyErr<O, E> {
    pub summary: O,
    pub err: E,
}
impl<P, I, E, N, L, O> ParserOnce<I, E, N, L> for Many<P, O>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    O: FromIterator<ValueOf<P, I, E, N, L>>,
{
    type Out = E::Out<O, ManyErr<O, ErrOf<P, I, E, N, L>>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let pos = i.input.pos();
        let o = ManyIterator { parser: self.0, i: i.rb(), is_end: false, err: &mut err }.collect();
        match err {
            None => E::value(o),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary: o, err }),
        }
    }
}

impl<P, I, E, N, L, O> ParserMut<I, E, N, L> for Many<P, O>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    O: FromIterator<ValueOf<P, I, E, N, L>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let pos = i.input.pos();
        let o = ManyIterator { parser: self.0.by_mut(), i: i.rb(), is_end: false, err: &mut err }.collect();
        match err {
            None => E::value(o),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary: o, err }),
        }
    }
}

impl<P, I, E, N, L, O> Parser<I, E, N, L> for Many<P, O>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    O: FromIterator<ValueOf<P, I, E, N, L>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let pos = i.input.pos();
        let o = ManyIterator { parser: self.0.by_ref(), i: i.rb(), is_end: false, err: &mut err }.collect();
        match err {
            None => E::value(o),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary: o, err }),
        }
    }
}

/// Iterator type for [`many_map`]-family combinators.
pub type ManyMapIterator<'a, 'b, P, I, E, N, L> = ManyIterator<'a, RefOrMutParser<'b, P, I, E, N, L>, I, E, N, L>;
/// Iterator type for [`many1_map`]-family combinators.
pub type Many1MapIterator<'a, 'b, P, I, E, N, L> =
    std::iter::Chain<std::iter::Once<ValueOf<P, I, E, N, L>>, ManyMapIterator<'a, 'b, P, I, E, N, L>>;

/// Like [`ManyMap`], but returns a [`ManyErr`] (with a summary) on hard failure.
pub struct ManyMap<P, F>(P, F);

/// Create a [`ManyMap`] combinator.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "aaab";
/// let out = parse_ok_once(&mut input, many_map(item('a'), |it| it.count())).unwrap();
/// assert_eq!(out, 3);
/// assert_eq!(input, "b");
/// ```
pub fn many_map<P, I, E, N, L, F, O>(parser: P, f: F) -> ManyMap<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> FnOnce(ManyMapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    ManyMap(parser, f)
}

/// Like [`many_map`], but intended for single-shot mappers (`FnOnce`).
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "aaab";
/// let prefix = String::from("got:");
/// let out = parse_ok_once(&mut input, many_map_once(item('a'), move |it| (prefix, it.count()))).unwrap();
/// assert_eq!(out.0, "got:");
/// assert_eq!(out.1, 3);
/// assert_eq!(input, "b");
/// ```
pub fn many_map_once<P, I, E, N, L, F, O>(parser: P, f: F) -> ManyMap<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> FnOnce(ManyMapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    ManyMap(parser, f)
}

/// Like [`many_map`], but intended for mutable/repeatable mappers (`FnMut`).
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "aaab";
/// let mut acc = 0usize;
/// let p = many_map_mut(item('a'), |it| {
///     acc += it.count();
///     acc
/// });
/// let out = parse_ok_once(&mut input, p).unwrap();
/// assert_eq!(out, 3);
/// assert_eq!(acc, 3);
/// assert_eq!(input, "b");
/// ```
pub fn many_map_mut<P, I, E, N, L, F, O>(parser: P, f: F) -> ManyMap<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> FnMut(ManyMapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    ManyMap(parser, f)
}

impl<P, F, I, E, N, L, O> ParserOnce<I, E, N, L> for ManyMap<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> FnOnce(ManyMapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    type Out = E::Out<O, ManyErr<O, ErrOf<P, I, E, N, L>>>;

    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        let pos = i.input.pos();
        let mut err = None;
        let (mut parser, f) = (self.0, self.1);
        let summary = f(ManyIterator { parser: RefOrMutParser::new_mut(&mut parser), i: i.rb(), is_end: false, err: &mut err });
        match err {
            None => E::value(summary),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary, err }),
        }
    }
}

impl<P, F, I, E, N, L, O> ParserMut<I, E, N, L> for ManyMap<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> FnMut(ManyMapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        let pos = i.input.pos();
        let mut err = None;
        let summary = (self.1)(ManyIterator { parser: RefOrMutParser::new_mut(&mut self.0), i: i.rb(), is_end: false, err: &mut err });
        match err {
            None => E::value(summary),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary, err }),
        }
    }
}

impl<P, F, I, E, N, L, O> Parser<I, E, N, L> for ManyMap<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> Fn(ManyMapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        let pos = i.input.pos();
        let mut err = None;
        let summary = (self.1)(ManyIterator { parser: RefOrMutParser::new_ref(&self.0), i: i.rb(), is_end: false, err: &mut err });
        match err {
            None => E::value(summary),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary, err }),
        }
    }
}

/// Like [`ManyMap`], but requires at least one element.
pub struct Many1Map<P, F>(P, F);

/// Create a [`Many1Map`] combinator.
pub fn many1_map<P, I, E, N, L, F, O>(parser: P, f: F) -> Many1Map<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> FnOnce(Many1MapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    Many1Map(parser, f)
}

/// Like [`many1_map`], but intended for single-shot mappers (`FnOnce`).
pub fn many1_map_once<P, I, E, N, L, F, O>(parser: P, f: F) -> Many1Map<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> FnOnce(Many1MapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    Many1Map(parser, f)
}

/// Like [`many1_map`], but intended for mutable/repeatable mappers (`FnMut`).
pub fn many1_map_mut<P, I, E, N, L, F, O>(parser: P, f: F) -> Many1Map<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> FnMut(Many1MapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    Many1Map(parser, f)
}

impl<P, F, I, E, N, L, O> ParserOnce<I, E, N, L> for Many1Map<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<Option<O>, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> FnOnce(Many1MapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    type Out = E::Out<O, ManyErr<Option<O>, ErrOf<P, I, E, N, L>>>;

    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        let pos = i.input.pos();
        let mut err = None;
        let (mut parser, f) = (self.0, self.1);
        let first = match parser.by_mut().run_once(i.rb()).as_result() {
            Ok(o) => o,
            Err(e) => return i.push_err(pos..i.input.pos(), ManyErr { summary: None, err: e }),
        };
        let it = std::iter::once(first).chain(ManyIterator {
            parser: RefOrMutParser::new_mut(&mut parser),
            i: i.rb(),
            is_end: false,
            err: &mut err,
        });
        let summary = f(it);
        match err {
            None => E::value(summary),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary: Some(summary), err }),
        }
    }
}

impl<P, F, I, E, N, L, O> ParserMut<I, E, N, L> for Many1Map<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<Option<O>, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> FnMut(Many1MapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        let pos = i.input.pos();
        let mut err = None;
        let first = match self.0.by_mut().run_once(i.rb()).as_result() {
            Ok(o) => o,
            Err(e) => return i.push_err(pos..i.input.pos(), ManyErr { summary: None, err: e }),
        };
        let it = std::iter::once(first).chain(ManyIterator {
            parser: RefOrMutParser::new_mut(&mut self.0),
            i: i.rb(),
            is_end: false,
            err: &mut err,
        });
        let summary = (self.1)(it);
        match err {
            None => E::value(summary),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary: Some(summary), err }),
        }
    }
}

impl<P, F, I, E, N, L, O> Parser<I, E, N, L> for Many1Map<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<Option<O>, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    F: for<'a, 'b> Fn(Many1MapIterator<'a, 'b, P, I, E, N, L>) -> O,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        let pos = i.input.pos();
        let mut err = None;
        let first = match self.0.by_ref().run_once(i.rb()).as_result() {
            Ok(o) => o,
            Err(e) => return i.push_err(pos..i.input.pos(), ManyErr { summary: None, err: e }),
        };
        let it = std::iter::once(first).chain(ManyIterator {
            parser: RefOrMutParser::new_ref(&self.0),
            i: i.rb(),
            is_end: false,
            err: &mut err,
        });
        let summary = (self.1)(it);
        match err {
            None => E::value(summary),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary: Some(summary), err }),
        }
    }
}

/// Repeat a parser (with `OutOf`) one or more times.
///
/// This is the "1+" variant of [`Many`]. If the first parse fails, the error is reported via
/// `ManyErr` and the input is rolled back according to the inner parser's `cut` behavior.
pub struct Many1<P, O>(P, PhantomData<fn() -> O>);

/// Create a [`Many1`] combinator.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "aaab";
/// let out: String = parse_ok_once(&mut input, many1(item('a'))).unwrap();
/// assert_eq!(out, "aaa");
/// assert_eq!(input, "b");
/// ```
pub fn many1<P, O>(parser: P) -> Many1<P, O> {
    Many1(parser, PhantomData)
}

impl<P, I, E, N, L, O> ParserOnce<I, E, N, L> for Many1<P, O>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    O: FromIterator<ValueOf<P, I, E, N, L>>,
{
    type Out = E::Out<O, ManyErr<O, ErrOf<P, I, E, N, L>>>;

    fn run_once(mut self, mut i: In<I, E, N, L>) -> Self::Out {
        let pos = i.input.pos();
        let mut err = None;
        let first = match self.0.by_mut().run_once(i.rb()).as_result() {
            Ok(o) => o,
            Err(e) => {
                let summary: O = std::iter::empty().collect();
                return i.push_err(pos..i.input.pos(), ManyErr { summary, err: e });
            }
        };
        let summary: O = std::iter::once(first).chain(ManyIterator { parser: self.0, i: i.rb(), is_end: false, err: &mut err }).collect();
        match err {
            None => E::value(summary),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary, err }),
        }
    }
}

impl<P, I, E, N, L, O> ParserMut<I, E, N, L> for Many1<P, O>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    O: FromIterator<ValueOf<P, I, E, N, L>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        let pos = i.input.pos();
        let mut err = None;
        let first = match self.0.by_mut().run_once(i.rb()).as_result() {
            Ok(o) => o,
            Err(e) => {
                let summary: O = std::iter::empty().collect();
                return i.push_err(pos..i.input.pos(), ManyErr { summary, err: e });
            }
        };
        let summary: O =
            std::iter::once(first).chain(ManyIterator { parser: self.0.by_mut(), i: i.rb(), is_end: false, err: &mut err }).collect();
        match err {
            None => E::value(summary),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary, err }),
        }
    }
}

impl<P, I, E, N, L, O> Parser<I, E, N, L> for Many1<P, O>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    O: FromIterator<ValueOf<P, I, E, N, L>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        let pos = i.input.pos();
        let mut err = None;
        let first = match self.0.by_ref().run_once(i.rb()).as_result() {
            Ok(o) => o,
            Err(e) => {
                let summary: O = std::iter::empty().collect();
                return i.push_err(pos..i.input.pos(), ManyErr { summary, err: e });
            }
        };
        let summary: O = std::iter::once(first)
            .chain(ManyIterator { parser: RefOrMutParser::new_ref(&self.0), i: i.rb(), is_end: false, err: &mut err })
            .collect();
        match err {
            None => E::value(summary),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary, err }),
        }
    }
}

/// Repeat a parser until it fails (softly), discarding outputs.
pub struct SkipMany<P>(P);

/// Create a [`SkipMany`] combinator.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "aaab";
/// let out = parse_ok_once(&mut input, many_skip(item('a'))).unwrap();
/// assert_eq!(out, ());
/// assert_eq!(input, "b");
/// ```
pub fn many_skip<P>(parser: P) -> SkipMany<P> {
    SkipMany(parser)
}

impl<P, I, E, N, L> ParserOnce<I, E, N, L> for SkipMany<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    type Out = E::Out<(), ErrOf<P, I, E, N, L>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let () = ManyIterator { parser: self.0, i: i.rb(), is_end: false, err: &mut err }.map(|_| ()).collect();
        match err {
            None => E::value(()),
            Some(err) => P::Out::embed_result(Err(err)),
        }
    }
}

impl<P, I, E, N, L> ParserMut<I, E, N, L> for SkipMany<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let () = ManyIterator { parser: self.0.by_mut(), i: i.rb(), is_end: false, err: &mut err }.map(|_| ()).collect();
        match err {
            None => E::value(()),
            Some(err) => P::Out::embed_result(Err(err)),
        }
    }
}

impl<P, I, E, N, L> Parser<I, E, N, L> for SkipMany<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let () = ManyIterator { parser: self.0.by_ref(), i: i.rb(), is_end: false, err: &mut err }.map(|_| ()).collect();
        match err {
            None => E::value(()),
            Some(err) => P::Out::embed_result(Err(err)),
        }
    }
}

/// Repeat a parser one or more times, discarding outputs.
pub struct SkipMany1<P>(P);

/// Create a [`SkipMany1`] combinator.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "aaab";
/// let out = parse_ok_once(&mut input, many1_skip(item('a'))).unwrap();
/// assert_eq!(out, ());
/// assert_eq!(input, "b");
/// ```
pub fn many1_skip<P>(parser: P) -> SkipMany1<P> {
    SkipMany1(parser)
}

impl<P, I, E, N, L> ParserOnce<I, E, N, L> for SkipMany1<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    type Out = E::Out<(), ErrOf<P, I, E, N, L>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        let mut parser = self.0;
        if let Err(e) = parser.run_mut(i.rb()).as_result() {
            return P::Out::embed_result(Err(e));
        }
        let mut err = None;
        let () = ManyIterator { parser, i: i.rb(), is_end: false, err: &mut err }.map(|_| ()).collect();
        match err {
            None => E::value(()),
            Some(err) => P::Out::embed_result(Err(err)),
        }
    }
}

impl<P, I, E, N, L> ParserMut<I, E, N, L> for SkipMany1<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        if let Err(e) = self.0.run_mut(i.rb()).as_result() {
            return P::Out::embed_result(Err(e));
        }
        let mut err = None;
        let () = ManyIterator { parser: self.0.by_mut(), i: i.rb(), is_end: false, err: &mut err }.map(|_| ()).collect();
        match err {
            None => E::value(()),
            Some(err) => P::Out::embed_result(Err(err)),
        }
    }
}

impl<P, I, E, N, L> Parser<I, E, N, L> for SkipMany1<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        if let Err(e) = self.0.run(i.rb()).as_result() {
            return P::Out::embed_result(Err(e));
        }
        let mut err = None;
        let () = ManyIterator { parser: self.0.by_ref(), i: i.rb(), is_end: false, err: &mut err }.map(|_| ()).collect();
        match err {
            None => E::value(()),
            Some(err) => P::Out::embed_result(Err(err)),
        }
    }
}

/// Range specification for [`count`] combinators.
pub trait CountRange {
    fn begin(&self) -> usize;
    fn end(self) -> Option<usize>;
}

impl CountRange for usize {
    fn begin(&self) -> usize {
        *self
    }
    fn end(self) -> Option<usize> {
        Some(self)
    }
}

impl CountRange for std::ops::RangeFull {
    fn begin(&self) -> usize {
        0
    }
    fn end(self) -> Option<usize> {
        None
    }
}

impl CountRange for std::ops::Range<usize> {
    fn begin(&self) -> usize {
        self.start
    }
    fn end(self) -> Option<usize> {
        Some(self.end.saturating_sub(1))
    }
}

impl CountRange for std::ops::RangeInclusive<usize> {
    fn begin(&self) -> usize {
        *self.start()
    }
    fn end(self) -> Option<usize> {
        Some(*std::ops::RangeInclusive::end(&self))
    }
}

impl CountRange for std::ops::RangeFrom<usize> {
    fn begin(&self) -> usize {
        self.start
    }
    fn end(self) -> Option<usize> {
        None
    }
}

impl CountRange for std::ops::RangeTo<usize> {
    fn begin(&self) -> usize {
        0
    }
    fn end(self) -> Option<usize> {
        Some(self.end.saturating_sub(1))
    }
}

impl CountRange for std::ops::RangeToInclusive<usize> {
    fn begin(&self) -> usize {
        0
    }
    fn end(self) -> Option<usize> {
        Some(self.end)
    }
}

/// Repeat a parser a specified number of times.
pub struct Count<P, O> {
    begin: usize,
    end: Option<usize>,
    parser: P,
    _marker: PhantomData<fn() -> O>,
}

/// Create a [`Count`] combinator.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "abcd1234";
/// let hex = one_of("0123456789abcdefABCDEF");
/// let out: String = parse_ok_once(&mut input, count(4, hex)).unwrap();
/// assert_eq!(out, "abcd");
/// assert_eq!(input, "1234");
///
/// // 2 to 4 times (inclusive)
/// let mut input = "aaab";
/// let out: String = parse_ok_once(&mut input, count(2..=4, item('a'))).unwrap();
/// assert_eq!(out, "aaa");
/// assert_eq!(input, "b");
///
/// // At least 2 times
/// let mut input = "aaab";
/// let out: String = parse_ok_once(&mut input, count(2.., item('a'))).unwrap();
/// assert_eq!(out, "aaa");
/// assert_eq!(input, "b");
/// ```
pub fn count<R, P, O>(range: R, parser: P) -> Count<P, O>
where
    R: CountRange,
{
    Count { begin: range.begin(), end: range.end(), parser, _marker: PhantomData }
}

struct CountIterator<'a, P, I: Input, E: ErrMode<I>, N: Rb, L: RbBack>
where
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    input: In<'a, I, E, N, L>,
    parser: P,
    begin: usize,
    count: usize,
    end: Option<usize>,
    err: &'a mut Option<ErrOf<P, I, E, N, L>>,
}

impl<'a, P, T, I, E, N, L> Iterator for CountIterator<'a, P, I, E, N, L>
where
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.err.is_some() {
            return None;
        }
        if matches!(self.end, Some(end) if self.count >= end) {
            return None;
        }
        if self.count < self.begin {
            match self.parser.run_mut(self.input.rb()).as_result() {
                Ok(item) => {
                    self.count += 1;
                    return Some(item);
                }
                Err(e) => {
                    *self.err = Some(e);
                    return None;
                }
            }
        }
        let checkpoint = self.input.checkpoint();
        match <P::Out>::project_result(self.input.maybe(self.parser.by_mut())) {
            Ok(Some(item)) => {
                self.count += 1;
                Some(item)
            }
            Ok(None) => {
                self.input.rollback(checkpoint);
                None
            }
            Err(e) => {
                *self.err = Some(e);
                None
            }
        }
    }
}

impl<P, T, O, I, E, N, L> ParserOnce<I, E, N, L> for Count<P, O>
where
    I: Input,
    E: ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    O: FromIterator<T>,
{
    type Out = E::Out<O, ManyErr<O, ErrOf<P, I, E, N, L>>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        let mut parser = self.parser;
        let pos = i.pos();
        let mut err = None;
        let out: O =
            CountIterator { input: i.rb(), parser: parser.by_mut(), begin: self.begin, count: 0, end: self.end, err: &mut err }.collect();

        match err {
            None => E::value(out),
            Some(err) => i.push_err(pos..i.input.pos(), ManyErr { summary: out, err }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::input::inner::Input as _;
    use crate::{input::In, parser::item, parser::many};

    type TestIn<'a> = In<'a, &'static str, crate::input::error::Merger<&'static str, crate::input::error::std::StdErr<char>>, (), ()>;

    #[test]
    fn many_rolls_back_terminator_attempt() {
        let mut input = "aa!";
        let p = |i: TestIn<'_>| match i.input.next() {
            Some('a') => Some('a'),
            Some('!') => None,
            Some(_) => None,
            None => None,
        };

        let r = crate::parse::parse_once::<_, crate::input::error::std::StdErr<char>, _, _>(&mut input, many::many::<_, String>(p));
        assert_eq!(r.out, Some("aa".to_string()));
        assert_eq!(input, "!");
        assert!(!r.did_cut);
    }

    #[test]
    fn many_returns_err_on_cut_failure() {
        let mut input = "aaX!";
        let p = |mut i: TestIn<'_>| match i.input.next() {
            Some('a') => Some('a'),
            Some('X') => {
                i.cut();
                None
            }
            Some('!') => None,
            Some(_) => None,
            None => None,
        };

        let r = crate::parse::parse_once::<_, crate::input::error::std::StdErr<char>, _, _>(&mut input, many::many::<_, String>(p));
        assert_eq!(r.out, None);
        assert_eq!(input, "!");
        assert!(r.did_cut);
    }

    #[test]
    fn count_range_collects_until_terminator() {
        let mut input = "aaab";
        let r = crate::parse::parse_once::<_, crate::input::error::std::StdErr<char>, _, _>(
            &mut input,
            many::count::<_, _, String>(2..=4, |i: TestIn<'_>| match i.input.next() {
                Some('a') => Some('a'),
                _ => None,
            }),
        );
        assert_eq!(r.out, Some("aaa".to_string()));
        assert_eq!(input, "b");
    }

    #[test]
    fn many1_requires_at_least_one() {
        let mut input = "bbb";
        let r = crate::parse::parse_once::<_, crate::input::error::std::StdErr<char>, _, _>(
            &mut input,
            many::many1::<_, String>(item::item('a')),
        );
        assert_eq!(r.out, None);
    }
}
