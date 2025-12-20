//! `sep`-family combinators for `OutOf` parsers.
//!
//! This module implements "separated lists" (items separated by a separator parser).
//!
//! Semantics are intentionally explicit and based on the same rules as `many`:
//!
//! - the separator is tested in a rollback-aware way so a missing separator ends the list with
//!   rollback of the separator attempt.
//! - whether a failure is soft/hard is determined by `cut` observed inside the attempt scope
//!   (via `maybe`/`capture_cut`), not by "how much input moved".
//!
//! Trailing separator handling is controlled by a mode:
//!
//! - `Allow` (default): a final separator may appear and is consumed.
//! - `No`: a separator must always be followed by an item.
//! - `Must`: the list must end with a separator.
//!
//! The current crate uses `cut` as a branch-pruning marker. If your item parser calls `cut` on
//! success, then once an item matches, later mismatches usually become hard failures with respect
//! to `choice`.

use std::marker::PhantomData;

use reborrow_generic::short::Rb;

use crate::{
    Input,
    input::{
        In,
        error::{ErrMode, OutOf},
        inner::RbBack,
    },
    parser::many::ManyErr,
    parser::{ErrOf, ParserMut, ValueOf, prim::RefOrMutParser},
};

pub mod iter;

use iter::{Count, Trailling};

/// Iterator driving separated-list parsing.
pub struct SepIterator<
    'a,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    Q,
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
> {
    pub(crate) is_first: bool,
    pub(crate) item: P,
    pub(crate) sep: Q,
    pub(crate) input: In<'a, I, E, N, L>,
    pub(crate) err: &'a mut Option<ErrOf<P, I, E, N, L>>,
    pub(crate) did_trail: &'a mut bool,
    pub(crate) is_end: bool,
    pub(crate) _mode: PhantomData<fn() -> (Co, Tr)>,
}

/// Borrowed iterator form (accepts `&P` / `&mut P` via [`RefOrMutParser`]).
pub type SepMapIterator<'a, 'b, P, Q, I, E, N, L, Co, Tr> =
    SepIterator<'a, RefOrMutParser<'b, P, I, E, N, L>, RefOrMutParser<'b, Q, I, E, N, L>, I, E, N, L, Co, Tr>;

impl<'a, P, Q, I, E, N, L, Co, Tr> Iterator for SepIterator<'a, P, Q, I, E, N, L, Co, Tr>
where
    I: Input,
    E: crate::input::error::ErrMode<I>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: ParserMut<I, E, N, L, Out: crate::input::error::OutOf<I, E>>,
    Q: ParserMut<I, E, N, L, Out: crate::input::error::OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    type Item = ValueOf<P, I, E, N, L>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_end || self.err.is_some() {
            return None;
        }

        if self.is_first {
            self.is_first = false;
            match Co::first(&mut self.input, &mut self.item) {
                Ok(Some(item)) => return Some(item),
                Ok(None) => {
                    self.is_end = true;
                    return None;
                }
                Err(e) => {
                    *self.err = Some(e);
                    self.is_end = true;
                    return None;
                }
            }
        }

        match Tr::second(&mut self.input, &mut self.sep, &mut self.item, &mut *self.did_trail) {
            Ok(Some(item)) => Some(item),
            Ok(None) => {
                self.is_end = true;
                None
            }
            Err(e) => {
                *self.err = Some(e);
                self.is_end = true;
                None
            }
        }
    }
}

impl<'a, P, Q, I, E, N, L, Co, Tr> SepIterator<'a, P, Q, I, E, N, L, Co, Tr>
where
    I: Input,
    E: crate::input::error::ErrMode<I>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: ParserMut<I, E, N, L, Out: crate::input::error::OutOf<I, E>>,
    Q: ParserMut<I, E, N, L, Out: crate::input::error::OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    /// Require at least one item in this iterator.
    pub fn one(self) -> SepIterator<'a, P, Q, I, E, N, L, iter::One, Tr> {
        SepIterator {
            is_first: self.is_first,
            item: self.item,
            sep: self.sep,
            input: self.input,
            err: self.err,
            did_trail: self.did_trail,
            is_end: self.is_end,
            _mode: PhantomData,
        }
    }

    /// Disallow a trailing separator in this iterator.
    pub fn no_trail(self) -> SepIterator<'a, P, Q, I, E, N, L, Co, iter::No> {
        SepIterator {
            is_first: self.is_first,
            item: self.item,
            sep: self.sep,
            input: self.input,
            err: self.err,
            did_trail: self.did_trail,
            is_end: self.is_end,
            _mode: PhantomData,
        }
    }

    /// Require a trailing separator in this iterator.
    pub fn must_trail(self) -> SepIterator<'a, P, Q, I, E, N, L, Co, iter::Must> {
        SepIterator {
            is_first: self.is_first,
            item: self.item,
            sep: self.sep,
            input: self.input,
            err: self.err,
            did_trail: self.did_trail,
            is_end: self.is_end,
            _mode: PhantomData,
        }
    }
}

/// Separated list combinator.
#[derive(Clone, Copy, Debug, Hash)]
pub struct Sep<O, Co, Tr, P, Q, T> {
    pub(crate) item: P,
    pub(crate) sep: Q,
    pub(crate) _marker: PhantomData<fn() -> (O, Co, Tr, T)>,
}

/// Create a separated-list parser with default modes (`Zero` + `Allow`).
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a,a,";
/// let comma = item(',').to(());
/// let out: String = parse_ok_once(&mut input, sep(item('a'), comma)).unwrap();
/// assert_eq!(out, "aa");
/// assert_eq!(input, "");
/// ```
pub fn sep<O, P, Q, T>(item: P, sep: Q) -> Sep<O, iter::Zero, iter::Allow, P, Q, T> {
    Sep { item, sep, _marker: PhantomData }
}

/// Like [`sep`], but requires at least one item (`1+`).
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a,a";
/// let comma = item(',').to(());
/// let out: String = parse_ok_once(&mut input, sep1(item('a'), comma)).unwrap();
/// assert_eq!(out, "aa");
/// assert_eq!(input, "");
/// ```
pub fn sep1<O, P, Q, T>(item: P, sep: Q) -> Sep<O, iter::One, iter::Allow, P, Q, T> {
    Sep { item, sep, _marker: PhantomData }
}

impl<O, Co, Tr, P, Q, T> Sep<O, Co, Tr, P, Q, T>
where
    Co: Count,
    Tr: Trailling,
{
    /// Require at least one item.
    pub fn one(self) -> Sep<O, iter::One, Tr, P, Q, T> {
        Sep { item: self.item, sep: self.sep, _marker: PhantomData }
    }

    /// Disallow trailing separators.
    pub fn no_trail(self) -> Sep<O, Co, iter::No, P, Q, T> {
        Sep { item: self.item, sep: self.sep, _marker: PhantomData }
    }

    /// Require a trailing separator.
    pub fn must_trail(self) -> Sep<O, Co, iter::Must, P, Q, T> {
        Sep { item: self.item, sep: self.sep, _marker: PhantomData }
    }

    /// Preserve the trailing-separator flag in the output.
    pub fn use_trail(self) -> UseTrailSep<O, Co, Tr, P, Q, T> {
        UseTrailSep(self)
    }
}

/// Wrapper that returns `(output, did_trail)`.
#[derive(Clone, Copy, Debug, Hash)]
pub struct UseTrailSep<O, Co, Tr, P, Q, T>(Sep<O, Co, Tr, P, Q, T>);

impl<O, Co, Tr, P, Q, T, I, E, N, L, S> crate::parser::ParserOnce<I, E, N, L> for UseTrailSep<O, Co, Tr, P, Q, T>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    O: FromIterator<T>,
{
    type Out = E::Out<(O, bool), ManyErr<O, ErrOf<P, I, E, N, L>>>;

    fn run_once(self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let mut did_trail = false;
        let pos = input.pos();
        let out: O = SepIterator {
            is_first: true,
            item: self.0.item,
            sep: self.0.sep,
            input: input.rb(),
            err: &mut err,
            did_trail: &mut did_trail,
            is_end: false,
            _mode: PhantomData::<fn() -> (Co, Tr)>,
        }
        .collect();

        match err {
            None => E::value((out, did_trail)),
            Some(err) => input.push_err(pos..input.pos(), ManyErr { summary: out, err }),
        }
    }
}

impl<O, Co, Tr, P, Q, T, I, E, N, L, S> crate::parser::ParserMut<I, E, N, L> for UseTrailSep<O, Co, Tr, P, Q, T>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: crate::parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: crate::parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    O: FromIterator<T>,
{
    fn run_mut(&mut self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let mut did_trail = false;
        let pos = input.pos();
        let out: O = SepIterator {
            is_first: true,
            item: self.0.item.by_mut(),
            sep: self.0.sep.by_mut(),
            input: input.rb(),
            err: &mut err,
            did_trail: &mut did_trail,
            is_end: false,
            _mode: PhantomData::<fn() -> (Co, Tr)>,
        }
        .collect();

        match err {
            None => E::value((out, did_trail)),
            Some(err) => input.push_err(pos..input.pos(), ManyErr { summary: out, err }),
        }
    }
}

impl<O, Co, Tr, P, Q, T, I, E, N, L, S> crate::parser::Parser<I, E, N, L> for UseTrailSep<O, Co, Tr, P, Q, T>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: crate::parser::Parser<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: crate::parser::Parser<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    O: FromIterator<T>,
{
    fn run(&self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let mut did_trail = false;
        let pos = input.pos();
        let out: O = SepIterator {
            is_first: true,
            item: self.0.item.by_ref(),
            sep: self.0.sep.by_ref(),
            input: input.rb(),
            err: &mut err,
            did_trail: &mut did_trail,
            is_end: false,
            _mode: PhantomData::<fn() -> (Co, Tr)>,
        }
        .collect();

        match err {
            None => E::value((out, did_trail)),
            Some(err) => input.push_err(pos..input.pos(), ManyErr { summary: out, err }),
        }
    }
}

/// Left-associative separator reduction: `term (op term)*`.
#[derive(Clone, Copy, Debug, Hash)]
pub struct SepReduce<P, Q, F>(pub(crate) P, pub(crate) Q, pub(crate) F);

/// Create a left-associative reducer for `OutOf` parsers.
///
/// The operator is optional: if it is not present (soft failure), parsing stops and the current
/// accumulator is returned.
///
/// If an operator is present but the following term fails, the whole parser fails without
/// rollback of the operator attempt (i.e. a trailing operator is an error).
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a+a+a";
/// let term = item('a').to(1);
/// let plus = item('+').to(());
/// let out = parse_ok_once(&mut input, sep_reduce(term, plus, |a, _, b| a + b)).unwrap();
/// assert_eq!(out, 3);
/// assert_eq!(input, "");
/// ```
pub fn sep_reduce<P, Q, F>(term: P, op: Q, f: F) -> SepReduce<P, Q, F> {
    SepReduce(term, op, f)
}

impl<O, Co, Tr, P, Q, T, I, E, N, L, S> crate::parser::ParserOnce<I, E, N, L> for Sep<O, Co, Tr, P, Q, T>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    O: FromIterator<T>,
{
    type Out = E::Out<O, ManyErr<O, ErrOf<P, I, E, N, L>>>;

    fn run_once(self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let mut did_trail = false;
        let pos = input.pos();
        let out: O = SepIterator {
            is_first: true,
            item: self.item,
            sep: self.sep,
            input: input.rb(),
            err: &mut err,
            did_trail: &mut did_trail,
            is_end: false,
            _mode: PhantomData::<fn() -> (Co, Tr)>,
        }
        .collect();

        match err {
            None => E::value(out),
            Some(err) => input.push_err(pos..input.pos(), ManyErr { summary: out, err }),
        }
    }
}

impl<P, Q, F, I, E, N, L, T, Op> crate::parser::ParserOnce<I, E, N, L> for SepReduce<P, Q, F>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<Option<T>, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = Op, Error = ErrOf<P, I, E, N, L>>>,
    F: FnMut(T, Op, T) -> T,
{
    type Out = E::Out<T, ManyErr<Option<T>, ErrOf<P, I, E, N, L>>>;

    fn run_once(mut self, mut input: In<I, E, N, L>) -> Self::Out {
        let pos = input.pos();
        let mut acc = match self.0.run_mut(input.rb()).as_result() {
            Ok(v) => v,
            Err(e) => return input.push_err(pos..input.pos(), ManyErr { summary: None, err: e }),
        };
        loop {
            let op = match <Q::Out as OutOf<I, E>>::project_result(input.maybe(self.1.by_mut())) {
                Ok(Some(op)) => op,
                Ok(None) => break,
                Err(e) => return input.push_err(pos..input.pos(), ManyErr { summary: Some(acc), err: e }),
            };
            let rhs = match self.0.run_mut(input.rb()).as_result() {
                Ok(v) => v,
                Err(e) => return input.push_err(pos..input.pos(), ManyErr { summary: Some(acc), err: e }),
            };
            acc = (self.2)(acc, op, rhs);
        }
        E::value(acc)
    }
}

impl<P, Q, F, I, E, N, L, T, Op> crate::parser::ParserMut<I, E, N, L> for SepReduce<P, Q, F>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<Option<T>, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = Op, Error = ErrOf<P, I, E, N, L>>>,
    F: FnMut(T, Op, T) -> T,
{
    fn run_mut(&mut self, mut input: In<I, E, N, L>) -> Self::Out {
        let pos = input.pos();
        let mut acc = match self.0.run_mut(input.rb()).as_result() {
            Ok(v) => v,
            Err(e) => return input.push_err(pos..input.pos(), ManyErr { summary: None, err: e }),
        };
        loop {
            let op = match <Q::Out as OutOf<I, E>>::project_result(input.maybe(self.1.by_mut())) {
                Ok(Some(op)) => op,
                Ok(None) => break,
                Err(e) => return input.push_err(pos..input.pos(), ManyErr { summary: Some(acc), err: e }),
            };
            let rhs = match self.0.run_mut(input.rb()).as_result() {
                Ok(v) => v,
                Err(e) => return input.push_err(pos..input.pos(), ManyErr { summary: Some(acc), err: e }),
            };
            acc = (self.2)(acc, op, rhs);
        }
        E::value(acc)
    }
}

impl<P, Q, F, I, E, N, L, T, Op> crate::parser::Parser<I, E, N, L> for SepReduce<P, Q, F>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<Option<T>, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: crate::parser::Parser<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: crate::parser::Parser<I, E, N, L, Out: OutOf<I, E, Value = Op, Error = ErrOf<P, I, E, N, L>>>,
    F: Fn(T, Op, T) -> T,
{
    fn run(&self, mut input: In<I, E, N, L>) -> Self::Out {
        let pos = input.pos();
        let mut acc = match self.0.run(input.rb()).as_result() {
            Ok(v) => v,
            Err(e) => return input.push_err(pos..input.pos(), ManyErr { summary: None, err: e }),
        };
        loop {
            let op = match <Q::Out as OutOf<I, E>>::project_result(input.maybe(self.1.by_ref())) {
                Ok(Some(op)) => op,
                Ok(None) => break,
                Err(e) => return input.push_err(pos..input.pos(), ManyErr { summary: Some(acc), err: e }),
            };
            let rhs = match self.0.run(input.rb()).as_result() {
                Ok(v) => v,
                Err(e) => return input.push_err(pos..input.pos(), ManyErr { summary: Some(acc), err: e }),
            };
            acc = (self.2)(acc, op, rhs);
        }
        E::value(acc)
    }
}

/// Map over a separated-list iterator.
#[derive(Clone, Copy, Debug, Hash)]
pub struct SepMap<Co, Tr, P, Q, F, T> {
    pub(crate) item: P,
    pub(crate) sep: Q,
    pub(crate) f: F,
    pub(crate) _marker: PhantomData<fn() -> (Co, Tr, T)>,
}

/// Create a [`SepMap`] with default modes (`Zero` + `Allow`).
///
/// ## Examples
///
/// Use `it.no_trail()` / `it.must_trail()` to control whether a trailing separator is allowed.
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a,a";
/// let comma = item(',').to(());
/// let out = parse_ok_once(&mut input, sep_map(item('a'), comma, |it| it.no_trail().count())).unwrap();
/// assert_eq!(out, 2);
/// assert_eq!(input, "");
/// ```
pub fn sep_map<P, Q, I, E, N, L, F, T, S, O>(item: P, sep: Q, f: F) -> SepMap<iter::Zero, iter::Allow, P, Q, F, T>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    F: for<'a, 'b> FnOnce(SepMapIterator<'a, 'b, P, Q, I, E, N, L, iter::Zero, iter::Allow>) -> O,
{
    SepMap { item, sep, f, _marker: PhantomData }
}

/// Like [`sep_map`], but requires at least one item (`1+`).
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a,a";
/// let comma = item(',').to(());
/// let out = parse_ok_once(&mut input, sep1_map(item('a'), comma, |it| it.no_trail().count())).unwrap();
/// assert_eq!(out, 2);
/// assert_eq!(input, "");
/// ```
pub fn sep1_map<P, Q, I, E, N, L, F, T, S, O>(item: P, sep: Q, f: F) -> SepMap<iter::One, iter::Allow, P, Q, F, T>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    F: for<'a, 'b> FnOnce(SepMapIterator<'a, 'b, P, Q, I, E, N, L, iter::One, iter::Allow>) -> O,
{
    SepMap { item, sep, f, _marker: PhantomData }
}

/// Like [`sep_map`], but intended for single-shot mappers (`FnOnce`).
///
/// This is mainly an ergonomic alias (mirrors upstream APIs that split `sep_map*` by closure kind).
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a,a";
/// let comma = item(',').to(());
/// let out = parse_ok_once(&mut input, sep_map_once(item('a'), comma, |it| it.no_trail().count())).unwrap();
/// assert_eq!(out, 2);
/// assert_eq!(input, "");
/// ```
pub fn sep_map_once<P, Q, I, E, N, L, F, T, S, O>(item: P, sep: Q, f: F) -> SepMap<iter::Zero, iter::Allow, P, Q, F, T>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    F: for<'a, 'b> FnOnce(SepMapIterator<'a, 'b, P, Q, I, E, N, L, iter::Zero, iter::Allow>) -> O,
{
    SepMap { item, sep, f, _marker: PhantomData }
}

/// Like [`sep_map`], but intended for mutable/repeatable mappers (`FnMut`).
///
/// This is mainly an ergonomic alias (mirrors upstream APIs that split `sep_map*` by closure kind).
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a,a";
/// let comma = item(',').to(());
/// let out = parse_ok_once(&mut input, sep_map_mut(item('a'), comma, |it| it.no_trail().count())).unwrap();
/// assert_eq!(out, 2);
/// assert_eq!(input, "");
/// ```
pub fn sep_map_mut<P, Q, I, E, N, L, F, T, S, O>(item: P, sep: Q, f: F) -> SepMap<iter::Zero, iter::Allow, P, Q, F, T>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    F: for<'a, 'b> FnMut(SepMapIterator<'a, 'b, P, Q, I, E, N, L, iter::Zero, iter::Allow>) -> O,
{
    SepMap { item, sep, f, _marker: PhantomData }
}

impl<Co, Tr, P, Q, F, I, E, N, L, T, S, O> crate::parser::ParserOnce<I, E, N, L> for SepMap<Co, Tr, P, Q, F, T>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    F: FnOnce(SepMapIterator<P, Q, I, E, N, L, Co, Tr>) -> O,
{
    type Out = E::Out<O, ManyErr<O, ErrOf<P, I, E, N, L>>>;

    fn run_once(mut self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let mut did_trail = false;
        let pos = input.pos();
        let out = (self.f)(SepIterator {
            is_first: true,
            item: RefOrMutParser::new_mut(&mut self.item),
            sep: RefOrMutParser::new_mut(&mut self.sep),
            input: input.rb(),
            err: &mut err,
            did_trail: &mut did_trail,
            is_end: false,
            _mode: PhantomData::<fn() -> (Co, Tr)>,
        });
        match err {
            None => E::value(out),
            Some(err) => input.push_err(pos..input.pos(), ManyErr { summary: out, err }),
        }
    }
}

impl<Co, Tr, P, Q, F, I, E, N, L, T, S, O> crate::parser::ParserMut<I, E, N, L> for SepMap<Co, Tr, P, Q, F, T>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: crate::parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: crate::parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    F: FnMut(SepMapIterator<P, Q, I, E, N, L, Co, Tr>) -> O,
{
    fn run_mut(&mut self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let mut did_trail = false;
        let pos = input.pos();
        let out = (self.f)(SepIterator {
            is_first: true,
            item: RefOrMutParser::new_mut(&mut self.item),
            sep: RefOrMutParser::new_mut(&mut self.sep),
            input: input.rb(),
            err: &mut err,
            did_trail: &mut did_trail,
            is_end: false,
            _mode: PhantomData::<fn() -> (Co, Tr)>,
        });
        match err {
            None => E::value(out),
            Some(err) => input.push_err(pos..input.pos(), ManyErr { summary: out, err }),
        }
    }
}

impl<Co, Tr, P, Q, F, I, E, N, L, T, S, O> crate::parser::Parser<I, E, N, L> for SepMap<Co, Tr, P, Q, F, T>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: crate::parser::Parser<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: crate::parser::Parser<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    F: Fn(SepMapIterator<P, Q, I, E, N, L, Co, Tr>) -> O,
{
    fn run(&self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let mut did_trail = false;
        let pos = input.pos();
        let out = (self.f)(SepIterator {
            is_first: true,
            item: RefOrMutParser::new_ref(&self.item),
            sep: RefOrMutParser::new_ref(&self.sep),
            input: input.rb(),
            err: &mut err,
            did_trail: &mut did_trail,
            is_end: false,
            _mode: PhantomData::<fn() -> (Co, Tr)>,
        });
        match err {
            None => E::value(out),
            Some(err) => input.push_err(pos..input.pos(), ManyErr { summary: out, err }),
        }
    }
}

impl<O, Co, Tr, P, Q, T, I, E, N, L, S> crate::parser::ParserMut<I, E, N, L> for Sep<O, Co, Tr, P, Q, T>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: crate::parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: crate::parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    O: FromIterator<T>,
{
    fn run_mut(&mut self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let mut did_trail = false;
        let pos = input.pos();
        let out: O = SepIterator {
            is_first: true,
            item: self.item.by_mut(),
            sep: self.sep.by_mut(),
            input: input.rb(),
            err: &mut err,
            did_trail: &mut did_trail,
            is_end: false,
            _mode: PhantomData::<fn() -> (Co, Tr)>,
        }
        .collect();
        match err {
            None => E::value(out),
            Some(err) => input.push_err(pos..input.pos(), ManyErr { summary: out, err }),
        }
    }
}

impl<O, Co, Tr, P, Q, T, I, E, N, L, S> crate::parser::Parser<I, E, N, L> for Sep<O, Co, Tr, P, Q, T>
where
    I: Input,
    E: ErrMode<I> + crate::input::error::ErrSink<I, ManyErr<O, ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    Co: Count,
    Tr: Trailling,
    P: crate::parser::Parser<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Q: crate::parser::Parser<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    O: FromIterator<T>,
{
    fn run(&self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut err = None;
        let mut did_trail = false;
        let pos = input.pos();
        let out: O = SepIterator {
            is_first: true,
            item: self.item.by_ref(),
            sep: self.sep.by_ref(),
            input: input.rb(),
            err: &mut err,
            did_trail: &mut did_trail,
            is_end: false,
            _mode: PhantomData::<fn() -> (Co, Tr)>,
        }
        .collect();
        match err {
            None => E::value(out),
            Some(err) => input.push_err(pos..input.pos(), ManyErr { summary: out, err }),
        }
    }
}


