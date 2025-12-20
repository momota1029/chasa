//! `ControlFlow`-driven combinators.
//!
//! This module provides looping primitives where the parser is repeatedly executed and the loop
//! is controlled via [`std::ops::ControlFlow`].
//!
//! Most users will use these via the extension methods on [`crate::parser::ParserMut`]:
//! - `flow`
//! - `flow_many`
//! - `flow_many_map`

use ::std::{marker::PhantomData, ops::ControlFlow};

use reborrow_generic::short::Rb;

use crate::{
    Input,
    input::{
        In,
        error::{ErrMode, OutOf},
        inner::RbBack,
    },
    parser::{ErrOf, Parser, ParserMut, ParserOnce, prim::RefOrMutParser},
};

/// Stateful loop that terminates on soft failure via [`In::maybe`].
///
/// - The parser is repeatedly executed with rollback-on-soft-failure.
/// - On soft termination (`Ok(None)`), the loop ends and `end(state)` is returned.
/// - On hard failure (`Err` with cut), the error is propagated.
///
/// Prefer [`crate::parser::ParserMut::flow`].
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
/// use std::ops::ControlFlow;
///
/// let mut input = "aaab";
/// let out = parse_ok_once(
///     &mut input,
///     item('a').flow(0usize, |n, _| ControlFlow::Continue(n + 1), |n| n),
/// )
/// .unwrap();
/// assert_eq!(out, 3);
/// assert_eq!(input, "b");
/// ```
pub struct Flow<P, S, Step, End>(pub P, pub S, pub Step, pub End);

impl<P, S, Step, End> Flow<P, S, Step, End> {
    /// Create a new `Flow` combinator.
    pub fn new(parser: P, state: S, step: Step, end: End) -> Self {
        Self(parser, state, step, end)
    }
}

impl<I, E, N, L, P, S, Step, End, O, T> ParserOnce<I, E, N, L> for Flow<P, S, Step, End>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    Step: FnMut(S, T) -> ControlFlow<O, S>,
    End: FnMut(S) -> O,
{
    type Out = E::Out<O, ErrOf<P, I, E, N, L>>;

    fn run_once(mut self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut state = self.1;
        loop {
            match <P::Out as OutOf<I, E>>::project_result(input.maybe(self.0.by_mut())) {
                Ok(Some(t)) => match (self.2)(state, t) {
                    ControlFlow::Continue(next) => state = next,
                    ControlFlow::Break(out) => return E::value(out),
                },
                Ok(None) => return E::value((self.3)(state)),
                Err(e) => return P::Out::embed_result(Err(e)),
            }
        }
    }
}

impl<I, E, N, L, P, S, Step, End, O, T> ParserMut<I, E, N, L> for Flow<P, S, Step, End>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    S: Clone,
    Step: FnMut(S, T) -> ControlFlow<O, S>,
    End: FnMut(S) -> O,
{
    fn run_mut(&mut self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut state = self.1.clone();
        loop {
            match <P::Out as OutOf<I, E>>::project_result(input.maybe(self.0.by_mut())) {
                Ok(Some(t)) => match (self.2)(state, t) {
                    ControlFlow::Continue(next) => state = next,
                    ControlFlow::Break(out) => return E::value(out),
                },
                Ok(None) => return E::value((self.3)(state)),
                Err(e) => return P::Out::embed_result(Err(e)),
            }
        }
    }
}

impl<I, E, N, L, P, S, Step, End, O, T> Parser<I, E, N, L> for Flow<P, S, Step, End>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    S: Clone,
    Step: Fn(S, T) -> ControlFlow<O, S>,
    End: Fn(S) -> O,
{
    fn run(&self, mut input: In<I, E, N, L>) -> Self::Out {
        let mut state = self.1.clone();
        loop {
            match <P::Out as OutOf<I, E>>::project_result(input.maybe(self.0.by_ref())) {
                Ok(Some(t)) => match (self.2)(state, t) {
                    ControlFlow::Continue(next) => state = next,
                    ControlFlow::Break(out) => return E::value(out),
                },
                Ok(None) => return E::value((self.3)(state)),
                Err(e) => return P::Out::embed_result(Err(e)),
            }
        }
    }
}

/// Iterator that yields `Continue` values and stores the final `Break` value.
pub struct RawFlowManyIterator<'a, P, I: Input, E: ErrMode<I>, N: Rb, L: RbBack, T1, T2> {
    parser: P,
    input: In<'a, I, E, N, L>,
    done: bool,
    brk: Option<T2>,
    _phantom: PhantomData<fn() -> T1>,
}

/// Iterator used by [`FlowManyMap`].
pub type FlowManyIterator<'a, 'b, P, I, E, N, L, T1, T2> = RawFlowManyIterator<'a, RefOrMutParser<'b, P, I, E, N, L>, I, E, N, L, T1, T2>;

impl<'a, P, I, E, N, L, T1, T2> Iterator for RawFlowManyIterator<'a, P, I, E, N, L, T1, T2>
where
    P: ParserMut<I, E, N, L, Out = ControlFlow<T2, T1>>,
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
{
    type Item = T1;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        match self.parser.run_mut(self.input.rb()) {
            ControlFlow::Continue(v) => Some(v),
            ControlFlow::Break(b) => {
                self.done = true;
                self.brk = Some(b);
                None
            }
        }
    }
}

impl<'a, P, I, E, N, L, T1, T2> RawFlowManyIterator<'a, P, I, E, N, L, T1, T2>
where
    P: ParserMut<I, E, N, L, Out = ControlFlow<T2, T1>>,
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
{
    /// Drain the iterator and return the final `Break` value.
    pub fn finish(&mut self) -> T2 {
        while self.next().is_some() {}
        self.brk.take().expect("FlowManyIterator finished without Break")
    }
}

/// Collect `Continue(T1)` values until `Break(T2)`.
///
/// Prefer [`crate::parser::ParserMut::flow_many`].
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
/// use std::ops::ControlFlow;
///
/// let mut input = "ab]";
/// let take_until_bracket = |i: In<&str, Merger<&str, StdErr<char>>, (), ()>| match i.input.next() {
///     Some(']') => ControlFlow::Break(']'),
///     Some(c) => ControlFlow::Continue(c),
///     None => ControlFlow::Break('\0'),
/// };
/// let r = parse_once::<_, StdErr<char>, _, _>(&mut input, take_until_bracket.flow_many::<String, char, char>());
/// assert_eq!(r.out.0, "ab");
/// assert_eq!(r.out.1, ']');
/// assert_eq!(input, "");
/// ```
pub struct FlowMany<P, O>(pub P, PhantomData<fn() -> O>);

impl<P, O> FlowMany<P, O> {
    /// Create a new `FlowMany`.
    pub fn new(parser: P) -> Self {
        Self(parser, PhantomData)
    }
}

impl<I, E, N, L, P, O, T1, T2> ParserOnce<I, E, N, L> for FlowMany<P, O>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out = ControlFlow<T2, T1>>,
    O: FromIterator<T1>,
{
    type Out = (O, T2);

    fn run_once(self, input: In<I, E, N, L>) -> Self::Out {
        let mut it = RawFlowManyIterator { parser: self.0, input, done: false, brk: None, _phantom: PhantomData };
        let o: O = (&mut it).collect();
        (o, it.finish())
    }
}

impl<I, E, N, L, P, O, T1, T2> ParserMut<I, E, N, L> for FlowMany<P, O>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out = ControlFlow<T2, T1>>,
    O: FromIterator<T1>,
{
    fn run_mut(&mut self, input: In<I, E, N, L>) -> Self::Out {
        let mut it =
            RawFlowManyIterator { parser: RefOrMutParser::new_mut(&mut self.0), input, done: false, brk: None, _phantom: PhantomData };
        let o: O = (&mut it).collect();
        (o, it.finish())
    }
}

impl<I, E, N, L, P, O, T1, T2> Parser<I, E, N, L> for FlowMany<P, O>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out = ControlFlow<T2, T1>>,
    O: FromIterator<T1>,
{
    fn run(&self, input: In<I, E, N, L>) -> Self::Out {
        let mut it = RawFlowManyIterator { parser: RefOrMutParser::new_ref(&self.0), input, done: false, brk: None, _phantom: PhantomData };
        let o: O = (&mut it).collect();
        (o, it.finish())
    }
}

/// Like [`FlowMany`], but lets you fold via a [`FlowManyIterator`].
///
/// Prefer [`crate::parser::Parser::flow_many_map`].
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
/// use std::ops::ControlFlow;
///
/// let mut input = "ab]";
/// let take_until_bracket = |i: In<&str, Merger<&str, StdErr<char>>, (), ()>| match i.input.next() {
///     Some(']') => ControlFlow::Break(']'),
///     Some(c) => ControlFlow::Continue(c),
///     None => ControlFlow::Break('\0'),
/// };
/// let r = parse_once::<_, StdErr<char>, _, _>(&mut input, take_until_bracket.flow_many_map(|it| it.count()));
/// assert_eq!(r.out.0, 2);
/// assert_eq!(r.out.1, ']');
/// assert_eq!(input, "");
/// ```
pub struct FlowManyMap<P, F>(pub P, pub F);

impl<P, F> FlowManyMap<P, F> {
    /// Create a new `FlowManyMap`.
    pub fn new(parser: P, f: F) -> Self {
        Self(parser, f)
    }
}

impl<I, E, N, L, P, F, O, T1, T2> ParserOnce<I, E, N, L> for FlowManyMap<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out = ControlFlow<T2, T1>>,
    F: for<'a, 'b> FnOnce(&mut FlowManyIterator<'a, 'b, P, I, E, N, L, T1, T2>) -> O,
{
    type Out = (O, T2);

    fn run_once(self, input: In<I, E, N, L>) -> Self::Out {
        let FlowManyMap(mut parser, f) = self;
        let mut it =
            RawFlowManyIterator { parser: RefOrMutParser::new_mut(&mut parser), input, done: false, brk: None, _phantom: PhantomData };
        let o = f(&mut it);
        let t2 = it.finish();
        (o, t2)
    }
}

impl<I, E, N, L, P, F, O, T1, T2> ParserMut<I, E, N, L> for FlowManyMap<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out = ControlFlow<T2, T1>>,
    F: for<'a, 'b> FnMut(&mut FlowManyIterator<'a, 'b, P, I, E, N, L, T1, T2>) -> O,
{
    fn run_mut(&mut self, input: In<I, E, N, L>) -> Self::Out {
        let mut it =
            RawFlowManyIterator { parser: RefOrMutParser::new_mut(&mut self.0), input, done: false, brk: None, _phantom: PhantomData };
        let o = (self.1)(&mut it);
        let t2 = it.finish();
        (o, t2)
    }
}

impl<I, E, N, L, P, F, O, T1, T2> Parser<I, E, N, L> for FlowManyMap<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out = ControlFlow<T2, T1>>,
    F: for<'a, 'b> Fn(&mut FlowManyIterator<'a, 'b, P, I, E, N, L, T1, T2>) -> O,
{
    fn run(&self, input: In<I, E, N, L>) -> Self::Out {
        let mut it = RawFlowManyIterator { parser: RefOrMutParser::new_ref(&self.0), input, done: false, brk: None, _phantom: PhantomData };
        let o = (self.1)(&mut it);
        let t2 = it.finish();
        (o, t2)
    }
}
