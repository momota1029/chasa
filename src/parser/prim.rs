//! Rollback policy (important):
//!
//! - Rollback happens only in combinators that explicitly do it (`maybe`, `choice`, `many`, `sep`, etc.).
//! - Other combinators do not roll back on failure, even when the output carries an `Err`.
//!   This preserves error start points as part of the observable output.
//!
//! This module also provides:
//! - Blanket impls so closures can act as parsers (`FnOnce`/`FnMut`/`Fn`).
//! - Borrow adapters [`RefParser`] / [`MutParser`] for `by_ref()` / `by_mut()`.
//! - Low-level wrappers used by convenience methods in [`crate::parser`].
//!
//! Important: `cut` is not "consumed input". It is a branch-pruning marker. Rollback decisions
//! are made by combinators (e.g. `choice`, `maybe`), not by the output type.

use reborrow_generic::Reborrow;
use reborrow_generic::short::Rb;

use std::{borrow::Cow, marker::PhantomData};

use crate::{
    Back, Input,
    input::{
        In,
        error::{ErrMode, ErrSink, OutOf, std::Expected},
        inner::RbBack,
    },
    parser::{ErrOf, Parser, ParserMut, ParserOnce, ValueOf},
};

impl<F, I: Input, E: ErrMode<I>, N: Rb, L: RbBack, O> ParserOnce<I, E, N, L> for F
where
    F: FnOnce(In<I, E, N, L>) -> O,
{
    type Out = O;
    fn run_once(self, i: In<I, E, N, L>) -> Self::Out {
        self(i)
    }
}
impl<F, I: Input, E: ErrMode<I>, N: Rb, L: RbBack, O> ParserMut<I, E, N, L> for F
where
    F: FnMut(In<I, E, N, L>) -> O,
{
    fn run_mut(&mut self, i: In<I, E, N, L>) -> Self::Out {
        self(i)
    }
}
impl<F, I: Input, E: ErrMode<I>, N: Rb, L: RbBack, O> Parser<I, E, N, L> for F
where
    F: Fn(In<I, E, N, L>) -> O,
{
    fn run(&self, i: In<I, E, N, L>) -> Self::Out {
        self(i)
    }
}

/// Borrow adapter that turns `&P` into a `ParserOnce`/`ParserMut`/`Parser`.
///
/// Constructed by [`crate::parser::Parser::by_ref`].
pub struct RefParser<'a, P>(pub(crate) &'a P);
impl<P: Parser<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> ParserOnce<I, E, N, L> for RefParser<'_, P> {
    type Out = P::Out;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        self.0.run(i.rb())
    }
}
impl<P: Parser<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> ParserMut<I, E, N, L> for RefParser<'_, P> {
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        self.0.run(i.rb())
    }
}
impl<P: Parser<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> Parser<I, E, N, L> for RefParser<'_, P> {
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        self.0.run(i.rb())
    }
}

/// Borrow adapter that turns `&mut P` into a `ParserOnce`/`ParserMut`.
///
/// Constructed by [`crate::parser::ParserMut::by_mut`].
pub struct MutParser<'a, P>(pub(crate) &'a mut P);
impl<P: ParserMut<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> ParserOnce<I, E, N, L> for MutParser<'_, P> {
    type Out = P::Out;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        self.0.run_mut(i.rb())
    }
}
impl<P: ParserMut<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> ParserMut<I, E, N, L> for MutParser<'_, P> {
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        self.0.run_mut(i.rb())
    }
}

/// Adapter that can hold either `&P` or `&mut P` and still behave like a `ParserMut`.
///
/// This is used by iterator-based combinators (e.g. `many_map`) so the same iterator type can be
/// created from both `&P` and `&mut P`.
pub struct RefOrMutParser<'a, P: ParserMut<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> {
    inner: RefOrMutParserInner<'a, P, I, E, N, L>,
}
impl<'a, P: ParserMut<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> RefOrMutParser<'a, P, I, E, N, L> {
    /// Create a mutable-backed adapter.
    pub fn new_mut(parser: &'a mut P) -> Self {
        Self { inner: RefOrMutParserInner::Mut(parser) }
    }
}
impl<'a, P: Parser<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> RefOrMutParser<'a, P, I, E, N, L> {
    /// Create a shared-backed adapter.
    pub fn new_ref(parser: &'a P) -> Self {
        Self { inner: RefOrMutParserInner::Ref(parser, |p, i| p.run(i)) }
    }
}

enum RefOrMutParserInner<'a, P: ParserMut<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> {
    Ref(&'a P, fn(&P, In<I, E, N, L>) -> P::Out),
    Mut(&'a mut P),
}

impl<'a, P: ParserMut<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> ParserOnce<I, E, N, L> for RefOrMutParser<'a, P, I, E, N, L> {
    type Out = P::Out;
    fn run_once(self, input: In<I, E, N, L>) -> Self::Out {
        match self.inner {
            RefOrMutParserInner::Ref(p, f) => f(p, input),
            RefOrMutParserInner::Mut(p) => p.run_mut(input),
        }
    }
}
impl<'a, P: ParserMut<I, E, N, L>, I: Input, E: ErrMode<I>, N: Rb, L: RbBack> ParserMut<I, E, N, L> for RefOrMutParser<'a, P, I, E, N, L> {
    fn run_mut(&mut self, input: In<I, E, N, L>) -> Self::Out {
        match &mut self.inner {
            RefOrMutParserInner::Ref(p, f) => f(p, input),
            RefOrMutParserInner::Mut(p) => p.run_mut(input),
        }
    }
}

/// Optionalized parser wrapper used by `maybe`.
pub struct OrNot<P>(P);
/// Optionalize a parser with `OutOf` semantics.
///
/// - On success: returns `Ok(Some(value))` (embedded in `E::Out`).
/// - On soft failure (no cut): returns `Ok(None)` and rolls back only when the error mode requests it
///   (e.g. `Merger` rolls back; direct `()` keeps the input position).
/// - On cut failure: propagates `Err` (embedded in `E::Out`).
///
/// This is the wrapper used by `ParserOnce::or_not`.
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "b";
/// let out = parse_ok_once(&mut input, maybe(item('a'))).unwrap();
/// assert_eq!(out, None);
/// assert_eq!(input, "b"); // direct mode keeps position
/// ```
pub fn maybe<P>(parser: P) -> OrNot<P> {
    OrNot(parser)
}

/// A parser that only performs `cut` and succeeds.
///
/// This is useful as a sequencing marker:
/// - `p.left(cut)` to commit after `p` succeeds
/// - `p.right(cut).right(q)` to prevent trying other branches after `p` matched
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "";
/// let out = parse_ok_once(&mut input, cut).unwrap();
/// assert_eq!(out, ());
/// assert_eq!(input, "");
/// ```
pub fn cut<I: Input, E: ErrMode<I>, N: Rb, L: RbBack>(mut input: In<I, E, N, L>) -> E::Out<(), ()> {
    input.cut();
    E::value(())
}

/// Commit-on-success wrapper for `OutOf` parsers.
///
/// This wrapper calls [`crate::input::In::cut`] only when the wrapped parser succeeds.
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let p = choice((
///     item('a').cut().right(item('b')),
///     item('a').right(item('c')),
/// ));
/// assert!(parse_ok_once(&mut input, p).is_ok());
/// ```
pub fn cut_if_ok<P>(parser: P) -> CutIfOk<P> {
    CutIfOk(parser)
}

/// Wrapper that applies `cut` only when the parser succeeds.
#[derive(Clone, Copy, Debug, Hash)]
pub struct CutIfOk<P>(pub(crate) P);

impl<P, I, E, N, L> ParserOnce<I, E, N, L> for CutIfOk<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
{
    type Out = P::Out;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        i.cut_if_ok(self.0)
    }
}

impl<P, I, E, N, L> ParserMut<I, E, N, L> for CutIfOk<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        i.cut_if_ok(self.0.by_mut())
    }
}

impl<P, I, E, N, L> Parser<I, E, N, L> for CutIfOk<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        i.cut_if_ok(self.0.by_ref())
    }
}

/// Lookahead for any parser (OutOf-based).
///
/// - On success (`as_result() -> Ok`): input is rolled back.
/// - On failure (`as_result() -> Err`): input may be consumed.
/// - `cut` is propagated as a branch-pruning signal.
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let out = parse_ok_once(&mut input, lookahead(item('a'))).unwrap();
/// assert_eq!(out, 'a');
/// assert_eq!(input, "ab"); // no consume
/// ```
pub fn lookahead<P>(parser: P) -> Lookahead<P> {
    Lookahead(parser)
}

/// Lookahead parser wrapper.
#[derive(Clone, Copy, Debug, Hash)]
/// Positive lookahead wrapper.
pub struct Lookahead<P>(pub(crate) P);

impl<I, E, N, L, P> ParserOnce<I, E, N, L> for Lookahead<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
{
    type Out = P::Out;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        i.lookahead(self.0)
    }
}
impl<I, E, N, L, P> ParserMut<I, E, N, L> for Lookahead<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        i.lookahead(self.0.by_mut())
    }
}
impl<I, E, N, L, P> Parser<I, E, N, L> for Lookahead<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        i.lookahead(self.0.by_ref())
    }
}

/// Negative lookahead for parsers with `OutOf` output.
///
/// - If the inner parser succeeds (`Ok`), this fails and may consume input.
/// - If the inner parser fails softly (`Err` without cut), this succeeds and rolls back.
/// - If the inner parser fails with cut, this fails and may consume input.
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "b";
/// let out = parse_ok_once(&mut input, not(item('a'))).unwrap();
/// assert_eq!(out, ());
/// assert_eq!(input, "b"); // no consume
/// ```
pub fn not<P>(parser: P) -> Not<P> {
    Not(parser)
}

/// Negative lookahead wrapper.
#[derive(Clone, Copy, Debug, Hash)]
/// Negative lookahead wrapper.
pub struct Not<P>(pub(crate) P);
/// Error emitted by [`not`] when the inner parser matches.
pub enum NotErr<E> {
    Exists,
    CutErr(E),
}
impl<P, I, E, N, L> ParserOnce<I, E, N, L> for Not<P>
where
    I: Input,
    E: ErrSink<I, NotErr<ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
{
    type Out = E::Out<(), NotErr<ErrOf<P, I, E, N, L>>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        i.not(self.0)
    }
}

impl<P, I, E, N, L> ParserMut<I, E, N, L> for Not<P>
where
    I: Input,
    E: ErrSink<I, NotErr<ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        i.not(self.0.by_mut())
    }
}

impl<P, I, E, N, L> Parser<I, E, N, L> for Not<P>
where
    I: Input,
    E: ErrSink<I, NotErr<ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        i.not(self.0.by_ref())
    }
}

impl<I, E, N, L, P> ParserOnce<I, E, N, L> for OrNot<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
{
    type Out = E::Out<Option<ValueOf<P, I, E, N, L>>, ErrOf<P, I, E, N, L>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        i.maybe(self.0)
    }
}

/// Run a parser in a non-root cut scope.
///
/// - `cut` performed inside this wrapper does **not** trigger [`crate::input::In::cut`] commits.
/// - The cut flag is still propagated outward as a branch-pruning signal.
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a";
/// let out = parse_ok_once(&mut input, no_cut(item('a').cut())).unwrap();
/// assert_eq!(out, 'a');
/// assert_eq!(input, "");
/// ```
pub fn no_cut<P>(parser: P) -> NoCut<P> {
    NoCut(parser)
}

#[derive(Clone, Copy, Debug, Hash)]
/// Wrapper that masks cut propagation.
pub struct NoCut<P>(pub(crate) P);

impl<P, I, E, N, L> ParserOnce<I, E, N, L> for NoCut<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L>,
{
    type Out = P::Out;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        i.no_cut(self.0)
    }
}
impl<P, I, E, N, L> ParserMut<I, E, N, L> for NoCut<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        i.no_cut(self.0.by_mut())
    }
}
impl<P, I, E, N, L> Parser<I, E, N, L> for NoCut<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        i.no_cut(self.0.by_ref())
    }
}

/// Attach a Parsec-like expected label to this parser (`<?>`).
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "b";
/// let err = parse_ok_once(&mut input, label(item('a'), "a")).unwrap_err();
/// assert_eq!(err.to_string(), "unexpected 'b', expecting a");
/// ```
pub fn label<P>(parser: P, label: impl Into<Cow<'static, str>>) -> Label<P> {
    Label(parser, label.into())
}

#[derive(Clone, Debug, Hash)]
/// Wrapper that attaches a label to errors.
pub struct Label<P>(pub(crate) P, pub(crate) Cow<'static, str>);

impl<P, I, E, N, L> ParserOnce<I, E, N, L> for Label<P>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, Expected<ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
{
    type Out = E::Out<ValueOf<P, I, E, N, L>, Expected<ErrOf<P, I, E, N, L>>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        i.label(self.0, self.1)
    }
}
impl<P, I, E, N, L> ParserMut<I, E, N, L> for Label<P>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, Expected<ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        i.label(self.0.by_mut(), self.1.clone())
    }
}
impl<P, I, E, N, L> Parser<I, E, N, L> for Label<P>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, Expected<ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        i.label(self.0.by_ref(), self.1.clone())
    }
}

/// Attach a lazily-evaluated expected label to this parser.
///
/// Unlike [`label`], this takes a function that generates the label only when needed (on failure).
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "b";
/// let err = parse_ok_once(&mut input, label_with(item('a'), || format!("letter 'a'"))).unwrap_err();
/// assert_eq!(err.to_string(), "unexpected 'b', expecting letter 'a'");
/// ```
pub fn label_with<P, F>(parser: P, f: F) -> LabelWith<P, F> {
    LabelWith { parser, f }
}

#[derive(Clone, Debug)]
/// Wrapper that builds labels dynamically from the input.
pub struct LabelWith<P, F> {
    pub(crate) parser: P,
    pub(crate) f: F,
}

impl<P, F, I, E, N, L, S> ParserOnce<I, E, N, L> for LabelWith<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, Expected<ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    F: FnOnce() -> S,
    S: Into<Cow<'static, str>>,
{
    type Out = E::Out<ValueOf<P, I, E, N, L>, Expected<ErrOf<P, I, E, N, L>>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        i.label_with(self.parser, self.f)
    }
}
impl<P, F, I, E, N, L, S> ParserMut<I, E, N, L> for LabelWith<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, Expected<ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: FnMut() -> S,
    S: Into<Cow<'static, str>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        i.label_with(self.parser.by_mut(), || (self.f)())
    }
}
impl<P, F, I, E, N, L, S> Parser<I, E, N, L> for LabelWith<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, Expected<ErrOf<P, I, E, N, L>>>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    F: Fn() -> S,
    S: Into<Cow<'static, str>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        i.label_with(self.parser.by_ref(), || (self.f)())
    }
}

/// Attach the consumed sequence to a parser output.
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let out = parse_ok_once(&mut input, with_seq(item('a'))).unwrap();
/// assert_eq!(out, ('a', "a"));
/// assert_eq!(input, "b");
/// ```
pub fn with_seq<P, S>(parser: P) -> WithSeq<P, S> {
    WithSeq(parser, PhantomData)
}

#[derive(Clone, Copy, Debug, Hash)]
/// Wrapper that returns the consumed slice along with the output.
pub struct WithSeq<P, S>(pub(crate) P, PhantomData<fn() -> S>);

impl<P, S, I, E, N, L> ParserOnce<I, E, N, L> for WithSeq<P, S>
where
    I: Input + crate::input::inner::SeqInput<S>,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
{
    type Out = E::Out<(ValueOf<P, I, E, N, L>, S), ErrOf<P, I, E, N, L>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        i.with_seq::<_, S>(self.0)
    }
}
impl<P, S, I, E, N, L> ParserMut<I, E, N, L> for WithSeq<P, S>
where
    I: Input + crate::input::inner::SeqInput<S>,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        i.with_seq::<_, S>(self.0.by_mut())
    }
}
impl<P, S, I, E, N, L> Parser<I, E, N, L> for WithSeq<P, S>
where
    I: Input + crate::input::inner::SeqInput<S>,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        i.with_seq::<_, S>(self.0.by_ref())
    }
}

/// Attach the consumed range (start and end positions) to a parser output.
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "hello world".with_counter(0usize);
/// let word = one_of(ASCII_ALPHA).many1::<String>().with_range();
/// let (text, range) = parse_ok_once(&mut input, word).unwrap();
/// assert_eq!(text, "hello");
/// assert_eq!(range, 0..5);
/// assert_eq!(*input.inner(), " world");
/// ```
pub fn with_range<P>(parser: P) -> WithRange<P> {
    WithRange(parser)
}

#[derive(Clone, Copy, Debug, Hash)]
/// Wrapper that returns the consumed range along with the output.
pub struct WithRange<P>(pub(crate) P);

impl<P, I, E, N, L> ParserOnce<I, E, N, L> for WithRange<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
{
    type Out = E::Out<(ValueOf<P, I, E, N, L>, std::ops::Range<I::Pos>), ErrOf<P, I, E, N, L>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        i.with_range(self.0)
    }
}
impl<P, I, E, N, L> ParserMut<I, E, N, L> for WithRange<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        i.with_range(self.0.by_mut())
    }
}
impl<P, I, E, N, L> Parser<I, E, N, L> for WithRange<P>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        i.with_range(self.0.by_ref())
    }
}

/// Convert a parser output using a fallible function, emitting a parse error on failure.
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a";
/// let err = parse_ok_once(
///     &mut input,
///     convert_err(item('a'), |_| if false { Ok('x') } else { Err("nope") }),
/// )
///     .unwrap_err();
/// assert_eq!(err.to_string(), "error nope");
/// ```
pub fn convert_err<P, F>(parser: P, f: F) -> ConvertErrP<P, F> {
    ConvertErrP(parser, f)
}

/// Wrapper that converts parse errors.
#[derive(Clone, Copy, Debug, Hash)]
pub struct ConvertErrP<P, F>(pub(crate) P, pub(crate) F);

/// Parse-vs-convert error payload.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ConvertErr<PE, CE> {
    Parse(PE),
    Convert(CE),
}

impl<PE, CE> ConvertErr<PE, CE> {
    /// Construct a parse error variant.
    pub fn parse(err: PE) -> Self {
        Self::Parse(err)
    }

    /// Construct a convert error variant.
    pub fn convert(err: CE) -> Self {
        Self::Convert(err)
    }
}

impl<PE> From<&'static str> for ConvertErr<PE, Cow<'static, str>> {
    fn from(value: &'static str) -> Self {
        ConvertErr::Convert(Cow::Borrowed(value))
    }
}

impl<PE> From<String> for ConvertErr<PE, Cow<'static, str>> {
    fn from(value: String) -> Self {
        ConvertErr::Convert(Cow::Owned(value))
    }
}

impl<PE, CE: std::fmt::Display> std::fmt::Display for ConvertErr<PE, CE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConvertErr::Parse(_) => f.write_str("parse error"),
            ConvertErr::Convert(e) => write!(f, "{e}"),
        }
    }
}

impl<P, F, I, E, N, L, O, CE> ParserOnce<I, E, N, L> for ConvertErrP<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ConvertErr<ErrOf<P, I, E, N, L>, CE>>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    F: FnOnce(ValueOf<P, I, E, N, L>) -> Result<O, CE>,
{
    type Out = E::Out<O, ConvertErr<ErrOf<P, I, E, N, L>, CE>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        let checkpoint = i.checkpoint();
        let p0 = i.pos();
        match self.0.run_once(i.rb()).as_result() {
            Ok(o) => match (self.1)(o) {
                Ok(v) => E::value(v),
                Err(e) => {
                    let p1 = i.pos();
                    if E::rollback_on_soft_failure() {
                        i.rollback(checkpoint);
                    }
                    i.push_err(p0..p1, ConvertErr::Convert(e))
                }
            },
            Err(e) => i.push_err(p0..i.pos(), ConvertErr::Parse(e)),
        }
    }
}

impl<P, F, I, E, N, L, O, CE> ParserMut<I, E, N, L> for ConvertErrP<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ConvertErr<ErrOf<P, I, E, N, L>, CE>>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: FnMut(ValueOf<P, I, E, N, L>) -> Result<O, CE>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        let checkpoint = i.checkpoint();
        let p0 = i.pos();
        match self.0.run_mut(i.rb()).as_result() {
            Ok(o) => match (self.1)(o) {
                Ok(v) => E::value(v),
                Err(e) => {
                    let p1 = i.pos();
                    i.rollback(checkpoint);
                    i.push_err(p0..p1, ConvertErr::Convert(e))
                }
            },
            Err(e) => i.push_err(p0..i.pos(), ConvertErr::Parse(e)),
        }
    }
}

impl<P, F, I, E, N, L, O, CE> Parser<I, E, N, L> for ConvertErrP<P, F>
where
    I: Input,
    E: ErrMode<I> + ErrSink<I, ConvertErr<ErrOf<P, I, E, N, L>, CE>>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    F: Fn(ValueOf<P, I, E, N, L>) -> Result<O, CE>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        let checkpoint = i.checkpoint();
        let p0 = i.pos();
        match self.0.run(i.rb()).as_result() {
            Ok(o) => match (self.1)(o) {
                Ok(v) => E::value(v),
                Err(e) => {
                    let p1 = i.pos();
                    i.rollback(checkpoint);
                    i.push_err(p0..p1, ConvertErr::Convert(e))
                }
            },
            Err(e) => i.push_err(p0..i.pos(), ConvertErr::Parse(e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{item, prim};

    #[test]
    fn lookahead_rolls_back_on_success() {
        let mut input = "a";
        let r =
            crate::parse::parse_once::<_, crate::input::error::std::Unexpected<char>, _, _>(&mut input, prim::lookahead(item::item('a')));
        assert_eq!(r.out, Some('a'));
        assert_eq!(input, "a");
    }

    #[test]
    fn not_succeeds_on_soft_failure() {
        let mut input = "a";
        let r = crate::parse::parse_once::<_, crate::input::error::std::StdErr<char>, _, _>(&mut input, prim::not(item::item('b')));
        assert_eq!(r.out, Some(()));
        assert_eq!(input, "a");
        assert!(r.errors().is_empty());
    }

    #[test]
    fn not_fails_on_inner_success_and_consumes() {
        let mut input = "a";
        let r = crate::parse::parse_once::<_, crate::input::error::std::StdErr<char>, _, _>(&mut input, prim::not(item::item('a')));
        assert_eq!(r.out, None);
        assert_eq!(input, "");
        assert!(!r.errors().is_empty());
    }

    #[test]
    fn label_does_not_add_errors_on_success() {
        let mut input = "a";
        let r = crate::parse::parse_once::<_, crate::input::error::std::StdErr<char>, _, _>(&mut input, prim::label(item::item('a'), "a"));
        assert_eq!(r.out, Some('a'));
        assert_eq!(input, "");
        assert!(r.errors().is_empty());
    }
}
