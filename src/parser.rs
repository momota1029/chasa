pub mod choice;
pub mod flow;
pub mod item;
pub mod many;
pub mod prim;
pub mod sep;
pub mod seq;
pub mod str;
pub mod then;
pub mod token;
use ::std::ops::ControlFlow;
use reborrow_generic::short::Rb;

use crate::{
    Input,
    input::{
        In,
        error::{ErrMode, OutOf},
        inner::RbBack,
    },
    parser::prim::{MutParser, OrNot},
    prelude::{Merger, StdErr},
};

/// Parser trait (single-shot) for `chasa-experiment`.
///
/// `ParserOnce` consumes `self` to run the parser. Most combinators in this crate implement
/// `ParserOnce`, and the same type may additionally implement [`ParserMut`] / [`Parser`].
///
/// Rollback is **not automatic**. Whether the input is rolled back on failure is decided by
/// combinators (not by the output type).
pub trait ParserOnce<I: Input, E: ErrMode<I> = Merger<I, StdErr<<I as Input>::Item>>, N: Rb = (), L: RbBack = ()>: Sized {
    type Out;
    fn run_once(self, i: In<I, E, N, L>) -> Self::Out;
    /// Optionalize a parser with `OutOf` semantics.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a";
    /// let out = parse_ok_once(&mut input, item('a').or_not()).unwrap();
    /// assert_eq!(out, Some('a'));
    /// ```
    fn or_not(self) -> OrNot<Self> {
        prim::maybe(self)
    }
    /// Commit-on-success for `OutOf` parsers.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a";
    /// let out = parse_ok_once(&mut input, item('a').cut()).unwrap();
    /// assert_eq!(out, 'a');
    /// ```
    fn cut(self) -> prim::CutIfOk<Self>
    where
        Self::Out: OutOf<I, E>,
    {
        prim::cut_if_ok(self)
    }
    /// Attach the consumed range (start and end positions) to a parser output.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "hello world".with_counter(0usize);
    /// let p = item('h').with_range();
    /// let (ch, range) = parse_ok_once(&mut input, p).unwrap();
    /// assert_eq!(ch, 'h');
    /// assert_eq!(range, 0..1);
    /// ```
    fn with_range(self) -> prim::WithRange<Self>
    where
        Self::Out: OutOf<I, E>,
    {
        prim::with_range(self)
    }
    /// Attach the consumed sequence to a parser output.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "ab";
    /// let out = parse_ok_once(&mut input, item('a').with_seq::<&str>()).unwrap();
    /// assert_eq!(out, ('a', "a"));
    /// assert_eq!(input, "b");
    /// ```
    fn with_seq<S>(self) -> prim::WithSeq<Self, S>
    where
        Self::Out: OutOf<I, E>,
    {
        prim::with_seq(self)
    }
    /// Run this parser as lookahead.
    ///
    /// On success, input is rolled back. On failure, input may be consumed (see crate-level docs).
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "ab";
    /// let out = parse_ok_once(&mut input, item('a').lookahead()).unwrap();
    /// assert_eq!(out, 'a');
    /// assert_eq!(input, "ab");
    /// ```
    fn lookahead(self) -> prim::Lookahead<Self>
    where
        Self::Out: OutOf<I, E>,
    {
        prim::lookahead(self)
    }
    /// Run this parser in a non-root cut scope (cut does not trigger commits).
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a";
    /// let out = parse_ok_once(&mut input, item('a').cut().no_cut()).unwrap();
    /// assert_eq!(out, 'a');
    /// assert_eq!(input, "");
    /// ```
    fn no_cut(self) -> prim::NoCut<Self> {
        prim::no_cut(self)
    }
    /// Negative lookahead for `OutOf` parsers.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "b";
    /// let out = parse_ok_once(&mut input, item('a').not()).unwrap();
    /// assert_eq!(out, ());
    /// assert_eq!(input, "b");
    /// ```
    fn not(self) -> prim::Not<Self>
    where
        Self::Out: OutOf<I, E>,
    {
        prim::not(self)
    }
    /// Attach a label to a parser failure (Parsec-like `<?>`).
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "b";
    /// let err = parse_ok_once(&mut input, item('a').label("a")).unwrap_err();
    /// assert_eq!(err.to_string(), "unexpected 'b', expecting a");
    /// ```
    fn label(self, label: impl Into<std::borrow::Cow<'static, str>>) -> prim::Label<Self> {
        prim::label(self, label)
    }
    /// Attach a lazily-evaluated label to a parser failure.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "b";
    /// let err = parse_ok_once(&mut input, item('a').label_with(|| format!("letter 'a'"))).unwrap_err();
    /// assert_eq!(err.to_string(), "unexpected 'b', expecting letter 'a'");
    /// ```
    fn label_with<F, S>(self, f: F) -> prim::LabelWith<Self, F>
    where
        F: Fn() -> S,
        S: Into<std::borrow::Cow<'static, str>>,
    {
        prim::label_with(self, f)
    }
    /// Like [`ParserOnce::label_with`], but intended for single-shot label builders (`FnOnce`).
    fn label_with_once<F, S>(self, f: F) -> prim::LabelWith<Self, F>
    where
        F: FnOnce() -> S,
        S: Into<std::borrow::Cow<'static, str>>,
    {
        prim::label_with(self, f)
    }
    /// Like [`ParserOnce::label_with`], but intended for mutable/repeatable label builders (`FnMut`).
    fn label_with_mut<F, S>(self, f: F) -> prim::LabelWith<Self, F>
    where
        F: FnMut() -> S,
        S: Into<std::borrow::Cow<'static, str>>,
    {
        prim::label_with(self, f)
    }
    /// Like [`Parser::map`], but intended for single-shot mappers (`FnOnce`).
    fn map_once<O, F>(self, f: F) -> then::Map<Self, F>
    where
        Self::Out: OutOf<I, E>,
        F: FnOnce(ValueOf<Self, I, E, N, L>) -> O,
    {
        then::map(self, f)
    }
    /// Replace the output of a parser with a constant value.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a";
    /// let out = parse_ok_once(&mut input, item('a').to(123)).unwrap();
    /// assert_eq!(out, 123);
    /// ```
    fn to<O>(self, value: O) -> then::To<Self, O>
    where
        Self::Out: OutOf<I, E>,
    {
        then::to(self, value)
    }
    /// Like [`Parser::bind`], but intended for single-shot binders (`FnOnce`).
    fn bind_once<P2, F>(self, f: F) -> then::Bind<Self, F>
    where
        Self::Out: OutOf<I, E>,
        F: FnOnce(ValueOf<Self, I, E, N, L>) -> P2,
        P2: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<Self, I, E, N, L>>>,
    {
        then::bind(self, f)
    }
    /// Parse `self` then `right`, returning both outputs as a tuple.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "ab";
    /// let out = parse_ok_once(&mut input, item('a').then(item('b'))).unwrap();
    /// assert_eq!(out, ('a', 'b'));
    /// assert_eq!(input, "");
    /// ```
    fn then<P>(self, right: P) -> (Self, P) {
        then::then(self, right)
    }
    /// Parse `self` then `right`, returning the output of `self`.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "ab";
    /// let out = parse_ok_once(&mut input, item('a').left(item('b'))).unwrap();
    /// assert_eq!(out, 'a');
    /// assert_eq!(input, "");
    /// ```
    fn left<P>(self, right: P) -> then::Left<Self, P> {
        then::left(self, right)
    }
    /// Parse `self` then `right`, returning the output of `right`.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "ab";
    /// let out = parse_ok_once(&mut input, item('a').right(item('b'))).unwrap();
    /// assert_eq!(out, 'b');
    /// assert_eq!(input, "");
    /// ```
    fn right<P>(self, right: P) -> then::Right<Self, P> {
        then::right(self, right)
    }
    /// Parse `left`, then `self`, then `right`, returning the output of `self`.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "(a)";
    /// let out = parse_ok_once(&mut input, item('a').between(item('('), item(')'))).unwrap();
    /// assert_eq!(out, 'a');
    /// assert_eq!(input, "");
    /// ```
    fn between<Lp, Rp>(self, left: Lp, right: Rp) -> then::Between<Lp, Self, Rp> {
        then::between(left, right, self)
    }
    /// Choice: try `self`, then `other` on soft failure.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "b";
    /// let out = parse_ok_once(&mut input, item('a').or(item('b'))).unwrap();
    /// assert_eq!(out, 'b');
    /// ```
    fn or<P>(self, other: P) -> choice::Choice<(Self, P)>
    where
        Self::Out: OutOf<I, E>,
        P: ParserOnce<I, E, N, L, Out = Self::Out>,
    {
        choice::choice((self, other))
    }
}
/// Parser trait for parsers that can be run multiple times by mutable reference.
///
/// `ParserMut` enables iterator-driven combinators like `many` to call the same parser repeatedly
/// without cloning.
pub trait ParserMut<I: Input, E: ErrMode<I> = Merger<I, StdErr<<I as Input>::Item>>, N: Rb = (), L: RbBack = ()>:
    ParserOnce<I, E, N, L>
{
    fn run_mut(&mut self, i: In<I, E, N, L>) -> Self::Out;
    /// Borrow a parser mutably as `ParserOnce`.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a";
    /// let mut p = item('a');
    /// let out = parse_ok_once(&mut input, p.by_mut()).unwrap();
    /// assert_eq!(out, 'a');
    /// ```
    fn by_mut<'a>(&'a mut self) -> MutParser<'a, Self> {
        MutParser(self)
    }
    /// Like [`Parser::map`], but intended for mutable/repeatable mappers (`FnMut`).
    fn map_mut<O, F>(self, f: F) -> then::Map<Self, F>
    where
        Self: Sized,
        Self::Out: OutOf<I, E>,
        F: FnMut(ValueOf<Self, I, E, N, L>) -> O,
    {
        then::map(self, f)
    }
    /// Like [`Parser::bind`], but intended for mutable/repeatable binders (`FnMut`).
    fn bind_mut<P2, F>(self, f: F) -> then::Bind<Self, F>
    where
        Self: Sized,
        Self::Out: OutOf<I, E>,
        F: FnMut(ValueOf<Self, I, E, N, L>) -> P2,
        P2: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<Self, I, E, N, L>>>,
    {
        then::bind(self, f)
    }
    /// Stateful loop that terminates on soft failure via [`In::maybe`].
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
    fn flow<S, Step, End, O, T>(self, state: S, step: Step, end: End) -> flow::Flow<Self, S, Step, End>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Step: FnMut(S, T) -> ControlFlow<O, S>,
        End: FnMut(S) -> O,
    {
        flow::Flow::new(self, state, step, end)
    }
    /// Collect `Continue(T1)` values until `Break(T2)`.
    ///
    /// ```
    /// use chasa::prelude::*;
    /// use std::ops::ControlFlow;
    ///
    /// let mut input = "aaab";
    /// let p = |mut i: In<&str, Merger<&str, StdErr<char>>, (), ()>| match i.input.next() {
    ///     Some('a') => ControlFlow::Continue('a'),
    ///     Some(c) => ControlFlow::Break(c),
    ///     None => ControlFlow::Break('\0'),
    /// };
    /// let r = parse_once::<_, StdErr<char>, _, _>(&mut input, p.flow_many::<String, char, char>());
    /// assert_eq!(r.out.0, "aaa");
    /// assert_eq!(r.out.1, 'b');
    /// assert_eq!(input, "");
    /// ```
    fn flow_many<O, T1, T2>(self) -> flow::FlowMany<Self, O>
    where
        Self: Sized + ParserMut<I, E, N, L, Out = ControlFlow<T2, T1>>,
        O: FromIterator<T1>,
    {
        flow::FlowMany::new(self)
    }
    /// Like [`Parser::flow_many_map`], but intended for single-shot mappers (`FnOnce`).
    fn flow_many_map_once<F, O, T1, T2>(self, f: F) -> flow::FlowManyMap<Self, F>
    where
        Self: Sized + ParserMut<I, E, N, L, Out = ControlFlow<T2, T1>>,
        F: for<'a, 'b> FnOnce(&mut flow::FlowManyIterator<'a, 'b, Self, I, E, N, L, T1, T2>) -> O,
    {
        flow::FlowManyMap::new(self, f)
    }
    /// Like [`Parser::flow_many_map`], but intended for mutable/repeatable mappers (`FnMut`).
    fn flow_many_map_mut<F, O, T1, T2>(self, f: F) -> flow::FlowManyMap<Self, F>
    where
        Self: Sized + ParserMut<I, E, N, L, Out = ControlFlow<T2, T1>>,
        F: for<'a, 'b> FnMut(&mut flow::FlowManyIterator<'a, 'b, Self, I, E, N, L, T1, T2>) -> O,
    {
        flow::FlowManyMap::new(self, f)
    }

    /// Repeat a parser until soft failure, collecting outputs.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "aaab";
    /// let out: String = parse_ok_once(&mut input, item('a').many()).unwrap();
    /// assert_eq!(out, "aaa");
    /// assert_eq!(input, "b");
    /// ```
    fn many<O>(self) -> many::Many<Self, O>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        O: FromIterator<ValueOf<Self, I, E, N, L>>,
    {
        many::many(self)
    }
    /// Repeat a parser one or more times, collecting outputs.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "aaab";
    /// let out: String = parse_ok_once(&mut input, item('a').many1()).unwrap();
    /// assert_eq!(out, "aaa");
    /// assert_eq!(input, "b");
    /// ```
    fn many1<O>(self) -> many::Many1<Self, O>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        O: FromIterator<ValueOf<Self, I, E, N, L>>,
    {
        many::many1(self)
    }
    /// Like [`Parser::many_map`], but intended for single-shot mappers (`FnOnce`).
    fn many_map_once<F, O>(self, f: F) -> many::ManyMap<Self, F>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        F: for<'a, 'b> FnOnce(many::ManyMapIterator<'a, 'b, Self, I, E, N, L>) -> O,
    {
        many::many_map_once(self, f)
    }
    /// Like [`Parser::many_map`], but intended for mutable/repeatable mappers (`FnMut`).
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "aaab";
    /// let mut acc = 0usize;
    /// let p = item('a').many_map_mut(|it| {
    ///     acc += it.count();
    ///     acc
    /// });
    /// let out = parse_ok_once(&mut input, p).unwrap();
    /// assert_eq!(out, 3);
    /// assert_eq!(acc, 3);
    /// assert_eq!(input, "b");
    /// ```
    fn many_map_mut<F, O>(self, f: F) -> many::ManyMap<Self, F>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        F: for<'a, 'b> FnMut(many::ManyMapIterator<'a, 'b, Self, I, E, N, L>) -> O,
    {
        many::many_map_mut(self, f)
    }
    /// Like [`Parser::many1_map`], but intended for single-shot mappers (`FnOnce`).
    fn many1_map_once<F, O>(self, f: F) -> many::Many1Map<Self, F>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        F: for<'a, 'b> FnOnce(many::Many1MapIterator<'a, 'b, Self, I, E, N, L>) -> O,
    {
        many::many1_map_once(self, f)
    }
    /// Like [`Parser::many1_map`], but intended for mutable/repeatable mappers (`FnMut`).
    fn many1_map_mut<F, O>(self, f: F) -> many::Many1Map<Self, F>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        F: for<'a, 'b> FnMut(many::Many1MapIterator<'a, 'b, Self, I, E, N, L>) -> O,
    {
        many::many1_map_mut(self, f)
    }
    /// Repeat a parser until soft failure, discarding outputs.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "aaab";
    /// let out = parse_ok_once(&mut input, item('a').many_skip()).unwrap();
    /// assert_eq!(out, ());
    /// assert_eq!(input, "b");
    /// ```
    fn many_skip(self) -> many::SkipMany<Self>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    {
        many::many_skip(self)
    }
    /// Repeat a parser one or more times, discarding outputs.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "aaab";
    /// let out = parse_ok_once(&mut input, item('a').many1_skip()).unwrap();
    /// assert_eq!(out, ());
    /// assert_eq!(input, "b");
    /// ```
    fn many1_skip(self) -> many::SkipMany1<Self>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    {
        many::many1_skip(self)
    }
    /// Repeat a parser a specified number of times.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "abcd1234";
    /// let hex = one_of("0123456789abcdefABCDEF");
    /// let out: String = parse_ok_once(&mut input, hex.count(4)).unwrap();
    /// assert_eq!(out, "abcd");
    /// assert_eq!(input, "1234");
    /// ```
    fn count<R, O>(self, range: R) -> many::Count<Self, O>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        R: many::CountRange,
        O: FromIterator<ValueOf<Self, I, E, N, L>>,
    {
        many::count(range, self)
    }
    /// Parse a separated list: `item (sep item)*`.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a,a,";
    /// let comma = item(',').to(());
    /// let out: String = parse_ok_once(&mut input, item('a').sep(comma)).unwrap();
    /// assert_eq!(out, "aa");
    /// assert_eq!(input, "");
    /// ```
    fn sep<O, Q, T>(self, sep_p: Q) -> sep::Sep<O, sep::iter::Zero, sep::iter::Allow, Self, Q, T>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<Self, I, E, N, L>>>,
        O: FromIterator<T>,
    {
        sep::sep(self, sep_p)
    }
    /// Like [`ParserMut::sep`], but requires at least one item (`1+`).
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a,a";
    /// let comma = item(',').to(());
    /// let out: String = parse_ok_once(&mut input, item('a').sep1(comma)).unwrap();
    /// assert_eq!(out, "aa");
    /// assert_eq!(input, "");
    /// ```
    fn sep1<O, Q, T>(self, sep_p: Q) -> sep::Sep<O, sep::iter::One, sep::iter::Allow, Self, Q, T>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<Self, I, E, N, L>>>,
        O: FromIterator<T>,
    {
        sep::sep1(self, sep_p)
    }
    /// Like [`Parser::sep_map`], but intended for single-shot mappers (`FnOnce`).
    fn sep_map_once<Q, F, T, S, O>(self, sep_p: Q, f: F) -> sep::SepMap<sep::iter::Zero, sep::iter::Allow, Self, Q, F, T>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<Self, I, E, N, L>>>,
        F: for<'a, 'b> FnOnce(sep::SepMapIterator<'a, 'b, Self, Q, I, E, N, L, sep::iter::Zero, sep::iter::Allow>) -> O,
    {
        sep::sep_map_once(self, sep_p, f)
    }
    /// Like [`Parser::sep_map`], but intended for mutable/repeatable mappers (`FnMut`).
    fn sep_map_mut<Q, F, T, S, O>(self, sep_p: Q, f: F) -> sep::SepMap<sep::iter::Zero, sep::iter::Allow, Self, Q, F, T>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<Self, I, E, N, L>>>,
        F: for<'a, 'b> FnMut(sep::SepMapIterator<'a, 'b, Self, Q, I, E, N, L, sep::iter::Zero, sep::iter::Allow>) -> O,
    {
        sep::sep_map_mut(self, sep_p, f)
    }
    /// Left-associative separator reduction: `term (op term)*`.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a+a+a";
    /// let term = item('a').to(1);
    /// let plus = item('+').to(());
    /// let out = parse_ok_once(&mut input, term.sep_reduce(plus, |a, _, b| a + b)).unwrap();
    /// assert_eq!(out, 3);
    /// assert_eq!(input, "");
    /// ```
    fn sep_reduce<Q, F>(self, op: Q, f: F) -> sep::SepReduce<Self, Q, F> {
        sep::sep_reduce(self, op, f)
    }
}
/// Parser trait for parsers that can be run by shared reference.
///
/// This is the most restrictive (and most convenient) parser trait, enabling reuse from `&self`.
pub trait Parser<I: Input, E: ErrMode<I> = Merger<I, StdErr<<I as Input>::Item>>, N: Rb = (), L: RbBack = ()>:
    ParserMut<I, E, N, L>
{
    fn run(&self, i: In<I, E, N, L>) -> Self::Out;
    /// Borrow a parser immutably as `ParserOnce`.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a";
    /// let p = item('a');
    /// let out = parse_ok_once(&mut input, p.by_ref()).unwrap();
    /// assert_eq!(out, 'a');
    /// ```
    fn by_ref<'a>(&'a self) -> prim::RefParser<'a, Self> {
        prim::RefParser(self)
    }
    /// Like [`ParserMut::flow_many`], but lets you fold via a [`flow::FlowManyIterator`].
    ///
    /// ```
    /// use chasa::prelude::*;
    /// use std::ops::ControlFlow;
    ///
    /// let mut input = "aaab";
    /// let p = |mut i: In<&str, Merger<&str, StdErr<char>>, (), ()>| match i.input.next() {
    ///     Some('a') => ControlFlow::Continue('a'),
    ///     Some(c) => ControlFlow::Break(c),
    ///     None => ControlFlow::Break('\0'),
    /// };
    /// let r = parse_once::<_, StdErr<char>, _, _>(&mut input, p.flow_many_map(|it| it.count()));
    /// assert_eq!(r.out.0, 3);
    /// assert_eq!(r.out.1, 'b');
    /// assert_eq!(input, "");
    /// ```
    fn flow_many_map<F, O, T1, T2>(self, f: F) -> flow::FlowManyMap<Self, F>
    where
        Self: Sized + ParserMut<I, E, N, L, Out = ControlFlow<T2, T1>>,
        F: for<'a, 'b> Fn(&mut flow::FlowManyIterator<'a, 'b, Self, I, E, N, L, T1, T2>) -> O,
    {
        flow::FlowManyMap::new(self, f)
    }
    /// Like [`ParserMut::many`], but lets you fold via a streaming iterator.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "aaab";
    /// let out = parse_ok_once(&mut input, item('a').many_map(|it| it.count())).unwrap();
    /// assert_eq!(out, 3);
    /// assert_eq!(input, "b");
    /// ```
    fn many_map<F, O>(self, f: F) -> many::ManyMap<Self, F>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        F: for<'a, 'b> Fn(many::ManyMapIterator<'a, 'b, Self, I, E, N, L>) -> O,
    {
        many::many_map(self, f)
    }
    /// Like [`Parser::many_map`], but requires at least one element.
    fn many1_map<F, O>(self, f: F) -> many::Many1Map<Self, F>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        F: for<'a, 'b> Fn(many::Many1MapIterator<'a, 'b, Self, I, E, N, L>) -> O,
    {
        many::many1_map(self, f)
    }
    /// Like [`ParserMut::sep`], but lets you fold via a streaming iterator.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a,a";
    /// let comma = item(',').to(());
    /// let out = parse_ok_once(&mut input, item('a').sep_map(comma, |it| it.no_trail().count())).unwrap();
    /// assert_eq!(out, 2);
    /// assert_eq!(input, "");
    /// ```
    fn sep_map<Q, F, T, S, O>(self, sep_p: Q, f: F) -> sep::SepMap<sep::iter::Zero, sep::iter::Allow, Self, Q, F, T>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<Self, I, E, N, L>>>,
        F: for<'a, 'b> Fn(sep::SepMapIterator<'a, 'b, Self, Q, I, E, N, L, sep::iter::Zero, sep::iter::Allow>) -> O,
    {
        sep::sep_map(self, sep_p, f)
    }
    /// Like [`Parser::sep_map`], but requires at least one item (`1+`).
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a,a";
    /// let comma = item(',').to(());
    /// let out = parse_ok_once(&mut input, item('a').sep1_map(comma, |it| it.no_trail().count())).unwrap();
    /// assert_eq!(out, 2);
    /// assert_eq!(input, "");
    /// ```
    fn sep1_map<Q, F, T, S, O>(self, sep_p: Q, f: F) -> sep::SepMap<sep::iter::One, sep::iter::Allow, Self, Q, F, T>
    where
        Self: Sized + ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<Self, I, E, N, L>>>,
        F: for<'a, 'b> Fn(sep::SepMapIterator<'a, 'b, Self, Q, I, E, N, L, sep::iter::One, sep::iter::Allow>) -> O,
    {
        sep::sep1_map(self, sep_p, f)
    }
    /// Map the output of a parser.
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "a";
    /// let out = parse_ok_once(&mut input, item('a').map(|c: char| c.to_ascii_uppercase())).unwrap();
    /// assert_eq!(out, 'A');
    /// ```
    fn map<O, F>(self, f: F) -> then::Map<Self, F>
    where
        Self: Sized,
        Self::Out: OutOf<I, E>,
        F: Fn(ValueOf<Self, I, E, N, L>) -> O,
    {
        then::map(self, f)
    }
    /// Sequential bind (flat_map).
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "ab";
    /// let out = parse_ok_once(&mut input, item('a').bind(|_| item('b'))).unwrap();
    /// assert_eq!(out, 'b');
    /// ```
    fn bind<P2, F>(self, f: F) -> then::Bind<Self, F>
    where
        Self: Sized,
        Self::Out: OutOf<I, E>,
        F: Fn(ValueOf<Self, I, E, N, L>) -> P2,
        P2: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<Self, I, E, N, L>>>,
    {
        then::bind(self, f)
    }
}

pub type ValueOf<P, I, E, N, L> = <<P as ParserOnce<I, E, N, L>>::Out as OutOf<I, E>>::Value;
pub type ErrOf<P, I, E, N, L> = <<P as ParserOnce<I, E, N, L>>::Out as OutOf<I, E>>::Error;

/// Sequentially run two parsers and return both outputs.
pub struct And<P, Q>(P, Q);
impl<I, E, N, L, P, Q> ParserOnce<I, E, N, L> for And<P, Q>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    Q: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    type Out = E::Out<(ValueOf<P, I, E, N, L>, ValueOf<Q, I, E, N, L>), ErrOf<P, I, E, N, L>>;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        E::and_then(self.0.run_once(i.rb()).into(), |o1| E::map(self.1.run_once(i).into(), |o2| (o1, o2)))
    }
}
