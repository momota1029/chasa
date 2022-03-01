use crate::{
    combi::{
        many, many1, And, AndThen, AndThenWith, Between, Bind, Case, Cut, Extend1Parser, ExtendParser, ExtendSep, ExtendSep1, Fold, Fold1, Label,
        LabelWith, Left, Many, Many1, ManyWith, Map, Or, OrNot, ParserIterator, Ranged, Right, Sep, Sep1, Value,
    },
    error::{CustomBuilder, LazyError, Nil},
    fold, fold1,
    input::{Input, IntoInput},
    prim::{self, RefParser},
    Error,
};
use std::{fmt::Display, marker::PhantomData};

/// General parser return value.
/// Returns the minimum information needed to continue the parse on success, or a thunk with error information on failure.
pub type IResult<O, I, S, M> = Result<(O, IOk<I, S, M>), LazyError<I, M>>;

/// Parser's success returns and common ones are summarized in `ok`, with functions for referencing immutable information and freeing resources.
pub struct ICont<'a, I: Input, C, S, M: CustomBuilder> {
    pub ok: IOk<I, S, M>,
    pub config: &'a C,
    pub drop: &'a mut dyn FnMut(),
}
/// This is the information that the Parser will continue to return in case it succeeds.
/// * `input` is an iterator with position
/// * `state` is information that is passed around throughout the parsing.
/// * `err` is the error that occurred in the previous parser. In rare cases, failures should be reported together.
/// * `cutted` is a bool value indicating that the previous parser has consumed the input.
pub struct IOk<I: Input, S, M: CustomBuilder> {
    pub input: I,
    pub state: S,
    pub err: Option<LazyError<I, M>>,
    pub cutted: bool,
}
impl<I: Input, S, M: CustomBuilder> IOk<I, S, M> {
    pub(crate) fn to_cont<'a, C>(self, config: &'a C, drop: &'a mut dyn FnMut()) -> ICont<'a, I, C, S, M> {
        ICont { ok: self, config, drop }
    }
}

/// The return value of the Parser is made truly persistent.
pub struct IReturn<'a, O, I: Input, C, S, M: CustomBuilder>(pub(crate) Result<(O, ICont<'a, I, C, S, M>), LazyError<I, M>>);

/// A parser that can be run only once, like `FnOnce`.
/// Some functions are marked with `once` because the callback only needs to be executed once.
/// Often used to process values.
/// `I` is a stream with position, `C` is an immutable value that the whole parser refers to (an argument if it's a function), `S` is a variable value that the whole parser refers to (like a global variable), and `M` is a type for defining your own errors.
pub trait ParserOnce<I: Input, C, S, M: CustomBuilder> {
    type Output;
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<Self::Output, I, S, M>;

    fn case_once<F: FnOnce(Self::Output, ICont<I, C, S, M>) -> IReturn<O, I, C, S, M>, O>(self, f: F) -> Case<Self, F>
    where
        Self: Sized,
    {
        Case(self, f)
    }
    fn map_once<F: FnOnce(Self::Output) -> O, O>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map(self, f)
    }
    fn value<O>(self, value: O) -> Value<Self, O>
    where
        Self: Sized,
    {
        Value(self, value)
    }
    fn label<L: Display + 'static>(self, label: L) -> Label<Self, L>
    where
        Self: Sized,
    {
        Label(self, label)
    }
    fn label_with<L: Display, F: Fn() -> L + 'static>(self, label: F) -> LabelWith<Self, F>
    where
        Self: Sized,
    {
        LabelWith(self, label)
    }
    fn bind_once<F: FnOnce(Self::Output) -> P, P: ParserOnce<I, C, S, M>>(self, f: F) -> Bind<Self, F>
    where
        Self: Sized,
    {
        Bind(self, f)
    }
    fn and_then_once<O, E: Display + 'static, F: FnOnce(Self::Output) -> Result<O, prim::Error<E>>>(self, f: F) -> AndThen<Self, F>
    where
        Self: Sized,
    {
        AndThen(self, f)
    }
    fn and_then_once_with<O, E: Display, F2: Fn() -> E + 'static, F1: FnOnce(Self::Output) -> Result<O, prim::Error<F2>>>(
        self, f: F1,
    ) -> AndThenWith<Self, F1, F2>
    where
        Self: Sized,
    {
        AndThenWith(self, f, PhantomData)
    }
    #[inline]
    fn and<P: ParserOnce<I, C, S, M>>(self, other: P) -> And<Self, P>
    where
        Self: Sized,
    {
        And(self, other)
    }
    #[inline]
    fn left<P: ParserOnce<I, C, S, M>>(self, other: P) -> Left<Self, P>
    where
        Self: Sized,
    {
        Left(self, other)
    }
    #[inline]
    fn right<P: ParserOnce<I, C, S, M>>(self, other: P) -> Right<Self, P>
    where
        Self: Sized,
    {
        Right(self, other)
    }
    #[inline]
    fn or_not(self) -> OrNot<Self>
    where
        Self: Sized,
    {
        OrNot(self)
    }
    #[inline]
    fn between<P1: ParserOnce<I, C, S, M>, P2: ParserOnce<I, C, S, M>>(self, left: P1, right: P2) -> Between<P1, Self, P2>
    where
        Self: Sized,
    {
        Between(left, self, right)
    }
    #[inline]
    fn or<P: ParserOnce<I, C, S, M>>(self, other: P) -> Or<Self, P>
    where
        Self: Sized,
    {
        Or(self, other)
    }
    #[inline]
    fn cut(self) -> Cut<Self>
    where
        Self: Sized,
    {
        Cut(self)
    }
    #[inline]
    fn ranged(self) -> Ranged<Self>
    where
        Self: Sized,
    {
        Ranged(self)
    }
}

/// A parser that can be used again and again.
/// There is no equivalent to FnMut to prevent it from becoming an unintentionally destructive parser.
pub trait Parser<I: Input, C, S, M: CustomBuilder>: ParserOnce<I, C, S, M> {
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<Self::Output, I, S, M>;

    fn to_ref(&self) -> RefParser<Self>
    where
        Self: Sized,
    {
        RefParser(&self)
    }
    fn case<F: Fn(Self::Output, ICont<I, C, S, M>) -> IReturn<O, I, C, S, M>, O>(self, f: F) -> Case<Self, F>
    where
        Self: Sized,
    {
        Case(self, f)
    }
    fn map<F: Fn(Self::Output) -> O, O>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map(self, f)
    }
    fn bind<F: Fn(Self::Output) -> P, P: ParserOnce<I, C, S, M>>(self, f: F) -> Bind<Self, F>
    where
        Self: Sized,
    {
        Bind(self, f)
    }
    fn and_then<O, E: Display + 'static, F: Fn(Self::Output) -> Result<O, prim::Error<E>>>(self, f: F) -> AndThen<Self, F>
    where
        Self: Sized,
    {
        AndThen(self, f)
    }
    fn and_then_with<O, E: Display, F2: Fn() -> E + 'static, F1: Fn(Self::Output) -> Result<O, prim::Error<F2>>>(
        self, f: F1,
    ) -> AndThenWith<Self, F1, F2>
    where
        Self: Sized,
    {
        AndThenWith(self, f, PhantomData)
    }
    fn fold<T, F: Fn(T, Self::Output) -> T>(self, init: T, f: F) -> Fold<T, Self, F>
    where
        Self: Sized,
    {
        fold(init, self, f)
    }
    fn fold1<T, F: Fn(T, Self::Output) -> T>(self, init: T, f: F) -> Fold1<T, Self, F>
    where
        Self: Sized,
    {
        fold1(init, self, f)
    }
    fn sep<T, F: Fn(T, Self::Output) -> T, P: Parser<I, C, S, M>>(self, init: T, f: F, sep: P) -> Sep<T, Self, P, F>
    where
        Self: Sized,
    {
        Sep { init, p: self, sep, succ: f }
    }
    fn sep1<T, F: Fn(T, Self::Output) -> T, P: Parser<I, C, S, M>>(self, init: T, f: F, sep: P) -> Sep1<T, Self, P, F>
    where
        Self: Sized,
    {
        Sep1 { init, p: self, sep, succ: f }
    }
    fn extend<O>(self, value: O) -> ExtendParser<O, Self>
    where
        Self: Sized,
    {
        ExtendParser(value, self)
    }
    fn extend1<O>(self, value: O) -> Extend1Parser<O, Self>
    where
        Self: Sized,
    {
        Extend1Parser(value, self)
    }
    fn extend_sep<T: Extend<Self::Output>, P: Parser<I, C, S, M>>(self, init: T, sep: P) -> ExtendSep<T, Self, P>
    where
        Self: Sized,
    {
        ExtendSep { init, p: self, sep }
    }
    fn extend_sep1<T: Extend<Self::Output>, P: Parser<I, C, S, M>>(self, init: T, sep: P) -> ExtendSep1<T, Self, P>
    where
        Self: Sized,
    {
        ExtendSep1 { init, p: self, sep }
    }
    fn skip_many(self) -> ExtendParser<(), Value<Self, ()>>
    where
        Self: Sized,
    {
        self.value(()).extend(())
    }
    fn skip_many1(self) -> Extend1Parser<(), Value<Self, ()>>
    where
        Self: Sized,
    {
        self.value(()).extend1(())
    }
    fn many<B: FromIterator<Self::Output>>(self) -> Many<Self, B>
    where
        Self: Sized,
    {
        many(self)
    }
    fn many1<B: FromIterator<Self::Output>>(self) -> Many1<Self, B>
    where
        Self: Sized,
    {
        many1(self)
    }
    fn many_with<O, F: Fn(ParserIterator<prim::RefParser<Self>, I, C, S, M>) -> O>(self, f: F) -> ManyWith<Self, F>
    where
        Self: Sized,
    {
        ManyWith(self, f)
    }
}

pub trait SimpleParser<I: Input, M: CustomBuilder>: ParserOnce<I, (), (), M> + Sized {
    fn parse<In: IntoInput<IntoI = I>>(self, input: In) -> Result<Self::Output, Error<I, M::To>> {
        self.run_once(ICont { ok: IOk { input: input.into_input(), state: (), err: None, cutted: false }, config: &(), drop: &mut || {} })
            .map(|(o, _)| o)
            .map_err(|e| e.map(|e| e.calc()))
    }
}
impl<I: Input, M: CustomBuilder, P: ParserOnce<I, (), (), M>> SimpleParser<I, M> for P {}

/// Trait to parse a &str easily.
pub trait EasyParser<I: Input>: ParserOnce<I, (), (), Nil> + Sized {
    /// Returns the result or error string, useful for testing.
    fn parse_easy<In: IntoInput<IntoI = I>>(self, input: In) -> Result<Self::Output, String>
    where
        I::Pos: Display,
    {
        self.run_once(ICont { ok: IOk { input: input.into_input(), state: (), err: None, cutted: false }, config: &(), drop: &mut || {} })
            .map(|(o, _)| o)
            .map_err(|e| format!("{}", e.map(|e| e.calc())))
    }
}
impl<I: Input, P: ParserOnce<I, (), (), Nil>> EasyParser<I> for P {}
