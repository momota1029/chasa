use crate::{
    combi::{
        And, AndThen, Between, Bind, Case, Cut, GetString, GetStringExtend, Label, LabelWith, Left, Map, Or, OrNot,
        Ranged, Right, Value,
    },
    error::{Builder, CustomBuilder, LazyError, Nil},
    fold::{fold, fold1, Extend1Parser, ExtendParser, Fold, Fold1, SepExtend, SepExtend1, SepFold, SepFold1},
    input::{Input, IntoInput},
    many::{
        many, many1, take, Many, Many1, ManyThen, ManyWith, ParserIterator, ParserSepIterator, Repeat, Sep, Sep1,
        SepThen, SepWith,
    },
    prim::RefParser,
    util::RangeWithOrd,
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
pub struct IReturn<'a, O, I: Input, C, S, M: CustomBuilder>(
    pub(crate) Result<(O, ICont<'a, I, C, S, M>), LazyError<I, M>>,
);

/// A parser that can be run only once, like `FnOnce`.
/// It is a parser that owns the value, processes it and returns it. Unlike normal parsers, it is renamed `map_mv` because it only needs to pass FnOnce for map and bind.
/// `I` is a stream with position, `C` is an immutable value that the whole parser refers to (an argument if it's a function), `S` is a variable value that the whole parser refers to (like a global variable), and `M` is a type for defining your own errors.
/// See Output type for the function of each parser.
pub trait ParserOnce<I: Input, Output, C, S, M: CustomBuilder> {
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<Output, I, S, M>;

    fn case_mv<F: FnOnce(Output, ICont<I, C, S, M>) -> IReturn<O, I, C, S, M>, O>(self, f: F) -> Case<Self, F, Output>
    where
        Self: Sized,
    {
        Case(self, f, PhantomData)
    }
    fn map_mv<F: FnOnce(Output) -> O, O>(self, f: F) -> Map<Self, F, Output>
    where
        Self: Sized,
    {
        Map(self, f, PhantomData)
    }
    fn to<O>(self, value: O) -> Value<Self, O, Output>
    where
        Self: Sized,
    {
        Value(self, value, PhantomData)
    }
    fn skip(self) -> Value<Self, (), Output>
    where
        Self: Sized,
    {
        Value(self, (), PhantomData)
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
    fn bind_mv<F: FnOnce(Output) -> P, P: ParserOnce<I, Output2, C, S, M>, Output2>(self, f: F) -> Bind<Self, F, Output>
    where
        Self: Sized,
    {
        Bind(self, f, PhantomData)
    }
    fn and_then_mv<O, F: FnOnce(Output) -> Result<O, Builder<M>>>(self, f: F) -> AndThen<Self, F, Output>
    where
        Self: Sized,
    {
        AndThen(self, f, PhantomData)
    }
    fn many_mv_with<O, F: FnOnce(ParserIterator<Self, I, Output, C, S, M>) -> O>(
        self, f: F,
    ) -> ManyWith<Self, F, Output>
    where
        Self: Sized,
    {
        ManyWith(self, f, PhantomData)
    }
    fn many_mv_then<O, F: FnOnce(ParserIterator<Self, I, Output, C, S, M>) -> Result<O, Builder<M>>>(
        self, f: F,
    ) -> ManyThen<Self, F, Output>
    where
        Self: Sized,
    {
        ManyThen(self, f, PhantomData)
    }
    fn sep_mv_with<
        O,
        F: FnOnce(ParserSepIterator<Self, P, I, Output, Output2, C, S, M>) -> O,
        P: Parser<I, Output2, C, S, M>,
        Output2,
    >(
        self, sep: P, f: F,
    ) -> SepWith<Self, P, F, Output, Output2>
    where
        Self: Sized,
    {
        SepWith(self, sep, f, PhantomData)
    }
    fn sep_mv_then<
        O,
        F: FnOnce(ParserSepIterator<Self, P, I, Output, Output2, C, S, M>) -> Result<O, Builder<M>>,
        P: Parser<I, Output2, C, S, M>,
        Output2,
    >(
        self, sep: P, f: F,
    ) -> SepThen<Self, P, F, Output, Output2>
    where
        Self: Sized,
    {
        SepThen(self, sep, f, PhantomData)
    }
    #[inline]
    fn and<P: ParserOnce<I, Output2, C, S, M>, Output2>(self, other: P) -> And<Self, P>
    where
        Self: Sized,
    {
        And(self, other)
    }
    #[inline]
    fn left<P: ParserOnce<I, Output2, C, S, M>, Output2>(self, other: P) -> Left<Self, P, Output2>
    where
        Self: Sized,
    {
        Left(self, other, PhantomData)
    }
    #[inline]
    fn right<P: ParserOnce<I, Output2, C, S, M>, Output2>(self, other: P) -> Right<Self, P, Output>
    where
        Self: Sized,
    {
        Right(self, other, PhantomData)
    }
    #[inline]
    fn or_not(self) -> OrNot<Self>
    where
        Self: Sized,
    {
        OrNot(self)
    }
    #[inline]
    fn between<P1: ParserOnce<I, Output1, C, S, M>, P2: ParserOnce<I, Output2, C, S, M>, Output1, Output2>(
        self, left: P1, right: P2,
    ) -> Between<P1, Self, P2, Output1, Output2>
    where
        Self: Sized,
    {
        Between(left, self, right, PhantomData)
    }
    #[inline]
    fn or<P: ParserOnce<I, Output, C, S, M>>(self, other: P) -> Or<Self, P>
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
    #[inline]
    fn get_str<O: FromIterator<I::Item>>(self) -> GetString<Self, O>
    where
        Self: Sized,
    {
        GetString(self, PhantomData)
    }
    #[inline]
    fn get_str_extend<O: Extend<I::Item>>(self, value: O) -> GetStringExtend<Self, O>
    where
        Self: Sized,
    {
        GetStringExtend(self, value)
    }
}

/// A parser that can be used again and again.
/// There is no equivalent to FnMut to prevent it from becoming an unintentionally destructive parser.
/// See Output type for the function of each parser.
pub trait Parser<I: Input, Output, C, S, M: CustomBuilder>: ParserOnce<I, Output, C, S, M> {
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<Output, I, S, M>;

    fn to_ref(&self) -> RefParser<Self>
    where
        Self: Sized,
    {
        RefParser(&self)
    }
    fn case<F: Fn(Output, ICont<I, C, S, M>) -> IReturn<O, I, C, S, M>, O>(self, f: F) -> Case<Self, F, Output>
    where
        Self: Sized,
    {
        Case(self, f, PhantomData)
    }
    fn map<F: Fn(Output) -> O, O>(self, f: F) -> Map<Self, F, Output>
    where
        Self: Sized,
    {
        Map(self, f, PhantomData)
    }
    fn bind<F: Fn(Output) -> P, P: ParserOnce<I, O, C, S, M>, O>(self, f: F) -> Bind<Self, F, Output>
    where
        Self: Sized,
    {
        Bind(self, f, PhantomData)
    }
    fn and_then<O, F: Fn(Output) -> Result<O, Builder<M>>>(self, f: F) -> AndThen<Self, F, Output>
    where
        Self: Sized,
    {
        AndThen(self, f, PhantomData)
    }
    fn fold<T, F: Fn(T, Output) -> T>(self, init: T, f: F) -> Fold<T, Self, F, Output>
    where
        Self: Sized,
    {
        fold(init, self, f)
    }
    fn fold1<T, F: Fn(T, Output) -> T>(self, init: T, f: F) -> Fold1<T, Self, F, Output>
    where
        Self: Sized,
    {
        fold1(init, self, f)
    }
    fn sep_fold<T, F: Fn(T, Output) -> T, P: Parser<I, Output2, C, S, M>, Output2>(
        self, init: T, sep: P, f: F,
    ) -> SepFold<T, Self, P, F, Output, Output2>
    where
        Self: Sized,
    {
        SepFold { init, p: self, sep, succ: f, _marker: PhantomData }
    }
    fn sep_fold1<F: Fn(Output, Output2, Output) -> Output, P: Parser<I, Output2, C, S, M>, Output2>(
        self, sep: P, f: F,
    ) -> SepFold1<Self, P, F, Output2>
    where
        Self: Sized,
    {
        SepFold1 { p: self, sep, succ: f, _marker: PhantomData }
    }
    fn extend<B>(self, value: B) -> ExtendParser<B, Self, Output>
    where
        Self: Sized,
    {
        ExtendParser(value, self, PhantomData)
    }
    fn extend1<O>(self, value: O) -> Extend1Parser<O, Self, Output>
    where
        Self: Sized,
    {
        Extend1Parser(value, self, PhantomData)
    }
    fn sep_extend<T: Extend<Output>, P: Parser<I, Output2, C, S, M>, Output2>(
        self, init: T, sep: P,
    ) -> SepExtend<T, Self, P, Output, Output2>
    where
        Self: Sized,
    {
        SepExtend { init, p: self, sep, _marker: PhantomData }
    }
    fn sep_extend1<T: Extend<Output>, P: Parser<I, Output2, C, S, M>, Output2>(
        self, init: T, sep: P,
    ) -> SepExtend1<T, Self, P, Output, Output2>
    where
        Self: Sized,
    {
        SepExtend1 { init, p: self, sep, _marker: PhantomData }
    }
    fn skip_many(self) -> ExtendParser<(), Value<Self, (), Output>, ()>
    where
        Self: Sized,
    {
        self.to(()).extend(())
    }
    fn skip_many1(self) -> Extend1Parser<(), Value<Self, (), Output>, ()>
    where
        Self: Sized,
    {
        self.to(()).extend1(())
    }
    fn many<B: FromIterator<Output>>(self) -> Many<Self, B, Output>
    where
        Self: Sized,
    {
        many(self)
    }
    fn many1<B: FromIterator<Output>>(self) -> Many1<Self, B, Output>
    where
        Self: Sized,
    {
        many1(self)
    }
    fn many_with<O, F: Fn(ParserIterator<Self, I, Output, C, S, M>) -> O>(self, f: F) -> ManyWith<Self, F, Output>
    where
        Self: Sized,
    {
        ManyWith(self, f, PhantomData)
    }
    fn many_then<O, F: Fn(ParserIterator<Self, I, Output, C, S, M>) -> Result<O, Builder<M>>>(
        self, f: F,
    ) -> ManyThen<Self, F, Output>
    where
        Self: Sized,
    {
        ManyThen(self, f, PhantomData)
    }
    fn sep<B: FromIterator<Output>, P: Parser<I, Output2, C, S, M>, Output2>(
        self, sep: P,
    ) -> Sep<Self, P, B, Output, Output2>
    where
        Self: Sized,
    {
        Sep(self, sep, PhantomData)
    }
    fn sep1<B: FromIterator<Output>, P: Parser<I, Output2, C, S, M>, Output2>(
        self, sep: P,
    ) -> Sep1<Self, P, B, Output, Output2>
    where
        Self: Sized,
    {
        Sep1(self, sep, PhantomData)
    }
    fn sep_with<
        O,
        F: Fn(ParserSepIterator<Self, P, I, Output, Output2, C, S, M>) -> O,
        P: Parser<I, Output2, C, S, M>,
        Output2,
    >(
        self, sep: P, f: F,
    ) -> SepWith<Self, P, F, Output, Output2>
    where
        Self: Sized,
    {
        SepWith(self, sep, f, PhantomData)
    }
    fn sep_then<
        O,
        F: Fn(ParserSepIterator<Self, P, I, Output, Output2, C, S, M>) -> Result<O, Builder<M>>,
        P: Parser<I, Output2, C, S, M>,
        Output2,
    >(
        self, sep: P, f: F,
    ) -> SepThen<Self, P, F, Output, Output2>
    where
        Self: Sized,
    {
        SepThen(self, sep, f, PhantomData)
    }
    fn repeat<O: FromIterator<Output>, N: RangeWithOrd<usize>>(self, count: N) -> Repeat<Self, O, Output>
    where
        Self: Sized,
    {
        take(self, count)
    }
}

pub trait SimpleParser<I: Input, Output, M: CustomBuilder>: ParserOnce<I, Output, (), (), M> + Sized {
    fn parse<In: IntoInput<IntoI = I>>(self, input: In) -> Result<Output, Error<I, M::To>> {
        self.run_once(ICont {
            ok: IOk { input: input.into_input(), state: (), err: None, cutted: false },
            config: &(),
            drop: &mut || {},
        })
        .map(|(o, _)| o)
        .map_err(|e| e.map(|e| e.calc()))
    }
    fn test<In: IntoInput<IntoI = I>>(self, input: In) -> bool {
        self.run_once(ICont {
            ok: IOk { input: input.into_input(), state: (), err: None, cutted: false },
            config: &(),
            drop: &mut || {},
        })
        .is_ok()
    }
}
impl<I: Input, M: CustomBuilder, Output, P: ParserOnce<I, Output, (), (), M>> SimpleParser<I, Output, M> for P {}

/// Trait to parse a &str easily.
pub trait EasyParser<I: Input, Output>: ParserOnce<I, Output, (), (), Nil> + Sized {
    /// Returns the result or error string, useful for testing.
    fn parse_easy<In: IntoInput<IntoI = I>>(self, input: In) -> Result<Output, String>
    where
        I::Pos: Display,
    {
        self.run_once(ICont {
            ok: IOk { input: input.into_input(), state: (), err: None, cutted: false },
            config: &(),
            drop: &mut || {},
        })
        .map(|(o, _)| o)
        .map_err(|e| format!("{}", e.map(|e| e.calc())))
    }
    /// Returns the result or not, useful for testing.
    fn parse_ok<In: IntoInput<IntoI = I>>(self, input: In) -> Option<Output>
    where
        I::Pos: Display,
    {
        self.run_once(ICont {
            ok: IOk { input: input.into_input(), state: (), err: None, cutted: false },
            config: &(),
            drop: &mut || {},
        })
        .map(|(o, _)| o)
        .ok()
    }
    fn test_nil<In: IntoInput<IntoI = I>>(self, input: In) -> bool {
        self.run_once(ICont {
            ok: IOk { input: input.into_input(), state: (), err: None, cutted: false },
            config: &(),
            drop: &mut || {},
        })
        .is_ok()
    }
}
impl<I: Input, Output, P: ParserOnce<I, Output, (), (), Nil>> EasyParser<I, Output> for P {}
