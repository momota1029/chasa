use std::{fmt::Display, fmt::Write, hash::Hash, marker::PhantomData};

use crate::{
    combi::OrWith,
    input::InputOnce,
    many::{ManyBind, SepBind},
};

use super::{
    combi::{
        And, AndThen, Between, Bind, Case, Cut, GetString, GetStringExtend, Label, LabelWith, Left, Map, Or, OrNot,
        Ranged, Right, Value,
    },
    cont::Cont,
    error::{self, ParseError, StdParseError, StdParseErrorFor},
    fold::{
        fold, fold1, Extend1Parser, ExtendParser, Fold, Fold1, SepExtend, SepExtend1, SepFold, SepFold1, SepReduce,
    },
    input::{PositionPrinter, Save},
    many::{many, many1, take, Many, Many1, ManyIterator, ManyMap, Repeat, Sep, Sep1, SepIterator, SepMap},
    prim::MutParser,
    util::{Consume, RangeWithOrd},
};

pub struct Args<'a, 'b, I: InputOnce, E: ParseError<I>, C, S> {
    pub input: &'a mut I,
    pub config: &'a C,
    pub state: &'a mut S,
    pub consume: &'a mut Consume<'b>,
    pub error: &'a mut E,
}
impl<'a, 'b, I: InputOnce, E: ParseError<I>, C, S> Args<'a, 'b, I, E, C, S> {
    #[inline(always)]
    pub fn by_ref<'c>(&'c mut self) -> Args<'c, 'b, I, E, C, S> {
        let Args { input, config, state, consume, error } = self;
        Args { input, config, state, consume, error }
    }
    #[inline(always)]
    pub fn uncons(&mut self) -> Option<(I::Token, I::Position)> {
        let pos = self.input.position();
        match self.input.uncons() {
            Ok(c) => Some((c, pos)),
            Err(e) => {
                if self.error.add(None, pos) {
                    self.error.set(e.into());
                }
                None
            },
        }
    }
}

pub trait ParserOnce<
    I: InputOnce,
    O,
    E: ParseError<I> = Option<StdParseError<<I as InputOnce>::Token, <I as InputOnce>::Position>>,
    C = (),
    S = (),
>
{
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O>;

    #[inline(always)]
    fn case_once<F: for<'a, 'b> FnOnce(O, Args<'a, 'b, I, E, C, S>) -> Cont<'a, 'b, I, O2, E, C, S>, O2>(
        self, f: F,
    ) -> Case<Self, F, O, E>
    where
        Self: Sized,
    {
        Case(self, f, PhantomData)
    }
    #[inline(always)]
    fn and_then_once<F: FnOnce(O) -> Result<O2, M>, O2, M>(self, f: F) -> AndThen<Self, F, M, O>
    where
        Self: Sized,
    {
        AndThen(self, f, PhantomData)
    }
    #[inline(always)]
    fn map_once<F: FnOnce(O) -> O2, O2>(self, f: F) -> Map<Self, F, O>
    where
        Self: Sized,
    {
        Map(self, f, PhantomData)
    }
    #[inline(always)]
    fn to<O2>(self, value: O2) -> Value<Self, O2, O>
    where
        Self: Sized,
    {
        Value(self, value, PhantomData)
    }
    #[inline(always)]
    fn skip(self) -> Value<Self, (), O>
    where
        Self: Sized,
    {
        Value(self, (), PhantomData)
    }
    #[inline(always)]
    fn label<L>(self, label: L) -> Label<Self, L>
    where
        Self: Sized,
        E::Message: From<error::Expected<error::Format<L>>>,
    {
        Label(self, label)
    }
    #[inline(always)]
    fn label_with<F: FnMut() -> L, L>(self, label: F) -> LabelWith<Self, F>
    where
        Self: Sized,
        E::Message: From<error::Expected<error::Format<L>>>,
    {
        LabelWith(self, label)
    }
    #[inline(always)]
    fn bind_once<F: FnOnce(O) -> P, P: ParserOnce<I, O2, E, C, S>, O2>(self, f: F) -> Bind<Self, F, O>
    where
        Self: Sized,
    {
        Bind(self, f, PhantomData)
    }
    #[inline(always)]
    fn or<P: ParserOnce<I, O, E, C, S>>(self, other: P) -> Or<Self, P>
    where
        Self: Sized,
    {
        Or(self, other)
    }
    #[inline(always)]
    fn or_not(self) -> OrNot<Self>
    where
        Self: Sized,
    {
        OrNot(self)
    }
    #[inline(always)]
    fn or_with_once<R, F: FnOnce(R) -> Q, Q: ParserOnce<I, O, E, C, S>>(self, resource: R, f: F) -> OrWith<Self, R, F>
    where
        Self: Sized,
    {
        OrWith(self, resource, f)
    }
    #[inline(always)]
    fn between<P1: ParserOnce<I, Output1, E, C, S>, P2: ParserOnce<I, Output2, E, C, S>, Output1, Output2>(
        self, left: P1, right: P2,
    ) -> Between<P1, Self, P2, Output1, Output2>
    where
        Self: Sized,
    {
        Between(left, self, right, PhantomData)
    }
    #[inline(always)]
    fn cut(self) -> Cut<Self>
    where
        Self: Sized,
    {
        Cut(self)
    }
    #[inline(always)]
    fn ranged(self) -> Ranged<Self>
    where
        Self: Sized,
    {
        Ranged(self)
    }
    #[inline(always)]
    fn and<P: ParserOnce<I, O2, E, C, S>, O2>(self, other: P) -> And<Self, P>
    where
        Self: Sized,
    {
        And(self, other)
    }
    #[inline(always)]
    fn left<P: ParserOnce<I, O2, E, C, S>, O2>(self, other: P) -> Left<Self, P, O2>
    where
        Self: Sized,
    {
        Left(self, other, PhantomData)
    }
    #[inline(always)]
    fn right<P: ParserOnce<I, O2, E, C, S>, O2>(self, other: P) -> Right<Self, P, O>
    where
        Self: Sized,
    {
        Right(self, other, PhantomData)
    }
    #[inline(always)]
    fn get_str<B: FromIterator<I::Token>>(self) -> GetString<Self, B>
    where
        Self: Sized,
    {
        GetString(self, PhantomData)
    }
    #[inline(always)]
    fn get_str_extend<B: Extend<I::Token>>(self, value: B) -> GetStringExtend<Self, B>
    where
        Self: Sized,
    {
        GetStringExtend(self, value)
    }
}

pub trait Parser<
    I: InputOnce,
    O,
    E: ParseError<I> = Option<StdParseError<<I as InputOnce>::Token, <I as InputOnce>::Position>>,
    C = (),
    S = (),
>: ParserOnce<I, O, E, C, S>
{
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O>;

    #[inline(always)]
    fn by_ref(&mut self) -> MutParser<Self>
    where
        Self: Sized,
    {
        MutParser(self)
    }
    #[inline(always)]
    fn or_with<R, F: FnMut(R) -> Q, Q: ParserOnce<I, O, E, C, S>>(self, resource: R, f: F) -> OrWith<Self, R, F>
    where
        Self: Sized,
    {
        OrWith(self, resource, f)
    }
    #[inline(always)]
    fn case<F: for<'a, 'b> FnMut(O, Args<'a, 'b, I, E, C, S>) -> Cont<'a, 'b, I, O2, E, C, S>, O2>(
        self, f: F,
    ) -> Case<Self, F, O, E>
    where
        Self: Sized,
    {
        Case(self, f, PhantomData)
    }
    #[inline(always)]
    fn and_then<F: FnMut(O) -> Result<O2, M>, O2, M>(self, f: F) -> AndThen<Self, F, M, O>
    where
        Self: Sized,
    {
        AndThen(self, f, PhantomData)
    }
    #[inline(always)]
    fn map<F: FnMut(O) -> O2, O2>(self, f: F) -> Map<Self, F, O>
    where
        Self: Sized,
    {
        Map(self, f, PhantomData)
    }
    #[inline(always)]
    fn bind<F: FnMut(O) -> P, P: ParserOnce<I, O2, E, C, S>, O2>(self, f: F) -> Bind<Self, F, O>
    where
        Self: Sized,
    {
        Bind(self, f, PhantomData)
    }
    #[inline(always)]
    fn fold<T, F: FnMut(T, O) -> T>(self, init: T, f: F) -> Fold<T, Self, F, O>
    where
        Self: Sized,
    {
        fold(init, self, f)
    }
    #[inline(always)]
    fn fold1<T, F: FnMut(T, O) -> T>(self, init: T, f: F) -> Fold1<T, Self, F, O>
    where
        Self: Sized,
    {
        fold1(init, self, f)
    }
    #[inline(always)]
    fn sep_fold<T, F: FnMut(T, O) -> T, P: Parser<I, U, E, C, S>, U>(
        self, init: T, sep: P, f: F,
    ) -> SepFold<T, Self, P, F, O, U>
    where
        Self: Sized,
    {
        SepFold { init, p: self, sep, succ: f, _marker: PhantomData }
    }
    #[inline(always)]
    fn sep_reduce<T, F: FnMut(O, T, O) -> O, P: Parser<I, T, E, C, S>, U>(
        self, sep: P, f: F,
    ) -> SepReduce<Self, P, F, U>
    where
        Self: Sized,
    {
        SepReduce { item: self, sep, accum: f, _marker: PhantomData }
    }
    #[inline(always)]
    fn sep_fold1<T, F: FnMut(T, O) -> T, P: Parser<I, U, E, C, S>, U>(
        self, init: T, sep: P, f: F,
    ) -> SepFold1<T, Self, P, F, O, U>
    where
        Self: Sized,
    {
        SepFold1 { init, p: self, sep, succ: f, _marker: PhantomData }
    }
    #[inline(always)]
    fn extend<T>(self, value: T) -> ExtendParser<T, Self, O>
    where
        Self: Sized,
    {
        ExtendParser(value, self, PhantomData)
    }
    #[inline(always)]
    fn extend1<T>(self, value: T) -> Extend1Parser<T, Self, O>
    where
        Self: Sized,
    {
        Extend1Parser(value, self, PhantomData)
    }
    #[inline(always)]
    fn sep_extend<T: Extend<O>, P: Parser<I, U, E, C, S>, U>(self, init: T, sep: P) -> SepExtend<T, Self, P, O, U>
    where
        Self: Sized,
    {
        SepExtend { init, p: self, sep, _marker: PhantomData }
    }
    #[inline(always)]
    fn sep_extend1<T: Extend<O>, P: Parser<I, U, E, C, S>, U>(self, init: T, sep: P) -> SepExtend1<T, Self, P, O, U>
    where
        Self: Sized,
    {
        SepExtend1 { init, p: self, sep, _marker: PhantomData }
    }
    #[inline]
    fn skip_many(self) -> ExtendParser<(), Value<Self, (), O>, ()>
    where
        Self: Sized,
        I: Save,
        S: Save,
    {
        self.to(()).extend(())
    }
    #[inline]
    fn skip_many1(self) -> Extend1Parser<(), Value<Self, (), O>, ()>
    where
        Self: Sized,
        I: Save,
        S: Save,
    {
        self.to(()).extend1(())
    }
    #[inline]
    fn many<T: FromIterator<O>>(self) -> Many<Self, T, O>
    where
        Self: Sized,
        I: Save,
        S: Save,
    {
        many(self)
    }
    #[inline]
    fn many1<T: FromIterator<O>>(self) -> Many1<Self, T, O>
    where
        Self: Sized,
        I: Save,
        S: Save,
    {
        many1(self)
    }
    #[inline]
    fn many_map<T, F: FnMut(ManyIterator<Self, I, O, E, C, S>) -> T>(self, f: F) -> ManyMap<Self, F, O>
    where
        Self: Sized,
    {
        ManyMap(self, f, PhantomData)
    }
    #[inline]
    fn many_map_once<T, F: FnOnce(ManyIterator<Self, I, O, E, C, S>) -> T>(self, f: F) -> ManyMap<Self, F, O>
    where
        Self: Sized,
    {
        ManyMap(self, f, PhantomData)
    }
    #[inline]
    fn many_bind<O2, P: ParserOnce<I, O2, E, C, S>, F: FnMut(ManyIterator<Self, I, O, E, C, S>) -> P>(
        self, f: F,
    ) -> ManyBind<Self, F, O>
    where
        Self: Sized,
    {
        ManyBind(self, f, PhantomData)
    }
    #[inline]
    fn many_bind_once<O2, P: ParserOnce<I, O2, E, C, S>, F: FnOnce(ManyIterator<Self, I, O, E, C, S>) -> P>(
        self, f: F,
    ) -> ManyBind<Self, F, O>
    where
        Self: Sized,
    {
        ManyBind(self, f, PhantomData)
    }
    #[inline]
    fn sep<T: FromIterator<O>, P: Parser<I, U, E, C, S>, U>(self, sep: P) -> Sep<Self, P, T, O, U>
    where
        Self: Sized,
    {
        Sep(self, sep, PhantomData)
    }
    #[inline]
    fn sep1<T: FromIterator<O>, P: Parser<I, U, E, C, S>, U>(self, sep: P) -> Sep1<Self, P, T, O, U>
    where
        Self: Sized,
    {
        Sep1(self, sep, PhantomData)
    }
    #[inline]
    fn sep_map<T, F: FnMut(SepIterator<Self, P, I, O, U, E, C, S>) -> T, P: Parser<I, U, E, C, S>, U>(
        self, sep: P, f: F,
    ) -> SepMap<Self, P, F, O, U>
    where
        Self: Sized,
    {
        SepMap(self, sep, f, PhantomData)
    }
    #[inline]
    fn sep_map_once<T, F: FnOnce(SepIterator<Self, P, I, O, U, E, C, S>) -> T, P: Parser<I, U, E, C, S>, U>(
        self, sep: P, f: F,
    ) -> SepMap<Self, P, F, O, U>
    where
        Self: Sized,
    {
        SepMap(self, sep, f, PhantomData)
    }
    #[inline]
    fn sep_bind<
        O2,
        F: FnMut(SepIterator<Self, P, I, O, T, E, C, S>) -> Q,
        P: Parser<I, T, E, C, S>,
        T,
        Q: ParserOnce<I, O2, E, C, S>,
    >(
        self, sep: P, f: F,
    ) -> SepBind<Self, P, F, O, T>
    where
        Self: Sized,
    {
        SepBind(self, sep, f, PhantomData)
    }
    #[inline]
    fn sep_bind_once<
        O2,
        F: FnOnce(SepIterator<Self, P, I, O, T, E, C, S>) -> Q,
        P: Parser<I, T, E, C, S>,
        T,
        Q: ParserOnce<I, O2, E, C, S>,
    >(
        self, sep: P, f: F,
    ) -> SepBind<Self, P, F, O, T>
    where
        Self: Sized,
    {
        SepBind(self, sep, f, PhantomData)
    }
    #[inline(always)]
    fn repeat<T: FromIterator<O>, N: RangeWithOrd<usize>>(self, count: N) -> Repeat<Self, T, O>
    where
        Self: Sized,
    {
        take(self, count)
    }
}

impl<P: ParserOnce<I, O>, I: InputOnce, O> Pat<I, O> for P
where
    I::Token: Hash + Eq,
    StdParseErrorFor<I::Token>: From<I::Message>,
{
}
pub trait Pat<I: InputOnce, O>: ParserOnce<I, O>
where
    I::Token: Hash + Eq,
    StdParseErrorFor<I::Token>: From<I::Message>,
{
    #[inline(always)]
    fn parse_ok(self, mut input: I) -> Option<O>
    where
        Self: Sized,
    {
        let mut error: Option<StdParseError<_, _>> = None;
        self.run_once(Args {
            input: &mut input,
            config: &().into(),
            state: &mut ().into(),
            consume: &mut Consume::new(),
            error: &mut error,
        })
    }

    #[inline(always)]
    fn parse_easy(self, mut input: I) -> Result<O, String>
    where
        Self: Sized,
        I::Token: Display,
        I::Position: PositionPrinter,
    {
        let mut error = <Option<StdParseError<_, _>> as ParseError<I>>::new();
        match self.run_once(Args {
            input: &mut input,
            config: &().into(),
            state: &mut ().into(),
            consume: &mut Consume::new(),
            error: &mut error,
        }) {
            Some(o) => Ok(o),
            None => {
                let error = error.unwrap();
                let mut errors = error.iter();
                let mut str = String::new();
                if let Some(line) = errors.next() {
                    write!(&mut str, "{}", line).unwrap();
                    for line in errors {
                        write!(&mut str, "\n{}", line).unwrap();
                    }
                }
                Err(str)
            },
        }
    }

    #[inline(always)]
    fn parse(
        self, input: &mut I,
    ) -> Result<O, Option<StdParseError<<I as InputOnce>::Token, <I as InputOnce>::Position>>>
    where
        Self: Sized,
    {
        let mut error = <Option<StdParseError<_, _>> as ParseError<I>>::new();
        match self.run_once(Args {
            input,
            config: &().into(),
            state: &mut ().into(),
            consume: &mut Consume::new(),
            error: &mut error,
        }) {
            Some(o) => Ok(o),
            None => Err(error),
        }
    }
}
