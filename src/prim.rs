use std::{
    fmt::Display,
    marker::PhantomData,
    ops::{Bound, RangeBounds},
};

use either::Either;

use super::{
    cont::Cont,
    error::{self, expected, unexpected, EndOfInput, Expected, ParseError, Unexpected},
    input::Input,
    parser::{Args, Parser, ParserOnce},
    util::{CharsOrRange, IntoChars},
};

impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>, F: FnOnce() -> P>
    ParserOnce<I, O, E, C, S> for F
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        self().run_once(args)
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>, F: FnMut() -> P> Parser<I, O, E, C, S>
    for F
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        self().run_once(args)
    }
}

impl<I: Input, O, E: ParseError<I>, C, S: Clone, P1: ParserOnce<I, O, E, C, S>, P2: ParserOnce<I, O, E, C, S>>
    ParserOnce<I, O, E, C, S> for Either<P1, P2>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        match self {
            Either::Left(left) => left.run_once(args),
            Either::Right(right) => right.run_once(args),
        }
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P1: Parser<I, O, E, C, S>, P2: Parser<I, O, E, C, S>>
    Parser<I, O, E, C, S> for Either<P1, P2>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        match self {
            Either::Left(left) => left.run(args),
            Either::Right(right) => right.run(args),
        }
    }
}

/// Make a function a parser such that the parser is applied chainwise to get the result.
/// See also `tail_rec` and `case` if you feel compelled to use this.
/// # Example
/// ```
/// use chasa::prelude::*;
/// fn p<'a>() -> impl Pat<&'a str, String> {
///     // Note the reverse order
///     parser_once(|k| k.then(any.and(p).map(|(x,mut xs)| {xs.push(x); xs}).or(pure(String::new()))))
/// }
/// assert_eq!(p.parse_ok("abcd"), Some("dcba".to_string()));
/// assert_eq!(p.parse_ok("stressed"), Some("desserts".to_string()));
///
/// use std::ops::ControlFlow::*;
/// // Straightforward implementation without wasting stacks
/// let q = tail_rec(vec![], |mut xs| any.or_not().map_once(move |x| match x {
///     Some(x) => { xs.push(x); Continue(xs) },
///     None => Break(xs)
/// })).map(|xs| xs.iter().rev().collect::<String>());
/// assert_eq!(q.parse_ok("stressed"), Some("desserts".to_string()));
/// ```
#[derive(Clone, Copy)]
pub struct FnParser<F>(F);
#[inline]
pub fn parser<
    I: Input,
    O,
    E: ParseError<I>,
    C,
    S: Clone,
    F: for<'a, 'b> FnMut(Args<'a, 'b, I, E, C, S>) -> Cont<'a, 'b, I, O, E, C, S>,
>(
    f: F,
) -> FnParser<F> {
    FnParser(f)
}
#[inline]
pub fn parser_once<
    I: Input,
    O,
    E: ParseError<I>,
    C,
    S: Clone,
    F: for<'a, 'b> FnOnce(Args<'a, 'b, I, E, C, S>) -> Cont<'a, 'b, I, O, E, C, S>,
>(
    f: F,
) -> FnParser<F> {
    FnParser(f)
}
impl<
        I: Input,
        O,
        E: ParseError<I>,
        C,
        S: Clone,
        F: for<'a, 'b> FnOnce(Args<'a, 'b, I, E, C, S>) -> Cont<'a, 'b, I, O, E, C, S>,
    > ParserOnce<I, O, E, C, S> for FnParser<F>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        Some(self.0(args).0?.0)
    }
}
impl<
        I: Input,
        O,
        E: ParseError<I>,
        C,
        S: Clone,
        F: for<'a, 'b> FnMut(Args<'a, 'b, I, E, C, S>) -> Cont<'a, 'b, I, O, E, C, S>,
    > Parser<I, O, E, C, S> for FnParser<F>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        Some(self.0(args).0?.0)
    }
}

/// Returns a reference to a new parser without consuming the parser like `by_ref` in Iterator.
pub struct MutParser<'a, P>(pub(crate) &'a mut P);
impl<'a, I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>> ParserOnce<I, O, E, C, S>
    for MutParser<'a, P>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.run(args)
    }
}
impl<'a, I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>> Parser<I, O, E, C, S>
    for MutParser<'a, P>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.0.run(args)
    }
}

/// A parser that returns its arguments as is.
#[derive(Clone, Copy)]
pub struct Pure<O>(O);
pub fn pure<O>(o: O) -> Pure<O> {
    Pure(o)
}
impl<O, I: Input, E: ParseError<I>, C, S: Clone> ParserOnce<I, O, E, C, S> for Pure<O> {
    #[inline(always)]
    fn run_once(self, _: Args<I, E, C, S>) -> Option<O> {
        Some(self.0)
    }
}
impl<O: Clone, I: Input, E: ParseError<I>, C, S: Clone> Parser<I, O, E, C, S> for Pure<O> {
    #[inline(always)]
    fn run(&mut self, _: Args<I, E, C, S>) -> Option<O> {
        Some(self.0.clone())
    }
}

// /// A parser that never fails. You can choose between [`error::unexpect`][`crate::error::unexpect`], which prints "Unexpected \[token\]", and [`error::message`][`crate::error::message`], which prints "\[msg\]".
// /// See also [`error::Builder`][`crate::error::Builder`]
// /// # Example
// /// ```
// /// use chasa::prelude::*;
// /// assert_eq!(unexpect::<()>("chasa").parse_easy(""),Err("error: unexpected chasa".to_string()));
// /// ```
// #[derive(Clone)]
// pub struct Unexpect<O>(Cow<'static, str>, PhantomData<fn() -> O>);
// #[inline(always)]
// pub fn unexpect<O>(token: impl Into<Cow<'static, str>>) -> Unexpect<O> {
//     Unexpect(token.into(), PhantomData)
// }
// impl<I: Input, O, E: ParseError<I>, C, S: Clone> ParserOnce<I, O, E, C, S> for Unexpect<O> {
//     #[inline(always)]
//     fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {

//         args.error.set_unexpected_format(args.input.position(), self.0);
//         None
//     }
// }
// impl<I: Input, O, E: ParseError<I>, C, S: Clone> Parser<I, O, E, C, S> for Unexpect<O> {
//     #[inline(always)]
//     fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
//         args.error.set_unexpected_format(args.input.position(), self.0.clone());
//         None
//     }
// }

/// The parser that matches the end of the input.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(eoi.parse_easy(""), Ok(()));
/// assert_eq!(eoi.parse_easy("a2"), Err("error: unexpected a, expecting end of input".to_string()))
/// ```
pub struct EoI<I, E, C, S>(PhantomData<fn() -> (I, E, C, S)>);
impl<I, E, C, S> Clone for EoI<I, E, C, S> {
    #[inline(always)]
    fn clone(&self) -> Self {
        EoI(PhantomData)
    }
}
impl<I, E, C, S> Copy for EoI<I, E, C, S> {}
#[inline(always)]
pub fn eoi<I, E, C, S>() -> EoI<I, E, C, S> {
    EoI(PhantomData)
}
impl<I: Input, E: ParseError<I>, C, S: Clone> ParserOnce<I, (), E, C, S> for EoI<I, E, C, S>
where
    E::Message: From<Unexpected<error::Token<I::Token>>> + From<Expected<EndOfInput>>,
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<()> {
        self.run(args)
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone> Parser<I, (), E, C, S> for EoI<I, E, C, S>
where
    E::Message: From<Unexpected<error::Token<I::Token>>> + From<Expected<EndOfInput>>,
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<()> {
        let pos = args.input.position();
        match args.input.uncons() {
            Err(_) => Some(()),
            Ok(token) => {
                if args.error.add(None, pos) {
                    args.error.set(unexpected(error::token(token)).into());
                    args.error.set(expected(EndOfInput).into());
                }
                None
            },
        }
    }
}

/// A parser that takes a single arbitrary character.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(any.parse_ok("12"), Some('1'));
/// assert_eq!(any.parse_ok(""), None)
/// ```
pub struct Any<I, E, C, S>(PhantomData<fn() -> (I, E, C, S)>);
impl<I, E, C, S> Clone for Any<I, E, C, S> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(PhantomData)
    }
}
impl<I, E, C, S> Copy for Any<I, E, C, S> {}
#[inline(always)]
pub fn any<I: Input, E: ParseError<I>, C, S: Clone>() -> Any<I, E, C, S> {
    Any(PhantomData)
}
impl<I: Input, E: ParseError<I>, C, S: Clone> ParserOnce<I, I::Token, E, C, S> for Any<I, E, C, S> {
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<I::Token> {
        self.run(args)
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone> Parser<I, I::Token, E, C, S> for Any<I, E, C, S> {
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<I::Token> {
        let pos = args.input.position();
        match args.input.uncons() {
            Err(e) => {
                if args.error.add(None, pos) {
                    args.error.set(e.into());
                }
                None
            },
            Ok(token) => {
                args.consume.drop();
                Some(token)
            },
        }
    }
}

/// If the given character matches the input character, it is accepted.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(char('a').parse_easy("a2"), Ok('a'));
/// assert_eq!(char('1').parse_easy("a2"), Err("error: unexpected a, expecting 1".to_string()))
/// ```
pub struct Char<Token, I, E, C, S>(Token, PhantomData<fn() -> (I, E, C, S)>);
impl<Token: Clone, I, E, C, S> Clone for Char<Token, I, E, C, S> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<Token: Copy, I, E, C, S> Copy for Char<Token, I, E, C, S> {}
#[inline(always)]
pub fn char<I: Input, E: ParseError<I>, C, S: Clone, Token: PartialEq<I::Token>>(
    token: Token,
) -> Char<Token, I, E, C, S> {
    Char(token, PhantomData)
}
impl<I: Input, E: ParseError<I>, C, S: Clone, Token: PartialEq<I::Token> + Clone> ParserOnce<I, I::Token, E, C, S>
    for Char<Token, I, E, C, S>
where
    E::Message: From<Unexpected<error::Token<I::Token>>> + From<Expected<error::Token<Token>>>,
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<I::Token> {
        self.run(args)
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone, Token: PartialEq<I::Token> + Clone> Parser<I, I::Token, E, C, S>
    for Char<Token, I, E, C, S>
where
    E::Message: From<Unexpected<error::Token<I::Token>>> + From<Expected<error::Token<Token>>>,
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<I::Token> {
        let (c, pos) = args.uncons()?;
        if self.0 == c {
            args.consume.drop();
            Some(c)
        } else {
            if args.error.add(None, pos) {
                args.error.set(unexpected(error::token(c)).into());
                args.error.set(expected(error::token(self.0.clone())).into())
            }
            None
        }
    }
}

/// It takes one character from the input, compares it with the given iterator, and accepts it if any of the characters match.
///
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(one_of("abc").parse_ok("a2"), Some('a'));
/// // You can also write like this
/// assert_eq!(one_of('a'..='c').parse_ok("a2"), Some('a'));
///
/// assert_eq!(one_of("def").parse_ok("a2"), None)
/// ```
pub struct OneOf<Iter, Item, I, E, C, S>(Iter, PhantomData<fn() -> (Item, I, E, C, S)>);
impl<Iter: Clone, Item, I, E, C, S> Clone for OneOf<Iter, Item, I, E, C, S> {
    #[inline]
    fn clone(&self) -> Self {
        OneOf(self.0.clone(), PhantomData)
    }
}
impl<Iter: Copy, Item, I, E, C, S> Copy for OneOf<Iter, Item, I, E, C, S> {}
#[inline]
pub fn one_of<Iter: CharsOrRange<Item>, Item, I, E, C, S>(chars: Iter) -> OneOf<Iter::To, Item, I, E, C, S> {
    OneOf(chars.to(), PhantomData)
}
impl<I: Input, E: ParseError<I>, C, S: Clone, Item: PartialEq<I::Token>, Iter: IntoChars<Item = Item>>
    Parser<I, I::Token, E, C, S> for OneOf<Iter, Item, I, E, C, S>
where
    Self: Clone,
{
    #[inline]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<I::Token> {
        self.clone().run_once(args)
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone, Item: PartialEq<I::Token>, Iter: IntoChars<Item = Item>>
    ParserOnce<I, I::Token, E, C, S> for OneOf<Iter, Item, I, E, C, S>
{
    #[inline]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<I::Token> {
        let c = args.uncons()?.0;
        if self.0.into_chars().any(|item| item == c) {
            args.consume.drop();
            Some(c)
        } else {
            None
        }
    }
}
impl<I: Input<Token = impl PartialOrd<Item>>, E: ParseError<I>, C, S: Clone, Item: PartialOrd<I::Token>>
    ParserOnce<I, I::Token, E, C, S> for OneOf<(Bound<Item>, Bound<Item>), Item, I, E, C, S>
{
    #[inline]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<I::Token> {
        self.run(args)
    }
}
impl<I: Input<Token = impl PartialOrd<Item>>, E: ParseError<I>, C, S: Clone, Item: PartialOrd<I::Token>>
    Parser<I, I::Token, E, C, S> for OneOf<(Bound<Item>, Bound<Item>), Item, I, E, C, S>
{
    #[inline]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<I::Token> {
        let c = args.uncons()?.0;
        if self.0.contains(&c) {
            args.consume.drop();
            Some(c)
        } else {
            None
        }
    }
}

/// It takes one character from the input, compares it with the given iterator, and only accepts if none of the characters match.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(none_of("abc").parse_ok("a2"), None);
/// assert_eq!(none_of("def").parse_ok("a2"), Some('a'))
/// ```
pub struct NoneOf<Iter, Item, I, E, C, S>(Iter, PhantomData<fn() -> (Item, I, E, C, S)>);
impl<Iter: Clone, Item, I, E, C, S> Clone for NoneOf<Iter, Item, I, E, C, S> {
    #[inline]
    fn clone(&self) -> Self {
        NoneOf(self.0.clone(), PhantomData)
    }
}
impl<Iter: Copy, Item, I, E, C, S> Copy for NoneOf<Iter, Item, I, E, C, S> {}
#[inline]
pub fn none_of<Iter: CharsOrRange<Item>, Item, I, E, C, S>(chars: Iter) -> NoneOf<Iter::To, Item, I, E, C, S> {
    NoneOf(chars.to(), PhantomData)
}
impl<I: Input, E: ParseError<I>, C, S: Clone, Item: PartialEq<I::Token>, Iter: IntoChars<Item = Item>>
    Parser<I, I::Token, E, C, S> for NoneOf<Iter, Item, I, E, C, S>
where
    E::Message: From<Unexpected<error::Token<I::Token>>>,
    Self: Clone,
{
    #[inline]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<I::Token> {
        self.clone().run_once(args)
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone, Item: PartialEq<I::Token>, Iter: IntoChars<Item = Item>>
    ParserOnce<I, I::Token, E, C, S> for NoneOf<Iter, Item, I, E, C, S>
where
    E::Message: From<Unexpected<error::Token<I::Token>>>,
{
    #[inline]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<I::Token> {
        let (c, pos) = args.uncons()?;
        for item in self.0.into_chars() {
            if item == c {
                if args.error.add(None, pos) {
                    args.error.set(error::unexpected(error::token(c)).into())
                }
                return None;
            }
        }
        args.consume.drop();
        Some(c)
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone, Item: PartialOrd<I::Token>> ParserOnce<I, I::Token, E, C, S>
    for NoneOf<(Bound<Item>, Bound<Item>), Item, I, E, C, S>
where
    I::Token: PartialOrd<Item>,
    E::Message: From<Unexpected<error::Token<I::Token>>>,
{
    #[inline]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<I::Token> {
        self.run(args)
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone, Item: PartialOrd<I::Token>> Parser<I, I::Token, E, C, S>
    for NoneOf<(Bound<Item>, Bound<Item>), Item, I, E, C, S>
where
    I::Token: PartialOrd<Item>,
    E::Message: From<Unexpected<error::Token<I::Token>>>,
{
    #[inline]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<I::Token> {
        let (c, pos) = args.uncons()?;
        if !self.0.contains(&c) {
            args.consume.drop();
            Some(c)
        } else {
            if args.error.add(None, pos) {
                args.error.set(error::unexpected(error::token(c)).into())
            }
            None
        }
    }

    fn by_ref(&mut self) -> MutParser<Self>
    where
        Self: Sized,
    {
        MutParser(self)
    }
}

/// A parser that compares character iterators and input as they are consumed together, and accepts them if they all match.
/// The return value is empty, as most of the time it should be intended to be a token.
/// If you want the whole string, use `.to([string])`(see [`Value`][`crate::combi::Value`]) or `.get_str()`(see [`GetString`][`crate::combi::GetString`]).
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(str("a").parse_ok("a2"), Some(()));
/// assert_eq!(str("a2").parse_ok("a2"), Some(()));
/// assert_eq!(str("a23").parse_ok("a2"), None);
/// assert_eq!(str("a3").to(1).or(str("a").to(2)).parse_ok("a2"), Some(2));
/// ```
pub struct String<Iter, I, E, C, S>(Iter, PhantomData<fn() -> (I, E, C, S)>);
impl<Iter: Clone, I, E, C, S> Clone for String<Iter, I, E, C, S> {
    #[inline]
    fn clone(&self) -> Self {
        String(self.0.clone(), PhantomData)
    }
}
impl<Iter: Copy, I, E, C, S> Copy for String<Iter, I, E, C, S> {}
pub fn str<Iter: IntoChars<Item = I::Token>, I: Input, E, C, S>(iter: Iter) -> String<Iter, I, E, C, S> {
    String(iter, PhantomData)
}
impl<
        I: Input,
        E: ParseError<I>,
        C,
        S: Clone,
        Iter: IntoChars<Item = impl PartialEq<I::Token> + Display + 'static> + Clone,
    > Parser<I, (), E, C, S> for String<Iter, I, E, C, S>
where
    E::Message: From<Unexpected<error::Token<I::Token>>>,
{
    #[inline]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<()> {
        self.clone().run_once(args)
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone, Iter: IntoChars<Item = impl PartialEq<I::Token> + Display + 'static>>
    ParserOnce<I, (), E, C, S> for String<Iter, I, E, C, S>
where
    E::Message: From<Unexpected<error::Token<I::Token>>>,
{
    #[inline]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<()> {
        for item in self.0.into_chars() {
            let (c, pos) = args.uncons()?;
            if item != c {
                if args.error.add(None, pos) {
                    args.error.set(error::unexpected(error::token(c)).into())
                }
                return None;
            }
        }
        args.consume.drop();
        Some(())
    }
}

/// A parser that takes a single character satisfying a condition.
pub struct Satisfy<F, I, E, C, S>(F, PhantomData<fn() -> (I, E, C, S)>);
impl<F: Clone, I, E, C, S> Clone for Satisfy<F, I, E, C, S> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Satisfy(self.0.clone(), PhantomData)
    }
}
impl<F: Copy, I, E, C, S> Copy for Satisfy<F, I, E, C, S> {}
#[inline(always)]
pub fn satisfy<I: Input, E: ParseError<I>, C, S: Clone, F: FnMut(&I::Token) -> bool>(f: F) -> Satisfy<F, I, E, C, S> {
    Satisfy(f, PhantomData)
}
#[inline(always)]
pub fn satisfy_once<I: Input, E: ParseError<I>, C, S: Clone, F: FnOnce(&I::Token) -> bool>(
    f: F,
) -> Satisfy<F, I, E, C, S> {
    Satisfy(f, PhantomData)
}
impl<I: Input, E: ParseError<I>, C, S: Clone, F: FnOnce(&I::Token) -> bool> ParserOnce<I, I::Token, E, C, S>
    for Satisfy<F, I, E, C, S>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<I::Token> {
        let c = args.uncons()?.0;
        if self.0(&c) {
            args.consume.drop();
            Some(c)
        } else {
            None
        }
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone, F: FnMut(&I::Token) -> bool> Parser<I, I::Token, E, C, S>
    for Satisfy<F, I, E, C, S>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<I::Token> {
        let c = args.uncons()?.0;
        if self.0(&c) {
            args.consume.drop();
            Some(c)
        } else {
            None
        }
    }
}

/// A parser that takes a single character satisfying a condition.
pub struct SatisfyMap<F, I, E, C, S>(F, PhantomData<fn() -> (I, E, C, S)>);
impl<F: Clone, I, E, C, S> Clone for SatisfyMap<F, I, E, C, S> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<F: Copy, I, E, C, S> Copy for SatisfyMap<F, I, E, C, S> {}
#[inline(always)]
pub fn satisfy_map<I: Input, O, E: ParseError<I>, C, S: Clone, F: FnMut(&I::Token) -> Option<O>>(
    f: F,
) -> SatisfyMap<F, I, E, C, S> {
    SatisfyMap(f, PhantomData)
}
#[inline(always)]
pub fn satisfy_map_once<I: Input, O, E: ParseError<I>, C, S: Clone, F: FnOnce(&I::Token) -> Option<O>>(
    f: F,
) -> SatisfyMap<F, I, E, C, S> {
    SatisfyMap(f, PhantomData)
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, F: FnOnce(&I::Token) -> Option<O>> ParserOnce<I, O, E, C, S>
    for SatisfyMap<F, I, E, C, S>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O> {
        let o = self.0(&(args.uncons()?.0))?;
        args.consume.drop();
        Some(o)
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, F: FnMut(&I::Token) -> Option<O>> Parser<I, O, E, C, S>
    for SatisfyMap<F, I, E, C, S>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O> {
        let o = self.0(&(args.uncons()?.0))?;
        args.consume.drop();
        Some(o)
    }
}

#[derive(Clone, Copy)]
pub struct Config<F>(F);
#[inline]
pub fn config<F>(f: F) -> Config<F> {
    Config(f)
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>, F: FnOnce(&C) -> P>
    ParserOnce<I, O, E, C, S> for Config<F>
{
    #[inline]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        self.0(args.config).run_once(args)
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>, F: Fn(&C) -> P> Parser<I, O, E, C, S>
    for Config<F>
{
    #[inline]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.0(args.config).run_once(args)
    }
}

#[derive(Clone, Copy)]
pub struct SetConfig<C, C2, P>(C, P, PhantomData<fn() -> C2>);
#[inline]
pub fn set_config<C, P, C2>(config: C, parser: P) -> SetConfig<C, C2, P> {
    SetConfig(config, parser, PhantomData)
}
impl<I: Input, O, E: ParseError<I>, C, C2, S: Clone, P: ParserOnce<I, O, E, C, S>> ParserOnce<I, O, E, C2, S>
    for SetConfig<C, C2, P>
{
    #[inline]
    fn run_once(self, args: Args<I, E, C2, S>) -> Option<O> {
        let Args { input, config: _, state, consume, error } = args;
        self.1.run_once(Args { input, config: &self.0, state, consume, error })
    }
}
impl<I: Input, O, E: ParseError<I>, C, C2, S: Clone, P: Parser<I, O, E, C, S>> Parser<I, O, E, C2, S>
    for SetConfig<C, C2, P>
{
    #[inline]
    fn run(&mut self, args: Args<I, E, C2, S>) -> Option<O> {
        let Args { input, config: _, state, consume, error } = args;
        self.1.run(Args { input, config: &self.0, state, consume, error })
    }
}

#[derive(Clone, Copy)]
pub struct Pos;
#[inline]
pub fn pos() -> Pos {
    Pos
}
impl<I: Input, E: ParseError<I>, C, S: Clone> ParserOnce<I, I::Position, E, C, S> for Pos {
    #[inline]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<I::Position> {
        self.run(args)
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone> Parser<I, I::Position, E, C, S> for Pos {
    #[inline]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<I::Position> {
        Some(args.input.position())
    }
}

#[derive(Clone, Copy)]
pub struct State<F>(F);
#[inline]
pub fn state<F>(f: F) -> State<F> {
    State(f)
}
impl<I: Input, E: ParseError<I>, C, S: Clone, F: FnOnce(&mut S)> ParserOnce<I, (), E, C, S> for State<F> {
    #[inline]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<()> {
        self.0(&mut args.state);
        Some(())
    }
}
impl<I: Input, E: ParseError<I>, C, S: Clone, F: Fn(&mut S)> Parser<I, (), E, C, S> for State<F> {
    #[inline]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<()> {
        self.0(&mut args.state);
        Some(())
    }
}

pub struct LocalState<S, SLocal, P>(SLocal, P, PhantomData<fn() -> S>);
impl<SLocal: Clone, P: Clone, S> Clone for LocalState<S, SLocal, P> {
    #[inline]
    fn clone(&self) -> Self {
        LocalState(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<SLocal: Copy, P: Copy, S> Copy for LocalState<S, SLocal, P> {}
#[inline]
pub fn local_state<S, SLocal, P>(state: SLocal, parser: P) -> LocalState<S, SLocal, P> {
    LocalState(state, parser, PhantomData)
}
pub fn no_state<S, P>(parser: P) -> LocalState<S, (), P> {
    LocalState((), parser, PhantomData)
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, SLocal: Clone, P: ParserOnce<I, O, E, C, SLocal>>
    ParserOnce<I, O, E, C, S> for LocalState<S, SLocal, P>
{
    #[inline]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state: _, consume, error } = args;
        consume.wrap(|consume| self.1.run_once(Args { input, config, state: &mut self.0, consume, error }))
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, SLocal: Clone, P: Parser<I, O, E, C, SLocal>> Parser<I, O, E, C, S>
    for LocalState<S, SLocal, P>
{
    #[inline]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state: _, consume, error } = args;
        consume.wrap(|consume| self.1.by_ref().run_once(Args { input, config, state: &mut self.0, consume, error }))
    }
}
