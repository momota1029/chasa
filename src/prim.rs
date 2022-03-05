use std::{
    fmt::Display,
    marker::PhantomData,
    ops::{Bound, RangeBounds},
};

use either::Either;

use crate::{
    error::{Builder as Eb, CustomBuilder as Cb},
    input::{Input, IntoChars},
    traits::{ICont, IOk, IResult, IReturn, Parser, ParserOnce},
    util::{run_satisfy, CharsOrRange},
};

impl<I: Input, C, S, M: Cb, Output, P: ParserOnce<I, Output, C, S, M>, F: FnOnce() -> P> ParserOnce<I, Output, C, S, M>
    for F
{
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<Output, I, S, M> {
        self().run_once(cont)
    }
}
impl<I: Input, C, S, M: Cb, Output, P: ParserOnce<I, Output, C, S, M>, F: Fn() -> P> Parser<I, Output, C, S, M> for F {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<Output, I, S, M> {
        self().run_once(cont)
    }
}

/// Make a function a parser such that the parser is applied chainwise to get the result.
/// See also `tail_rec` and `case` if you feel compelled to use this.
/// # Example
/// ```
/// use chasa::char::prelude::*;
/// fn p<I:Input<Item=char>>() -> impl Pat<I,String> {
///     // Note the reverse order
///     parser(|k| k.then(any.and(p).map(|(x,mut xs)| {xs.push(x); xs}).or(pure(String::new()))))
/// }
/// assert_eq!(p.parse_ok("abcd"), Some("dcba".to_string()));
/// assert_eq!(p.parse_ok("stressed"), Some("desserts".to_string()));
///
/// // Straightforward implementation without wasting stacks
/// let q = tail_rec(vec![], |mut xs| any.or_not().map_mv(move |x| match x {
///     Some(x) => { xs.push(x); Err(xs) },
///     None => Ok(xs)
/// })).map(|xs| xs.iter().rev().collect::<String>());
/// assert_eq!(q.parse_ok("stressed"), Some("desserts".to_string()));
/// ```
#[derive(Clone, Copy)]
pub struct FnParser<F>(F);
#[inline]
pub fn parser<I: Input, O, C, S, M: Cb, F: Fn(ICont<I, C, S, M>) -> IReturn<O, I, C, S, M>>(f: F) -> FnParser<F> {
    FnParser(f)
}
#[inline]
pub fn parser_mv<I: Input, O, C, S, M: Cb, F: FnOnce(ICont<I, C, S, M>) -> IReturn<O, I, C, S, M>>(
    f: F,
) -> FnParser<F> {
    FnParser(f)
}
impl<O, I: Input, C, S, M: Cb, F: FnOnce(ICont<I, C, S, M>) -> IReturn<O, I, C, S, M>> ParserOnce<I, O, C, S, M>
    for FnParser<F>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0(cont).0.map(|(o, cont)| (o, cont.ok))
    }
}
impl<O, I: Input, C, S, M: Cb, F: Fn(ICont<I, C, S, M>) -> IReturn<O, I, C, S, M>> Parser<I, O, C, S, M>
    for FnParser<F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0(cont).0.map(|(o, cont)| (o, cont.ok))
    }
}

/// Makes a parser reference a parser. Use this to reuse the parser locally.
/// # Example
/// ```
/// use chasa::char::prelude::*;
/// let p = char('a').and(char('b'));
/// let p = p.to_ref();
/// assert_eq!(p.parse_ok("ab"), Some(('a','b')));
/// assert_eq!(many(p).parse_ok("abababc"), Some(vec![('a','b'),('a','b'),('a','b')]));
/// ```
pub struct RefParser<'a, P>(pub(crate) &'a P);
impl<'a, P> Clone for RefParser<'a, P> {
    #[inline]
    fn clone(&self) -> Self {
        RefParser(&self.0)
    }
}
impl<'a, P> Copy for RefParser<'a, P> {}
impl<'a, I: Input, O, C, S, M: Cb, P: Parser<I, O, C, S, M>> ParserOnce<I, O, C, S, M> for RefParser<'a, P> {
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0.run(cont)
    }
}
impl<'a, I: Input, O, C, S, M: Cb, P: Parser<I, O, C, S, M>> Parser<I, O, C, S, M> for RefParser<'a, P> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0.run(cont)
    }
}

impl<I: Input, O, C, S, M: Cb, P1: ParserOnce<I, O, C, S, M>, P2: ParserOnce<I, O, C, S, M>> ParserOnce<I, O, C, S, M>
    for Either<P1, P2>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        match self {
            Either::Left(left) => left.run_once(cont),
            Either::Right(right) => right.run_once(cont),
        }
    }
}

/// A parser that returns its arguments as is.
#[derive(Clone, Copy)]
pub struct Pure<O>(O);
pub fn pure<O>(o: O) -> Pure<O> {
    Pure(o)
}
impl<O, I: Input, C, S, M: Cb> ParserOnce<I, O, C, S, M> for Pure<O> {
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        Ok((self.0, cont.ok))
    }
}
impl<O: Clone, I: Input, C, S, M: Cb> Parser<I, O, C, S, M> for Pure<O> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        Ok((self.0.clone(), cont.ok))
    }
}

/// A parser that never fails. You can choose between [`error::unexpect`][`crate::error::unexpect`], which prints "Unexpected \[token\]", and [`error::message`][`crate::error::message`], which prints "\[msg\]".
/// See also [`error::Builder`][`crate::error::Builder`]
/// # Example
/// ```
/// use chasa::char::prelude::*;
/// assert_eq!(fail::<_,()>(unexpect("chasa")).parse_easy(""),Err("unexpected chasa at 0".to_string()));
/// ```
pub struct Fail<O, M: Cb>(Eb<M>, PhantomData<fn() -> O>);
#[inline]
pub fn fail<M: Cb, O>(err: Eb<M>) -> Fail<O, M> {
    Fail(err, PhantomData)
}
impl<O, I: Input, C, S, M: Cb> ParserOnce<I, O, C, S, M> for Fail<O, M> {
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        Err(self.0.at::<I>(cont.ok.input.index(), cont.ok.input.pos(), None))
    }
}

/// The parser that matches the end of the input.
/// # Example
/// ```
/// use chasa::char::prelude::*;
/// assert_eq!(eoi.parse_easy(""), Ok(()));
/// assert_eq!(eoi.parse_easy("a2"), Err("unexpected a, expecting eoi at 0".to_string()))
/// ```
pub struct EoI<I, C, S, M>(PhantomData<fn() -> (I, C, S, M)>);
impl<I, C, S, M> Clone for EoI<I, C, S, M> {
    #[inline]
    fn clone(&self) -> Self {
        EoI(PhantomData)
    }
}
impl<I, C, S, M> Copy for EoI<I, C, S, M> {}
#[inline]
pub fn eoi<I, C, S, M>() -> EoI<I, C, S, M> {
    EoI(PhantomData)
}
impl<I: Input<Item = impl Display + 'static>, C, S, M: Cb> ParserOnce<I, (), C, S, M> for EoI<I, C, S, M> {
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<(), I, S, M> {
        self.run(cont)
    }
}
impl<I: Input<Item = impl Display + 'static>, C, S, M: Cb> Parser<I, (), C, S, M> for EoI<I, C, S, M> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<(), I, S, M> {
        let mut ok = cont.ok;
        let (index, pos) = (ok.input.index(), ok.input.pos());
        match ok.input.next() {
            None => Ok(((), ok)),
            Some(Err(e)) => Err(Eb::message(e).at::<I>(index, pos, None).or_merge(ok.err)),
            Some(Ok(c)) => Err(Eb::unexpected(c).merge(Eb::expected_eoi()).at::<I>(index, pos, None).or_merge(ok.err)),
        }
    }
}

/// A parser that takes a single arbitrary character.
/// # Example
/// ```
/// use chasa::char::prelude::*;
/// assert_eq!(any.parse_ok("12"), Some('1'));
/// assert_eq!(any.parse_ok(""), None)
/// ```
pub struct Any<I, C, S, M>(PhantomData<fn() -> (I, C, S, M)>);
impl<I, C, S, M> Clone for Any<I, C, S, M> {
    #[inline]
    fn clone(&self) -> Self {
        Any(PhantomData)
    }
}
impl<I, C, S, M> Copy for Any<I, C, S, M> {}
#[inline]
pub fn any<I, C, S, M>() -> Any<I, C, S, M> {
    Any(PhantomData)
}
impl<I: Input, C, S, M: Cb> ParserOnce<I, I::Item, C, S, M> for Any<I, C, S, M> {
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        self.run(cont)
    }
}
impl<I: Input, C, S, M: Cb> Parser<I, I::Item, C, S, M> for Any<I, C, S, M> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;

        if cont.ok.cutted {
            drop()
        }
        let (index, pos) = (input.index(), input.pos());
        match input.next() {
            None => Err(Eb::unexpected_eoi().at::<I>(index, pos, None).or_merge(err)),
            Some(Err(e)) => Err(Eb::message(e).at::<I>(index, pos, None).or_merge(err)),
            Some(Ok(item)) => {
                drop();
                Ok((item, IOk { input, state, err, cutted: true }))
            },
        }
    }
}

/// If the given character matches the input character, it is accepted.
/// # Example
/// ```
/// use chasa::char::prelude::*;
/// assert_eq!(char('a').parse_easy("a2"), Ok('a'));
/// assert_eq!(char('1').parse_easy("a2"), Err("unexpected a, expecting 1 at 0..1".to_string()))
/// ```
pub struct Char<Item, I, C, S, M>(Item, PhantomData<fn() -> (I, C, S, M)>);
impl<Item: Clone, I, C, S, M> Clone for Char<Item, I, C, S, M> {
    #[inline]
    fn clone(&self) -> Self {
        Char(self.0.clone(), PhantomData)
    }
}
impl<Item: Copy, I, C, S, M> Copy for Char<Item, I, C, S, M> {}
pub fn char<Item, I, C, S, M>(char: Item) -> Char<Item, I, C, S, M> {
    Char(char, PhantomData)
}
impl<I: Input<Item = impl Display + 'static>, C, S, M: Cb, Item: PartialEq<I::Item> + Display>
    ParserOnce<I, I::Item, C, S, M> for Char<Item, I, C, S, M>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        self.run(cont)
    }
}
impl<I: Input<Item = impl Display + 'static>, C, S, M: Cb, Item: PartialEq<I::Item> + Display>
    Parser<I, I::Item, C, S, M> for Char<Item, I, C, S, M>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;

        match run_satisfy(&mut input, drop, cont.ok.cutted, |item| if &self.0 == &item { Ok(item) } else { Err(item) })
        {
            Err(e) => Err(e.label(format!("{}", self.0)).or_merge(err)),
            Ok(item) => Ok((item, IOk { input, state, err, cutted: true })),
        }
    }
}

/// It takes one character from the input, compares it with the given iterator, and accepts it if any of the characters match.
///
/// # Example
/// ```
/// use chasa::char::prelude::*;
/// assert_eq!(one_of("abc").parse_ok("a2"), Some('a'));
/// // You can also write like this
/// assert_eq!(one_of('a'..='c').parse_ok("a2"), Some('a'));
///
/// assert_eq!(one_of("def").parse_ok("a2"), None)
/// ```
pub struct OneOf<Iter, Item, I, C, S, M>(Iter, PhantomData<fn() -> (Item, I, C, S, M)>);
impl<Iter: Clone, Item, I, C, S, M> Clone for OneOf<Iter, Item, I, C, S, M> {
    #[inline]
    fn clone(&self) -> Self {
        OneOf(self.0.clone(), PhantomData)
    }
}
impl<Iter: Copy, Item, I, C, S, M> Copy for OneOf<Iter, Item, I, C, S, M> {}
#[inline]
pub fn one_of<Iter: CharsOrRange<Item>, Item, I, C, S, M>(chars: Iter) -> OneOf<Iter::To, Item, I, C, S, M> {
    OneOf(chars.to(), PhantomData)
}
impl<I: Input, C, S, M: Cb, Item: PartialEq<I::Item>, Iter: IntoChars<Item = Item>> Parser<I, I::Item, C, S, M>
    for OneOf<Iter, Item, I, C, S, M>
where
    I::Item: Display + 'static,
    Self: Clone,
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        self.clone().run_once(cont)
    }
}
impl<I: Input, C, S, M: Cb, Item: PartialEq<I::Item>, Iter: IntoChars<Item = Item>> ParserOnce<I, I::Item, C, S, M>
    for OneOf<Iter, Item, I, C, S, M>
where
    I::Item: Display + 'static,
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;
        match run_satisfy(&mut input, drop, cont.ok.cutted, |item| {
            for compared in self.0.into_chars() {
                if &compared == &item {
                    return Ok(item);
                }
            }
            Err(item)
        }) {
            Ok(item) => Ok((item, IOk { input, state, err, cutted: true })),
            Err(e) => Err(e.or_merge(err)),
        }
    }
}
impl<I: Input, C, S, M: Cb, Item: PartialOrd<I::Item>> ParserOnce<I, I::Item, C, S, M>
    for OneOf<(Bound<Item>, Bound<Item>), Item, I, C, S, M>
where
    I::Item: PartialOrd<Item> + Display + 'static,
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        self.run(cont)
    }
}
impl<I: Input, C, S, M: Cb, Item: PartialOrd<I::Item>> Parser<I, I::Item, C, S, M>
    for OneOf<(Bound<Item>, Bound<Item>), Item, I, C, S, M>
where
    I::Item: PartialOrd<Item> + Display + 'static,
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;
        match run_satisfy(
            &mut input,
            drop,
            cont.ok.cutted,
            |item| {
                if self.0.contains(&item) {
                    Ok(item)
                } else {
                    Err(item)
                }
            },
        ) {
            Ok(item) => Ok((item, IOk { input, state, err, cutted: true })),
            Err(e) => Err(e.or_merge(err)),
        }
    }
}

/// It takes one character from the input, compares it with the given iterator, and only accepts if none of the characters match.
/// # Example
/// ```
/// use chasa::char::prelude::*;
/// assert_eq!(none_of("abc").parse_ok("a2"), None);
/// assert_eq!(none_of("def").parse_ok("a2"), Some('a'))
/// ```
pub struct NoneOf<Iter, Item, I, C, S, M>(Iter, PhantomData<fn() -> (Item, I, C, S, M)>);
impl<Iter: Clone, Item, I, C, S, M> Clone for NoneOf<Iter, Item, I, C, S, M> {
    #[inline]
    fn clone(&self) -> Self {
        NoneOf(self.0.clone(), PhantomData)
    }
}
impl<Iter: Copy, Item, I, C, S, M> Copy for NoneOf<Iter, Item, I, C, S, M> {}
#[inline]
pub fn none_of<Iter: CharsOrRange<Item>, Item, I, C, S, M>(chars: Iter) -> NoneOf<Iter::To, Item, I, C, S, M> {
    NoneOf(chars.to(), PhantomData)
}
impl<I: Input<Item = impl Display + 'static>, C, S, M: Cb, Item: PartialEq<I::Item>, Iter: IntoChars<Item = Item>>
    Parser<I, I::Item, C, S, M> for NoneOf<Iter, Item, I, C, S, M>
where
    Self: Clone,
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        self.clone().run_once(cont)
    }
}
impl<I: Input<Item = impl Display + 'static>, C, S, M: Cb, Item: PartialEq<I::Item>, Iter: IntoChars<Item = Item>>
    ParserOnce<I, I::Item, C, S, M> for NoneOf<Iter, Item, I, C, S, M>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;
        if cont.ok.cutted {
            drop()
        }
        let (index, pos) = (input.index(), input.pos());
        match input.next() {
            None => Err(Eb::unexpected_eoi().at::<I>(index, pos, None).or_merge(err)),
            Some(Err(e)) => Err(Eb::message(e).at::<I>(index, pos, None).or_merge(err)),
            Some(Ok(item)) => {
                for compared in self.0.into_chars() {
                    if &compared == &item {
                        return Err(Eb::unexpected(item).at::<I>(input.index(), pos, Some(input.pos())).or_merge(err));
                    }
                }
                Ok((item, IOk { input, state, err, cutted: true }))
            },
        }
    }
}
impl<I: Input<Item = impl PartialOrd<Item> + Display + 'static>, C, S, M: Cb, Item: PartialOrd<I::Item>>
    ParserOnce<I, I::Item, C, S, M> for NoneOf<(Bound<Item>, Bound<Item>), Item, I, C, S, M>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        self.run(cont)
    }
}
impl<I: Input<Item = impl PartialOrd<Item> + Display + 'static>, C, S, M: Cb, Item: PartialOrd<I::Item>>
    Parser<I, I::Item, C, S, M> for NoneOf<(Bound<Item>, Bound<Item>), Item, I, C, S, M>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;
        match run_satisfy(
            &mut input,
            drop,
            cont.ok.cutted,
            |item| {
                if !self.0.contains(&item) {
                    Ok(item)
                } else {
                    Err(item)
                }
            },
        ) {
            Ok(item) => Ok((item, IOk { input, state, err, cutted: true })),
            Err(e) => Err(e.or_merge(err)),
        }
    }
}

/// A parser that compares character iterators and input as they are consumed together, and accepts them if they all match.
/// The return value is empty, as most of the time it should be intended to be a token.
/// If you want the whole string, use `.to([string])`(see [`Value`][`crate::combi::Value`]) or `.get_str()`(see [`GetString`][`crate::combi::GetString`]).
/// # Example
/// ```
/// use chasa::char::prelude::*;
/// assert_eq!(str("a").parse_ok("a2"), Some(()));
/// assert_eq!(str("a2").parse_ok("a2"), Some(()));
/// assert_eq!(str("a23").parse_ok("a2"), None);
/// assert_eq!(str("a3").to(1).or(str("a").to(2)).parse_ok("a2"), Some(2));
/// ```
pub struct String<Iter, I, C, S, M>(Iter, PhantomData<fn() -> (I, C, S, M)>);
impl<Iter: Clone, I, C, S, M> Clone for String<Iter, I, C, S, M> {
    #[inline]
    fn clone(&self) -> Self {
        String(self.0.clone(), PhantomData)
    }
}
impl<Iter: Copy, I, C, S, M> Copy for String<Iter, I, C, S, M> {}
pub fn str<Iter: IntoChars<Item = I::Item>, I: Input, C, S, M: Cb>(iter: Iter) -> String<Iter, I, C, S, M> {
    String(iter, PhantomData)
}
impl<
        I: Input<Item = impl Display + 'static>,
        C,
        S,
        M: Cb,
        Iter: IntoChars<Item = impl PartialEq<I::Item> + Display + 'static>,
    > ParserOnce<I, (), C, S, M> for String<Iter, I, C, S, M>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<(), I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;
        let mut is_first = true;
        for str_c in self.0.into_chars() {
            if is_first {
                if cont.ok.cutted {
                    drop()
                }
                is_first = false;
            }
            let (index, pos) = (input.index(), input.pos());
            match input.next() {
                None => return Err(Eb::unexpected_eoi().at::<I>(index, pos, None).or_merge(err)),
                Some(Err(e)) => return Err(Eb::message(e).at::<I>(index, pos, None).or_merge(err)),
                Some(Ok(input_c)) => {
                    if str_c != input_c {
                        return Err(Eb::unexpected(input_c)
                            .label(str_c)
                            .at::<I>(input.index(), pos, Some(input.pos()))
                            .or_merge(err));
                    }
                },
            }
        }
        Ok(((), IOk { input, err: if is_first { err } else { None }, state, cutted: !is_first }))
    }
}
impl<
        I: Input<Item = impl Display + 'static>,
        C,
        S,
        M: Cb,
        Iter: IntoChars<Item = impl PartialEq<I::Item> + Display + 'static> + Clone,
    > Parser<I, (), C, S, M> for String<Iter, I, C, S, M>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<(), I, S, M> {
        self.clone().run(cont)
    }
}

/// A parser that takes a single character satisfying a condition.
pub struct Satisfy<F, I, C, S, M>(F, PhantomData<fn() -> (I, C, S, M)>);
#[inline]
pub fn satisfy<I: Input, F: Fn(&I::Item) -> bool, C, S, M>(f: F) -> Satisfy<F, I, C, S, M> {
    Satisfy(f, PhantomData)
}
#[inline]
pub fn satisfy_mv<I: Input, F: FnOnce(&I::Item) -> bool, C, S, M>(f: F) -> Satisfy<F, I, C, S, M> {
    Satisfy(f, PhantomData)
}
impl<F: Clone, I, C, S, M> Clone for Satisfy<F, I, C, S, M> {
    #[inline]
    fn clone(&self) -> Self {
        Satisfy(self.0.clone(), PhantomData)
    }
}
impl<F: Copy, I, C, S, M> Copy for Satisfy<F, I, C, S, M> {}
impl<I: Input, C, S, M: Cb, F: FnOnce(&I::Item) -> bool> ParserOnce<I, I::Item, C, S, M> for Satisfy<F, I, C, S, M>
where
    I::Item: Display + 'static,
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;
        if cont.ok.cutted {
            drop()
        }
        let (index, pos) = (input.index(), input.pos());
        match input.next() {
            None => Err(Eb::unexpected_eoi().at::<I>(index, pos, None).or_merge(err)),
            Some(Err(e)) => Err(Eb::message(e).at::<I>(index, pos, None).or_merge(err)),
            Some(Ok(item)) => {
                if self.0(&item) {
                    drop();
                    Ok((item, IOk { input, state, err, cutted: true }))
                } else {
                    Err(Eb::unexpected(item).at::<I>(input.index(), pos, Some(input.pos())).or_merge(err))
                }
            },
        }
    }
}
impl<I: Input, C, S, M: Cb, F: Fn(&I::Item) -> bool> Parser<I, I::Item, C, S, M> for Satisfy<F, I, C, S, M>
where
    I::Item: Display + 'static,
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<I::Item, I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;
        if cont.ok.cutted {
            drop()
        }
        let (index, pos) = (input.index(), input.pos());
        match input.next() {
            None => Err(Eb::unexpected_eoi().at::<I>(index, pos, None).or_merge(err)),
            Some(Err(e)) => Err(Eb::message(e).at::<I>(index, pos, None).or_merge(err)),
            Some(Ok(item)) => {
                if self.0(&item) {
                    drop();
                    Ok((item, IOk { input, state, err, cutted: true }))
                } else {
                    Err(Eb::unexpected(item).at::<I>(input.index(), pos, Some(input.pos())).or_merge(err))
                }
            },
        }
    }
}

pub struct SatisfyMap<F, I, C, S, M>(F, PhantomData<fn() -> (I, C, S, M)>);
#[inline]
pub fn satisfy_map<F: Fn(&I::Item) -> Option<O>, O, I: Input, C, S, M>(f: F) -> SatisfyMap<F, I, C, S, M> {
    SatisfyMap(f, PhantomData)
}
#[inline]
pub fn satisfy_map_mv<F: FnOnce(&I::Item) -> Option<O>, O, I: Input, C, S, M>(f: F) -> SatisfyMap<F, I, C, S, M> {
    SatisfyMap(f, PhantomData)
}
impl<F: Clone, I, C, S, M> Clone for SatisfyMap<F, I, C, S, M> {
    #[inline]
    fn clone(&self) -> Self {
        SatisfyMap(self.0.clone(), PhantomData)
    }
}
impl<F: Copy, I, C, S, M> Copy for SatisfyMap<F, I, C, S, M> {}
impl<O, I: Input, C, S, M: Cb, F: FnOnce(&I::Item) -> Option<O>> ParserOnce<I, O, C, S, M> for SatisfyMap<F, I, C, S, M>
where
    I::Item: Display + 'static,
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;
        if cont.ok.cutted {
            drop()
        }
        let (index, pos) = (input.index(), input.pos());
        match input.next() {
            None => Err(Eb::unexpected_eoi().at::<I>(index, pos, None).or_merge(err)),
            Some(Err(e)) => Err(Eb::message(e).at::<I>(index, pos, None).or_merge(err)),
            Some(Ok(item)) => match self.0(&item) {
                Some(o) => {
                    drop();
                    Ok((o, IOk { input, state, err, cutted: true }))
                },
                None => Err(Eb::unexpected(item).at::<I>(input.index(), pos, Some(input.pos())).or_merge(err)),
            },
        }
    }
}
impl<O, I: Input, C, S, M: Cb, F: Fn(&I::Item) -> Option<O>> Parser<I, O, C, S, M> for SatisfyMap<F, I, C, S, M>
where
    I::Item: Display + 'static,
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok: IOk { mut input, err, state, .. }, drop, .. } = cont;
        if cont.ok.cutted {
            drop()
        }
        let (index, pos) = (input.index(), input.pos());
        match input.next() {
            None => Err(Eb::unexpected_eoi().at::<I>(index, pos, None).or_merge(err)),
            Some(Err(e)) => Err(Eb::message(e).at::<I>(index, pos, None).or_merge(err)),
            Some(Ok(item)) => match self.0(&item) {
                Some(o) => {
                    drop();
                    Ok((o, IOk { input, state, err, cutted: true }))
                },
                None => Err(Eb::unexpected(item).at::<I>(input.index(), pos, Some(input.pos())).or_merge(err)),
            },
        }
    }
}

#[derive(Clone, Copy)]
pub struct Config<F>(F);
#[inline]
pub fn config<F>(f: F) -> Config<F> {
    Config(f)
}
impl<I: Input, O, C, S, M: Cb, P: ParserOnce<I, O, C, S, M>, F: FnOnce(&C) -> P> ParserOnce<I, O, C, S, M>
    for Config<F>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0(cont.config).run_once(cont)
    }
}
impl<I: Input, O, C, S, M: Cb, P: ParserOnce<I, O, C, S, M>, F: Fn(&C) -> P> Parser<I, O, C, S, M> for Config<F> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0(cont.config).run_once(cont)
    }
}

#[derive(Clone, Copy)]
pub struct SetConfig<C, C2, P>(C, P, PhantomData<fn() -> C2>);
#[inline]
pub fn set_config<C, P, C2>(config: C, parser: P) -> SetConfig<C, C2, P> {
    SetConfig(config, parser, PhantomData)
}
impl<I: Input, O, C, C2, S, M: Cb, P: ParserOnce<I, O, C, S, M>> ParserOnce<I, O, C2, S, M> for SetConfig<C, C2, P> {
    #[inline]
    fn run_once(self, cont: ICont<I, C2, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, drop, config: _ } = cont;
        self.1.run_once(ICont { ok, config: &self.0, drop })
    }
}
impl<I: Input, O, C, C2, S, M: Cb, P: Parser<I, O, C, S, M>> Parser<I, O, C2, S, M> for SetConfig<C, C2, P> {
    #[inline]
    fn run(&self, cont: ICont<I, C2, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, drop, config: _ } = cont;
        self.1.run(ICont { ok, config: &self.0, drop })
    }
}

#[derive(Clone, Copy)]
pub struct Pos;
#[inline]
pub fn pos() -> Pos {
    Pos
}
impl<I: Input, C, S, M: Cb> ParserOnce<I, I::Pos, C, S, M> for Pos {
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<I::Pos, I, S, M> {
        self.run(cont)
    }
}
impl<I: Input, C, S, M: Cb> Parser<I, I::Pos, C, S, M> for Pos {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<I::Pos, I, S, M> {
        let ICont { ok, .. } = cont;
        Ok((ok.input.pos(), ok))
    }
}

#[derive(Clone, Copy)]
pub struct State<F>(F);
#[inline]
pub fn state<F>(f: F) -> State<F> {
    State(f)
}
impl<I: Input, C, S, M: Cb, F: FnOnce(S) -> S> ParserOnce<I, (), C, S, M> for State<F> {
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<(), I, S, M> {
        let IOk { input, err, state, cutted, .. } = cont.ok;
        Ok(((), IOk { input, state: self.0(state), err, cutted }))
    }
}
impl<I: Input, C, S, M: Cb, F: Fn(S) -> S> Parser<I, (), C, S, M> for State<F> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<(), I, S, M> {
        let IOk { input, err, state, cutted, .. } = cont.ok;
        Ok(((), IOk { input, state: self.0(state), err, cutted }))
    }
}

#[derive(Clone, Copy)]
pub struct GetState<F>(F);
#[inline]
pub fn get_state<F>(f: F) -> GetState<F> {
    GetState(f)
}
impl<I: Input, O, C, S, M: Cb, P: ParserOnce<I, O, C, S, M>, F: FnOnce(&S) -> P> ParserOnce<I, O, C, S, M>
    for GetState<F>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0(&cont.ok.state).run_once(cont)
    }
}
impl<I: Input, O, C, S, M: Cb, P: ParserOnce<I, O, C, S, M>, F: Fn(&S) -> P> Parser<I, O, C, S, M> for GetState<F> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0(&cont.ok.state).run_once(cont)
    }
}

#[derive(Clone, Copy)]
pub struct SetState<S>(S);
#[inline]
pub fn set_state<S>(state: S) -> SetState<S> {
    SetState(state)
}
impl<I: Input, C, S, M: Cb> ParserOnce<I, (), C, S, M> for SetState<S> {
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<(), I, S, M> {
        let ICont { ok: IOk { input, err, cutted, .. }, .. } = cont;
        Ok(((), IOk { input, state: self.0, err, cutted }))
    }
}
impl<I: Input, C, S: Clone, M: Cb> Parser<I, (), C, S, M> for SetState<S> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<(), I, S, M> {
        let ICont { ok: IOk { input, err, cutted, .. }, .. } = cont;
        Ok(((), IOk { input, state: self.0.clone(), err, cutted }))
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
impl<I: Input, Output, C, S, SLocal, M: Cb, P: ParserOnce<I, Output, C, SLocal, M>> ParserOnce<I, Output, C, S, M>
    for LocalState<S, SLocal, P>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<Output, I, S, M> {
        let ICont { ok: IOk { input, err, cutted, state }, drop, config, .. } = cont;
        self.1
            .run_once(ICont { ok: IOk { input, err, cutted, state: self.0 }, drop, config })
            .map(|(o, IOk { input, err, cutted, state: _ })| (o, IOk { input, err, cutted, state }))
    }
}
impl<I: Input, Output, C, S, SLocal: Clone, M: Cb, P: Parser<I, Output, C, SLocal, M>> Parser<I, Output, C, S, M>
    for LocalState<S, SLocal, P>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<Output, I, S, M> {
        let ICont { ok: IOk { input, err, cutted, state }, drop, config, .. } = cont;
        self.1
            .run(ICont { ok: IOk { input, err, cutted, state: self.0.clone() }, drop, config })
            .map(|(o, IOk { input, err, cutted, state: _ })| (o, IOk { input, err, cutted, state }))
    }
}
