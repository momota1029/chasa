use std::{fmt::Display, marker::PhantomData};

use either::Either;

use crate::{
    error::{Builder as Eb, CustomBuilder as Cb},
    prim::{pure, Pure},
    util::run_drop,
    ICont, IOk, IResult, IReturn, Input, Parser, ParserOnce,
};

/**
If the first argument is Some, return its value; if None, execute the parser of the second argument.
# Example
```
use chasa::*;
assert_eq!(pure_or(Some("first"), str("second").to("second")).parse_ok("second"), Some("first"));
assert_eq!(pure_or(None, str("second").to("second")).parse_ok("second"), Some("second"))
```
*/
pub fn pure_or<O, I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>>(o: Option<O>, p: P) -> Either<Pure<O>, P> {
    match o {
        Some(o) => Either::Left(pure(o)),
        None => Either::Right(p),
    }
}

/**
Replace the parser result with the specified value.
*/
#[derive(Clone, Copy)]
pub struct Value<P, O>(pub(crate) P, pub(crate) O);
impl<I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>, O> ParserOnce<I, C, S, M> for Value<P, O> {
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0.run_once(cont).map(|(_, ok)| (self.1, ok))
    }
}
impl<I: Input, C, S, M: Cb, P: Parser<I, C, S, M>, O: Clone> Parser<I, C, S, M> for Value<P, O> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0.run(cont).map(|(_, ok)| (self.1.clone(), ok))
    }
}

/// Process parser results with functions.
#[derive(Clone, Copy)]
pub struct Map<P, F>(pub(crate) P, pub(crate) F);
impl<I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>, F: FnOnce(P::Output) -> O, O> ParserOnce<I, C, S, M>
    for Map<P, F>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0.run_once(cont).map(|(o, ok)| (self.1(o), ok))
    }
}
impl<I: Input, C, S, M: Cb, P: Parser<I, C, S, M>, F: Fn(P::Output) -> O, O> Parser<I, C, S, M> for Map<P, F> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.0.run(cont).map(|(o, ok)| (self.1(o), ok))
    }
}

/**
Add "expecting [specified label]" to the parser error display.
# Example
```
use chasa::*;
assert_eq!(char('a').label("special a").parse_easy("b"), Err("unexpected b, expecting special a at 0..1".to_string()));
```
*/
#[derive(Clone, Copy)]
pub struct Label<P, L>(pub(crate) P, pub(crate) L);
impl<I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>, L: Display + 'static> ParserOnce<I, C, S, M> for Label<P, L> {
    type Output = P::Output;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<P::Output, I, S, M> {
        if cont.ok.cutted {
            (cont.drop)()
        }
        self.0.run_once(cont).map_err(|e| e.label(self.1))
    }
}
impl<I: Input, C, S, M: Cb, P: Parser<I, C, S, M>, L: Display + 'static + Clone> Parser<I, C, S, M> for Label<P, L> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<P::Output, I, S, M> {
        if cont.ok.cutted {
            (cont.drop)()
        }
        self.0.run(cont).map_err(|e| e.label(self.1.clone()))
    }
}

/**
Add "expecting [specified label]" to the parser error display. The label will be evaluated lazily.
# Example
```
use chasa::*;
assert_eq!(char('a').label_with(||"special a").parse_easy("b"), Err("unexpected b, expecting special a at 0..1".to_string()));
```
*/
#[derive(Clone, Copy)]
pub struct LabelWith<P, F>(pub(crate) P, pub(crate) F);
impl<I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>, L: Display, F: Fn() -> L + 'static> ParserOnce<I, C, S, M>
    for LabelWith<P, F>
{
    type Output = P::Output;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<P::Output, I, S, M> {
        self.0.run_once(cont).map_err(|e| e.label_with(self.1))
    }
}
impl<I: Input, C, S, M: Cb, P: Parser<I, C, S, M>, L: Display, F: Fn() -> L + 'static + Clone> Parser<I, C, S, M>
    for LabelWith<P, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<P::Output, I, S, M> {
        self.0.run(cont).map_err(|e| e.label_with(self.1.clone()))
    }
}

/**
Execute the next parser generated by accepting the result of the parser. If either of them fails, the whole thing will fail.
# Example
```
use chasa::*;
assert_eq!(any.bind(|c| char(c)).parse_ok("aa"), Some('a'));
assert_eq!(char('b').bind(|_| char('a')).parse_ok("aa"), None);
assert_eq!(any.bind(|c| char(c)).parse_ok("ab"), None);
```
*/
#[derive(Clone, Copy)]
pub struct Bind<P, F>(pub(crate) P, pub(crate) F);
impl<I: Input, C, S, M: Cb, P1: ParserOnce<I, C, S, M>, P2: ParserOnce<I, C, S, M>, F: FnOnce(P1::Output) -> P2>
    ParserOnce<I, C, S, M> for Bind<P1, F>
{
    type Output = P2::Output;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<P2::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0.run_once(ICont { ok, config, drop }).and_then(|(o, ok)| self.1(o).run_once(ok.to_cont(config, drop)))
    }
}
impl<I: Input, C, S, M: Cb, P1: Parser<I, C, S, M>, P2: ParserOnce<I, C, S, M>, F: Fn(P1::Output) -> P2>
    Parser<I, C, S, M> for Bind<P1, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<P2::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0.run(ICont { ok, config, drop }).and_then(|(o, ok)| self.1(o).run_once(ok.to_cont(config, drop)))
    }
}

/// Execute the two parsers in succession and return them as tuples.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(any.and(any).parse_ok("aa"), Some(('a','a')))
/// ```
#[derive(Clone, Copy)]
pub struct And<P1, P2>(pub(crate) P1, pub(crate) P2);
impl<I: Input, C, S, M: Cb, P1: ParserOnce<I, C, S, M>, P2: ParserOnce<I, C, S, M>> ParserOnce<I, C, S, M>
    for And<P1, P2>
{
    type Output = (P1::Output, P2::Output);
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<(P1::Output, P2::Output), I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0
            .run_once(ICont { ok, config, drop })
            .and_then(|(o1, ok)| self.1.run_once(ok.to_cont(config, drop)).map(|(o2, ok)| ((o1, o2), ok)))
    }
}
impl<I: Input, C, S, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>> Parser<I, C, S, M> for And<P1, P2> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<(P1::Output, P2::Output), I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0
            .run(ICont { ok, config, drop })
            .and_then(|(o1, ok)| self.1.run(ok.to_cont(config, drop)).map(|(o2, ok)| ((o1, o2), ok)))
    }
}

/**
Runs two parsers in succession, returning only the first value. If either of them fails, the whole thing will fail.
# Example
```
use chasa::*;
assert_eq!(any.left(any).parse_ok("ab"), Some('a'));
assert_eq!(any.left(char('a')).parse_ok("ab"), None);
assert_eq!(str("chasa").to( ()).left(char(':')).parse_ok("chasa: parser combinator"), Some(()));
```
*/
#[derive(Clone, Copy)]
pub struct Left<P1, P2>(pub(crate) P1, pub(crate) P2);
impl<I: Input, C, S, M: Cb, P1: ParserOnce<I, C, S, M>, P2: ParserOnce<I, C, S, M>> ParserOnce<I, C, S, M>
    for Left<P1, P2>
{
    type Output = P1::Output;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<P1::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0
            .run_once(ICont { ok, config, drop })
            .and_then(|(o, ok)| self.1.run_once(ok.to_cont(config, drop)).map(|(_, ok)| (o, ok)))
    }
}
impl<I: Input, C, S, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>> Parser<I, C, S, M> for Left<P1, P2> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<P1::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0
            .run(ICont { ok, config, drop })
            .and_then(|(o, ok)| self.1.run(ok.to_cont(config, drop)).map(|(_, ok)| (o, ok)))
    }
}

/// Runs two parsers in succession, returning only the second value. If either of them fails, the whole thing will fail.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(any.right(any).parse_ok("ab"), Some('b'));
/// assert_eq!(char('b').right(any).parse_ok("ab"), None);
/// assert_eq!(ws.right(str("chasa").to( ())).parse_ok("   chasa"), Some(()));
/// ```
#[derive(Clone, Copy)]
pub struct Right<P1, P2>(pub(crate) P1, pub(crate) P2);
impl<I: Input, C, S, M: Cb, P1: ParserOnce<I, C, S, M>, P2: ParserOnce<I, C, S, M>> ParserOnce<I, C, S, M>
    for Right<P1, P2>
{
    type Output = P2::Output;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<P2::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0.run_once(ICont { ok, config, drop }).and_then(|(_, ok)| self.1.run_once(ok.to_cont(config, drop)))
    }
}
impl<I: Input, C, S, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>> Parser<I, C, S, M> for Right<P1, P2> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<P2::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0.run(ICont { ok, config, drop }).and_then(|(_, ok)| self.1.run(ok.to_cont(config, drop)))
    }
}
/// Place the parser between two parsers. The results at both ends will be ignored.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(any.between(char('('), char(')')).parse_ok("(a)"), Some('a'));
/// assert_eq!(char('a').between(char('('), char(')')).parse_ok("(a"), None);
/// ```
#[derive(Clone, Copy)]
pub struct Between<P1, P2, P3>(pub(crate) P1, pub(crate) P2, pub(crate) P3);
impl<I: Input, C, S, M: Cb, P1: ParserOnce<I, C, S, M>, P2: ParserOnce<I, C, S, M>, P3: ParserOnce<I, C, S, M>>
    ParserOnce<I, C, S, M> for Between<P1, P2, P3>
{
    type Output = P2::Output;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<P2::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0.run_once(ICont { ok, config, drop }).and_then(|(_, ok)| {
            self.1
                .run_once(ok.to_cont(config, drop))
                .and_then(|(o, ok)| self.2.run_once(ok.to_cont(config, drop)).map(|(_, ok)| (o, ok)))
        })
    }
}
impl<I: Input, C, S, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>, P3: Parser<I, C, S, M>> Parser<I, C, S, M>
    for Between<P1, P2, P3>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<P2::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0.run(ICont { ok, config, drop }).and_then(|(_, ok)| {
            self.1
                .run(ok.to_cont(config, drop))
                .and_then(|(o, ok)| self.2.run(ok.to_cont(config, drop)).map(|(_, ok)| (o, ok)))
        })
    }
}

/// Pass a value to chain the parser together with the parser result, and let the parser continue.
/// Even if `bind` returns a lot of parsers of different types, `case` does not need to use `Either` artificially
/// # Example
/// ```
/// use chasa::*;
/// fn parser<I:Input<Item=char>>() -> impl ParserOnce<I,(),(),Nil,Output=usize> {
///     one_of("abc").case(|char, k| match char {
///         'a' => k.to(10),
///         'b' => k.then(parser).and(parser).map(|(a,b)| a + b),
///         'c' => k.then(parser),
///         _ => unreachable!()
///     })
/// }
/// assert_eq!(parser.parse_ok("abc"), Some(10));
/// assert_eq!(parser.parse_ok("bcaa"), Some(20));
/// assert_eq!(parser.parse_ok("bcabacca"), Some(30));
/// assert_eq!(parser.parse_ok("ba"), None);
/// ```
#[derive(Clone, Copy)]
pub struct Case<P, F>(pub(crate) P, pub(crate) F);
impl<
        I: Input,
        C,
        S,
        M: Cb,
        P: ParserOnce<I, C, S, M>,
        O,
        F: FnOnce(P::Output, ICont<I, C, S, M>) -> IReturn<O, I, C, S, M>,
    > ParserOnce<I, C, S, M> for Case<P, F>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0
            .run_once(ICont { ok, config, drop })
            .and_then(|(o, ok)| self.1(o, ok.to_cont(config, drop)).0.map(|(o, k)| (o, k.ok)))
    }
}
impl<
        I: Input,
        C,
        S,
        M: Cb,
        P: Parser<I, C, S, M>,
        O,
        F: Fn(P::Output, ICont<I, C, S, M>) -> IReturn<O, I, C, S, M>,
    > Parser<I, C, S, M> for Case<P, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0
            .run(ICont { ok, config, drop })
            .and_then(|(o, ok)| self.1(o, ok.to_cont(config, drop)).0.map(|(o, k)| (o, k.ok)))
    }
}

/**
Sift through the parser results. If a token is sifted out, it does not consume input.
# Example
```
use chasa::*;
let p = any.and_then(|c| match c {
    'a' => Ok(true),
    _ => Err(message("hello"))
});
assert_eq!(p.parse_ok("abc"), Some(true));
assert_eq!(p.parse_easy("cba"), Err("hello at 0..1".to_string()));
```
*/
#[derive(Clone, Copy)]
pub struct AndThen<P, F>(pub(crate) P, pub(crate) F);
impl<I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>, O, F: FnOnce(P::Output) -> Result<O, Eb<M>>>
    ParserOnce<I, C, S, M> for AndThen<P, F>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        let pos = ok.input.pos();
        self.0.run_once(ICont { ok, config, drop }).and_then(|(o, ok)| match self.1(o) {
            Ok(o) => Ok((o, ok)),
            Err(err) => Err(err.at::<I>(ok.input.index(), pos, Some(ok.input.pos())).or_merge(ok.err)),
        })
    }
}
impl<I: Input, C, S, M: Cb, P: Parser<I, C, S, M>, O, F: Fn(P::Output) -> Result<O, Eb<M>>> Parser<I, C, S, M>
    for AndThen<P, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        let pos = ok.input.pos();
        self.0.run(ICont { ok, config, drop }).and_then(|(o, ok)| match self.1(o) {
            Ok(o) => Ok((o, ok)),
            Err(err) => Err(err.at::<I>(ok.input.index(), pos, Some(ok.input.pos())).or_merge(ok.err)),
        })
    }
}

/// If the first parser fails without consuming any input, try the next parser.
/// It is more efficient to assume that the syntax is determined when the first parser consumes input.
/// See also [`Cut`] for input consumption.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(char('a').or(char('b')).parse_ok("aa"), Some('a'));
/// assert_eq!(char('a').or(char('b')).parse_ok("bb"), Some('b'));
/// assert_eq!(char('a').right(char('b')).or(char('b').right(char('b'))).parse_ok("bb"), Some('b'));
/// assert_eq!(char('b').right(char('b')).or(char('b').right(char('a'))).parse_ok("ba"), None);
/// ```
#[derive(Clone, Copy)]
pub struct Or<P1, P2>(pub(crate) P1, pub(crate) P2);
impl<I: Input, C, S: Clone, M: Cb, P1: ParserOnce<I, C, S, M>, P2: ParserOnce<I, C, S, M, Output = P1::Output>>
    ParserOnce<I, C, S, M> for Or<P1, P2>
{
    type Output = P1::Output;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<P1::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        if ok.cutted {
            drop()
        }
        let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
        match run_drop(self.0, ICont { ok, config, drop }, (input, state)) {
            (Ok((o, ok)), _) => Ok((o, ok)),
            (Err(e), None) => Err(e),
            (Err(e), Some((input, state))) => {
                self.1.run_once(IOk { input, state, err: Some(e), cutted }.to_cont(config, drop))
            },
        }
    }
}
impl<I: Input, C, S: Clone, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M, Output = P1::Output>>
    Parser<I, C, S, M> for Or<P1, P2>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<P1::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        if ok.cutted {
            drop()
        }
        let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
        match run_drop(self.0.to_ref(), ICont { ok, config, drop }, (input, state)) {
            (Ok((o, ok)), _) => Ok((o, ok)),
            (Err(e), None) => Err(e),
            (Err(e), Some((input, state))) => {
                self.1.run(IOk { input, state, err: Some(e), cutted }.to_cont(config, drop))
            },
        }
    }
}

/// It returns `Some` if the parser succeeds, returns `None` if the parser does not consume any input and fails, and fails if the parser consumes some input and fails.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(char('a').or_not().parse_ok("aa"), Some(Some('a')));
/// assert_eq!(char('a').or_not().parse_ok("bb"), Some(None));
/// assert_eq!(char('a').right(char('b')).or_not().parse_ok("bb"), Some(None));
/// assert_eq!(char('b').right(char('b')).or_not().parse_ok("ba"), None);
/// ```
#[derive(Clone, Copy)]
pub struct OrNot<P>(pub(crate) P);
impl<I: Input, C, S: Clone, M: Cb, P: ParserOnce<I, C, S, M>> ParserOnce<I, C, S, M> for OrNot<P> {
    type Output = Option<P::Output>;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<Option<P::Output>, I, S, M> {
        let ICont { ok, config, drop } = cont;
        if ok.cutted {
            drop()
        }
        let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
        match run_drop(self.0, ICont { ok, config, drop }, (input, state)) {
            (Ok((o, ok)), _) => Ok((Some(o), ok)),
            (Err(e), None) => Err(e),
            (Err(e), Some((input, state))) => Ok((None, IOk { input, state, err: Some(e), cutted })),
        }
    }
}
impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>> Parser<I, C, S, M> for OrNot<P> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<Option<P::Output>, I, S, M> {
        let ICont { ok, config, drop } = cont;
        if ok.cutted {
            drop()
        }
        let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
        match run_drop(self.0.to_ref(), ICont { ok, config, drop }, (input, state)) {
            (Ok((o, ok)), _) => Ok((Some(o), ok)),
            (Err(e), None) => Err(e),
            (Err(e), Some((input, state))) => Ok((None, IOk { input, state, err: Some(e), cutted })),
        }
    }
}

/// If the parser fails, it will not consume any input. This is useful for parsing tokens that have multiple parts combined.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(char('b').right(char('b')).or(char('b').right(char('a'))).parse_ok("ba"), None);
/// assert_eq!(char('b').right(char('b')).cut().or(char('b').right(char('a'))).parse_ok("ba"), Some('a'));
/// ```
#[derive(Clone, Copy)]
pub struct Cut<P>(pub(crate) P);
impl<I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>> ParserOnce<I, C, S, M> for Cut<P> {
    type Output = P::Output;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<P::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        if ok.cutted {
            drop()
        }
        match run_drop(self.0, ICont { ok, config, drop: &mut || {} }, ()) {
            (Ok((o, ok)), d) => {
                if d.is_some() {
                    drop()
                }
                Ok((o, IOk { cutted: ok.cutted && !d.is_some(), ..ok }))
            },
            (Err(e), _) => Err(e),
        }
    }
}
impl<I: Input, C, S, M: Cb, P: Parser<I, C, S, M>> Parser<I, C, S, M> for Cut<P> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<P::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        if ok.cutted {
            drop()
        }
        match run_drop(self.0.to_ref(), ICont { ok, config, drop }, ()) {
            (Ok((o, ok)), d) => {
                if d.is_some() {
                    drop()
                }
                Ok((o, IOk { cutted: ok.cutted && !d.is_some(), ..ok }))
            },
            (Err(e), _) => Err(e),
        }
    }
}

/// Returns the parser result with the position before and after the parse.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(char('a').ranged().parse_ok("a"), Some(('a',0,1)));
/// assert_eq!(str("abcd").to(()).ranged().parse_ok("abcd"), Some(((),0,4)));
/// assert_eq!(ws.right(str("abcd").to(()).ranged()).parse_ok("    abcd"), Some(((),4,8)))
/// ```
#[derive(Clone, Copy)]
pub struct Ranged<P>(pub(crate) P);
impl<I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>> ParserOnce<I, C, S, M> for Ranged<P> {
    type Output = (P::Output, I::Pos, I::Pos);
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<Self::Output, I, S, M> {
        let pos = cont.ok.input.pos();
        self.0.run_once(cont).map(|(o, ok)| ((o, pos, ok.input.pos()), ok))
    }
}
impl<I: Input, C, S, M: Cb, P: Parser<I, C, S, M>> Parser<I, C, S, M> for Ranged<P> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<Self::Output, I, S, M> {
        let pos = cont.ok.input.pos();
        self.0.run(cont).map(|(o, ok)| ((o, pos, ok.input.pos()), ok))
    }
}

/// Returns together with the string accepted by the parser.
pub struct GetString<P, O>(pub(crate) P, pub(crate) PhantomData<fn() -> O>);
impl<O: FromIterator<I::Item>, I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>> ParserOnce<I, C, S, M>
    for GetString<P, O>
{
    type Output = (P::Output, O);
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<(P::Output, O), I, S, M> {
        let (mut input, begin) = (cont.ok.input.clone(), cont.ok.input.index());
        self.0.run_once(cont).map(|(o, ok)| {
            let end = ok.input.index();
            let str = InputIter { input: &mut input, begin, end }.collect();
            ((o, str), ok)
        })
    }
}
impl<O: FromIterator<I::Item>, I: Input, C, S, M: Cb, P: Parser<I, C, S, M>> Parser<I, C, S, M> for GetString<P, O> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<(P::Output, O), I, S, M> {
        let (mut input, begin) = (cont.ok.input.clone(), cont.ok.input.index());
        self.0.run(cont).map(|(o, ok)| {
            let end = ok.input.index();
            let str = InputIter { input: &mut input, begin, end }.collect();
            ((o, str), ok)
        })
    }
}
struct InputIter<'a, I: Input> {
    input: &'a mut I,
    begin: usize,
    end: usize,
}
impl<'a, I: Input> Iterator for InputIter<'a, I> {
    type Item = I::Item;
    #[inline]
    fn next(&mut self) -> Option<I::Item> {
        if self.input.index() < self.end {
            self.input.next()?.ok()
        } else {
            None
        }
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.end - self.begin;
        (len, Some(len))
    }
}
impl<'a, I: Input> ExactSizeIterator for InputIter<'a, I> {}

pub struct GetStringExtend<P, O>(pub(crate) P, pub(crate) O);
#[inline]
pub fn extend_with_str<O, P>(str: O, parser: P) -> GetStringExtend<P, O> {
    GetStringExtend(parser, str)
}
impl<O: Extend<I::Item>, I: Input, C, S, M: Cb, P: ParserOnce<I, C, S, M>> ParserOnce<I, C, S, M>
    for GetStringExtend<P, O>
{
    type Output = (P::Output, O);
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<(P::Output, O), I, S, M> {
        let (mut input, begin) = (cont.ok.input.clone(), cont.ok.input.index());
        self.0.run_once(cont).map(|(o, ok)| {
            let end = ok.input.index();
            let mut str = self.1;
            str.extend(InputIter { input: &mut input, begin, end });
            ((o, str), ok)
        })
    }
}
impl<O: Extend<I::Item> + Clone, I: Input, C, S, M: Cb, P: Parser<I, C, S, M>> Parser<I, C, S, M>
    for GetStringExtend<P, O>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<(P::Output, O), I, S, M> {
        let (mut input, begin) = (cont.ok.input.clone(), cont.ok.input.index());
        self.0.run(cont).map(|(o, ok)| {
            let end = ok.input.index();
            let mut str = self.1.clone();
            str.extend(InputIter { input: &mut input, begin, end });
            ((o, str), ok)
        })
    }
}
/// If successful, it does not consume input. The subsequent parser will read the same part again.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(char('a').and(char('a')).parse_ok("a"), None);
/// assert_eq!(before(char('a')).and(char('a')).parse_ok("a"), Some(('a','a')));
/// assert_eq!(char('a').and(char('b')).parse_ok("ab"), Some(('a','b')));
/// assert_eq!(before(char('a')).and(char('b')).parse_ok("ab"), None);
/// ```
#[derive(Clone, Copy)]
pub struct Before<P>(pub(crate) P);
#[inline]
pub fn before<P>(parser: P) -> Before<P> {
    Before(parser)
}

impl<I: Input, C, S: Clone, M: Cb, P: ParserOnce<I, C, S, M>> ParserOnce<I, C, S, M> for Before<P> {
    type Output = P::Output;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<P::Output, I, S, M> {
        let (input, state) = (cont.ok.input.clone(), cont.ok.state.clone());
        self.0.run_once(cont).map(|(o, IOk { err, .. })| (o, IOk { input, state, err, cutted: false }))
    }
}
impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>> Parser<I, C, S, M> for Before<P> {
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<P::Output, I, S, M> {
        let (input, state) = (cont.ok.input.clone(), cont.ok.state.clone());
        self.0.run(cont).map(|(o, IOk { err, .. })| (o, IOk { input, state, err, cutted: false }))
    }
}

/// Swaps parser successes with failures that do not consume any input. Results will be discarded.
/// If the original parser consumes input and fails, the whole thing will fail.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(char('a').and(char('a')).parse_ok("aa"), Some(('a','a')));
/// assert_eq!(not_followed_by(char('a'),'a').and(char('a')).parse_ok("aa"), None);
/// assert_eq!(not_followed_by(char('b'),'b').and(char('a')).parse_ok("a"), Some(((),'a')));
/// assert_eq!(not_followed_by(char('b'),'b').and(any).parse_ok("b"), None);
/// assert_eq!(not_followed_by(char('b').and(char('a')),'b').and(any).parse_ok("bb"), None);
/// ```
#[derive(Clone, Copy)]
pub struct NotFollowedBy<P, L>(pub(crate) P, pub(crate) L);
#[inline]
pub fn not_followed_by<P, L: Display + 'static>(parser: P, label: L) -> NotFollowedBy<P, L> {
    NotFollowedBy(parser, label)
}
impl<I: Input, C, S: Clone, M: Cb, P: ParserOnce<I, C, S, M>, L: Display + 'static> ParserOnce<I, C, S, M>
    for NotFollowedBy<P, L>
{
    type Output = ();
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<(), I, S, M> {
        let ICont { ok, config, drop } = cont;
        let (input, state, pos) = (ok.input.clone(), ok.state.clone(), ok.input.pos());
        match run_drop(self.0, ICont { ok, config, drop: &mut || {} }, (input, state)) {
            (Ok((_, ok)), _) => {
                Err(Eb::unexpected(self.1).at::<I>(ok.input.index(), pos, Some(ok.input.pos())).or_merge(ok.err))
            },
            (Err(e), None) => {
                drop();
                Err(e)
            },
            (Err(e), Some((input, state))) => Ok(((), IOk { input, state, err: Some(e), cutted: false })),
        }
    }
}
impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>, L: Display + Clone + 'static> Parser<I, C, S, M>
    for NotFollowedBy<P, L>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<(), I, S, M> {
        let ICont { ok, config, drop } = cont;
        let (input, state, pos) = (ok.input.clone(), ok.state.clone(), ok.input.pos());
        match run_drop(self.0.to_ref(), ICont { ok, config, drop: &mut || {} }, (input, state)) {
            (Ok((_, ok)), _) => Err(Eb::unexpected(self.1.clone())
                .at::<I>(ok.input.index(), pos, Some(ok.input.pos()))
                .or_merge(ok.err)),
            (Err(e), None) => {
                drop();
                Err(e)
            },
            (Err(e), Some((input, state))) => Ok(((), IOk { input, state, err: Some(e), cutted: false })),
        }
    }
}
