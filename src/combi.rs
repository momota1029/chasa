use std::{borrow::Cow, marker::PhantomData};

use either::Either;

pub use super::using_macros::{chain, choice, skip_chain, tuple, Chain, ChainRight, Choice};
use super::{
    cont::Cont,
    error,
    error::ParseError,
    input::Input,
    input::Position,
    parser::{Args, Parser, ParserOnce},
    prim::{pure, Pure},
    util::Consume,
};
/**
If the first argument is Some, return its value; if None, execute the parser of the second argument.
# Example
```
use chasa::prelude::*;
assert_eq!(pure_or(Some("first"), str("second").to("second")).parse_ok("second"), Some("first"));
assert_eq!(pure_or(None, str("second").to("second")).parse_ok("second"), Some("second"))
```
*/
pub fn pure_or<O, I: Input, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>>(
    o: Option<O>, p: P,
) -> Either<Pure<O, I, E, C, S>, P> {
    match o {
        Some(o) => Either::Left(pure(o)),
        None => Either::Right(p),
    }
}

/**
Replace the parser result with the specified value.
*/
pub struct Value<P, O, Old>(pub(crate) P, pub(crate) O, pub(crate) PhantomData<fn() -> Old>);
impl<P: Clone, O: Clone, Old> Clone for Value<P, O, Old> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P: Copy, O: Copy, Old> Copy for Value<P, O, Old> {}
impl<I: Input, Old, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, Old, E, C, S>, O> ParserOnce<I, O, E, C, S>
    for Value<P, O, Old>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        self.0.run_once(args)?;
        Some(self.1)
    }
}
impl<I: Input, Old, E: ParseError<I>, C, S: Clone, P: Parser<I, Old, E, C, S>, O: Clone> Parser<I, O, E, C, S>
    for Value<P, O, Old>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.0.run(args)?;
        Some(self.1.clone())
    }
}

/// Process parser results with functions.
/// ```
/// use chasa::prelude::*;
/// let p = char('a').map(|c| {let mut str = String::new(); str.push(c); str});
/// assert_eq!(p.parse_ok("aa"), Some("a".to_string()));
/// ```
pub struct Map<P, F, O>(pub(crate) P, pub(crate) F, pub(crate) PhantomData<fn() -> O>);
impl<P: Clone, F: Clone, O> Clone for Map<P, F, O> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P: Copy, F: Copy, O> Copy for Map<P, F, O> {}
impl<I: Input, Old, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, Old, E, C, S>, F: FnOnce(Old) -> O, O>
    ParserOnce<I, O, E, C, S> for Map<P, F, Old>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        self.0.run_once(args).map(self.1)
    }
}
impl<I: Input, Old, E: ParseError<I>, C, S: Clone, P: Parser<I, Old, E, C, S>, F: FnMut(Old) -> O, O>
    Parser<I, O, E, C, S> for Map<P, F, Old>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.0.run(args).map(&mut self.1)
    }
}

/**
Add "expecting [specified label]" to the parser error display.
# Example
```
use chasa::prelude::*;
assert_eq!(char('a').label("special a").parse_easy("b"), Err("error: unexpected b, expecting special a".to_string()));
```
*/
#[derive(Clone, Copy)]
pub struct Label<P, L>(pub(crate) P, pub(crate) L);
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>, L> ParserOnce<I, O, E, C, S>
    for Label<P, L>
where
    E::Message: From<error::Expected<error::Format<L>>>,
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O> {
        match self.0.run_once(args.by_ref()) {
            Some(c) => Some(c),
            None => {
                args.error.clear_expected();
                args.error.set(error::expected(error::format(self.1)).into());
                None
            },
        }
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>, L: Clone> Parser<I, O, E, C, S>
    for Label<P, L>
where
    E::Message: From<error::Expected<error::Format<L>>>,
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O> {
        match self.0.run(args.by_ref()) {
            Some(c) => Some(c),
            None => {
                args.error.clear_expected();
                args.error.set(error::expected(error::format(self.1.clone())).into());
                None
            },
        }
    }
}

/**
Add "expecting [specified label]" to the parser error display. The label will be evaluated lazily.
# Example
```
use chasa::prelude::*;
assert_eq!(char('a').label_with(||"special a").parse_easy("b"), Err("error: unexpected b, expecting special a".to_string()));
```
*/
#[derive(Clone, Copy)]
pub struct LabelWith<P, F>(pub(crate) P, pub(crate) F);
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>, L, F: FnOnce() -> L>
    ParserOnce<I, O, E, C, S> for LabelWith<P, F>
where
    E::Message: From<error::Expected<error::Format<L>>>,
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O> {
        match self.0.run_once(args.by_ref()) {
            Some(c) => Some(c),
            None => {
                args.error.clear_expected();
                args.error.set(error::expected(error::format(self.1())).into());
                None
            },
        }
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>, L, F: FnMut() -> L> Parser<I, O, E, C, S>
    for LabelWith<P, F>
where
    E::Message: From<error::Expected<error::Format<L>>>,
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O> {
        match self.0.run(args.by_ref()) {
            Some(c) => Some(c),
            None => {
                args.error.clear_expected();
                args.error.set(error::expected(error::format(self.1())).into());
                None
            },
        }
    }
}

/**
Execute the next parser generated by accepting the result of the parser. If either of them fails, the whole thing will fail.
# Example
```
use chasa::prelude::*;
assert_eq!(any.bind(|c| char(c)).parse_ok("aa"), Some('a'));
assert_eq!(char('b').bind(|_| char('a')).parse_ok("aa"), None);
assert_eq!(any.bind(|c| char(c)).parse_ok("ab"), None);
```
*/
pub struct Bind<P, F, O1>(pub(crate) P, pub(crate) F, pub(crate) PhantomData<fn() -> O1>);
impl<P: Clone, F: Clone, O1> Clone for Bind<P, F, O1> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P: Copy, F: Copy, O1> Copy for Bind<P, F, O1> {}

impl<
        I: Input,
        O1,
        O2,
        E: ParseError<I>,
        C,
        S: Clone,
        P1: ParserOnce<I, O1, E, C, S>,
        P2: ParserOnce<I, O2, E, C, S>,
        F: FnOnce(O1) -> P2,
    > ParserOnce<I, O2, E, C, S> for Bind<P1, F, O1>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O2> {
        self.1(self.0.run_once(args.by_ref())?).run_once(args)
    }
}
impl<
        I: Input,
        O1,
        O2,
        E: ParseError<I>,
        C,
        S: Clone,
        P1: Parser<I, O1, E, C, S>,
        P2: ParserOnce<I, O2, E, C, S>,
        F: FnMut(O1) -> P2,
    > Parser<I, O2, E, C, S> for Bind<P1, F, O1>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O2> {
        self.1(self.0.run(args.by_ref())?).run_once(args)
    }
}

/// Execute the two parsers in succession and return them as tuples.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(any.and(any).parse_ok("aa"), Some(('a','a')))
/// ```
#[derive(Clone, Copy)]
pub struct And<P1, P2>(pub(crate) P1, pub(crate) P2);
impl<
        I: Input,
        O1,
        O2,
        E: ParseError<I>,
        C,
        S: Clone,
        P1: ParserOnce<I, O1, E, C, S>,
        P2: ParserOnce<I, O2, E, C, S>,
    > ParserOnce<I, (O1, O2), E, C, S> for And<P1, P2>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<(O1, O2)> {
        Some((self.0.run_once(args.by_ref())?, self.1.run_once(args)?))
    }
}
impl<I: Input, O1, O2, E: ParseError<I>, C, S: Clone, P1: Parser<I, O1, E, C, S>, P2: Parser<I, O2, E, C, S>>
    Parser<I, (O1, O2), E, C, S> for And<P1, P2>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<(O1, O2)> {
        Some((self.0.run(args.by_ref())?, self.1.run(args)?))
    }
}

/**
Runs two parsers in succession, returning only the first value. If either of them fails, the whole thing will fail.
# Example
```
use chasa::prelude::*;
assert_eq!(any.left(any).parse_ok("ab"), Some('a'));
assert_eq!(any.left(char('a')).parse_ok("ab"), None);
assert_eq!(str("chasa").to( ()).left(char(':')).parse_ok("chasa: parser combinator"), Some(()));
```
*/
pub struct Left<P1, P2, O2>(pub(crate) P1, pub(crate) P2, pub(crate) PhantomData<fn() -> O2>);
impl<P1: Clone, P2: Clone, O2> Clone for Left<P1, P2, O2> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P1: Copy, P2: Copy, O2> Copy for Left<P1, P2, O2> {}
impl<
        I: Input,
        O1,
        O2,
        E: ParseError<I>,
        C,
        S: Clone,
        P1: ParserOnce<I, O1, E, C, S>,
        P2: ParserOnce<I, O2, E, C, S>,
    > ParserOnce<I, O1, E, C, S> for Left<P1, P2, O2>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O1> {
        let left = self.0.run_once(args.by_ref());
        self.1.run_once(args)?;
        left
    }
}
impl<I: Input, O1, O2, E: ParseError<I>, C, S: Clone, P1: Parser<I, O1, E, C, S>, P2: Parser<I, O2, E, C, S>>
    Parser<I, O1, E, C, S> for Left<P1, P2, O2>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O1> {
        let left = self.0.run(args.by_ref());
        self.1.run(args)?;
        left
    }
}

/// Runs two parsers in succession, returning only the second value. If either of them fails, the whole thing will fail.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(any.right(any).parse_ok("ab"), Some('b'));
/// assert_eq!(char('b').right(any).parse_ok("ab"), None);
/// assert_eq!(char('b').right(str("chasa").to( ())).parse_ok("bchasa"), Some(()));
/// ```
pub struct Right<P1, P2, O1>(pub(crate) P1, pub(crate) P2, pub(crate) PhantomData<O1>);
impl<P1: Clone, P2: Clone, O1> Clone for Right<P1, P2, O1> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P1: Copy, P2: Copy, O1> Copy for Right<P1, P2, O1> {}
impl<
        I: Input,
        O1,
        O2,
        E: ParseError<I>,
        C,
        S: Clone,
        P1: ParserOnce<I, O1, E, C, S>,
        P2: ParserOnce<I, O2, E, C, S>,
    > ParserOnce<I, O2, E, C, S> for Right<P1, P2, O1>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O2> {
        self.0.run_once(args.by_ref())?;
        self.1.run_once(args)
    }
}
impl<I: Input, O1, O2, E: ParseError<I>, C, S: Clone, P1: Parser<I, O1, E, C, S>, P2: Parser<I, O2, E, C, S>>
    Parser<I, O2, E, C, S> for Right<P1, P2, O1>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O2> {
        self.0.run(args.by_ref())?;
        self.1.run(args)
    }
}

/// Place the parser between two parsers. The results at both ends will be ignored.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(any.between(char('('), char(')')).parse_ok("(a)"), Some('a'));
/// assert_eq!(char('a').between(char('('), char(')')).parse_ok("(a"), None);
/// ```
pub struct Between<P1, P2, P3, O1, O3>(
    pub(crate) P1,
    pub(crate) P2,
    pub(crate) P3,
    pub(crate) PhantomData<fn() -> (O1, O3)>,
);
impl<P1: Clone, P2: Clone, P3: Clone, O1, O3> Clone for Between<P1, P2, P3, O1, O3> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), self.2.clone(), PhantomData)
    }
}
impl<P1: Copy, P2: Copy, P3: Copy, O1, O3> Copy for Between<P1, P2, P3, O1, O3> {}
impl<
        I: Input,
        O1,
        O2,
        O3,
        E: ParseError<I>,
        C,
        S: Clone,
        P1: ParserOnce<I, O1, E, C, S>,
        P2: ParserOnce<I, O2, E, C, S>,
        P3: ParserOnce<I, O3, E, C, S>,
    > ParserOnce<I, O2, E, C, S> for Between<P1, P2, P3, O1, O3>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O2> {
        self.0.run_once(args.by_ref())?;
        let center = self.1.run_once(args.by_ref());
        self.2.run_once(args)?;
        center
    }
}
impl<
        I: Input,
        O1,
        O2,
        O3,
        E: ParseError<I>,
        C,
        S: Clone,
        P1: Parser<I, O1, E, C, S>,
        P2: Parser<I, O2, E, C, S>,
        P3: Parser<I, O3, E, C, S>,
    > Parser<I, O2, E, C, S> for Between<P1, P2, P3, O1, O3>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O2> {
        self.0.run(args.by_ref())?;
        let center = self.1.run(args.by_ref());
        self.2.run(args)?;
        center
    }
}

/**
Sift through the parser results. If a token is sifted out, it does not consume input.
# Example
```
use chasa::prelude::*;
let p = any.and_then(|c| match c {
    'a' => Ok(true),
    _ => Err(message(format("hello")))
});
assert_eq!(p.parse_ok("abc"), Some(true));
assert_eq!(p.parse_easy("cba"), Err("error: hello".to_string()));
```
*/
pub struct AndThen<P, F, M, O>(pub(crate) P, pub(crate) F, pub(crate) PhantomData<fn() -> (O, M)>);
impl<P: Clone, F: Clone, M, O> Clone for AndThen<P, F, M, O> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P: Copy, F: Copy, M, O> Copy for AndThen<P, F, M, O> {}
impl<
        I: Input,
        O1,
        O2,
        E: ParseError<I>,
        C,
        S: Clone,
        P: ParserOnce<I, O1, E, C, S>,
        F: FnOnce(O1) -> Result<O2, M>,
        M: Into<E::Message>,
    > ParserOnce<I, O2, E, C, S> for AndThen<P, F, M, O1>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O2> {
        let start = args.input.position();
        self.0.run_once(args.by_ref()).and_then(|o| match self.1(o) {
            Ok(o) => Some(o),
            Err(m) => {
                let end = args.input.position();
                if args.error.add(Some(start), end) {
                    args.error.set(m.into());
                }
                None
            },
        })
    }
}
impl<
        I: Input,
        O1,
        O2,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, O1, E, C, S>,
        F: FnMut(O1) -> Result<O2, M>,
        M: Into<E::Message>,
    > Parser<I, O2, E, C, S> for AndThen<P, F, M, O1>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O2> {
        let start = args.input.position();
        self.0.run(args.by_ref()).and_then(|o| match self.1(o) {
            Ok(o) => Some(o),
            Err(m) => {
                let end = args.input.position();
                if args.error.add(Some(start), end) {
                    args.error.set(m.into());
                }
                None
            },
        })
    }
}
// impl<I: Input, Output, C, S, M: Cb, P: ParserOnce<I, Output, C, S, M>, O, F: FnOnce(Output) -> Result<O, Eb<M>>>
//     ParserOnce<I, O, C, S, M> for AndThen<P, F, Output>
// {
//     #[inline(always)]
//     fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
//         let ICont { ok, config, drop } = cont;
//         let pos = ok.input.pos();
//         self.0.run_once(ICont { ok, config, drop }).and_then(|(o, ok)| match self.1(o) {
//             Ok(o) => Ok((o, ok)),
//             Err(err) => Err(err.at::<I>(ok.input.index(), pos, Some(ok.input.pos())).or_merge(ok.err)),
//         })
//     }
// }
// impl<I: Input, Output, C, S, M: Cb, P: Parser<I, Output, C, S, M>, O, F: Fn(Output) -> Result<O, Eb<M>>>
//     Parser<I, O, C, S, M> for AndThen<P, F, Output>
// {
//     #[inline(always)]
//     fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
//         let ICont { ok, config, drop } = cont;
//         let pos = ok.input.pos();
//         self.0.run(ICont { ok, config, drop }).and_then(|(o, ok)| match self.1(o) {
//             Ok(o) => Ok((o, ok)),
//             Err(err) => Err(err.at::<I>(ok.input.index(), pos, Some(ok.input.pos())).or_merge(ok.err)),
//         })
//     }
// }

/// Pass a value to chain the parser together with the parser result, and let the parser continue.
/// Even if `bind` returns a lot of parsers of different types, `case` does not need to use `Either` artificially
/// # Example
/// ```
/// use chasa::prelude::*;
/// fn parser<'a>() -> impl Parser<&'a str,usize> {
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
pub struct Case<P, F, O, E>(pub(crate) P, pub(crate) F, pub(crate) PhantomData<fn() -> (O, E)>);
impl<P: Clone, F: Clone, O, E> Clone for Case<P, F, O, E> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P: Copy, F: Copy, O, E> Copy for Case<P, F, O, E> {}
impl<
        I: Input,
        O1,
        O2,
        E: ParseError<I>,
        C,
        S: Clone,
        P: ParserOnce<I, O1, E, C, S>,
        F: for<'a, 'b> FnOnce(O1, Args<'a, 'b, I, E, C, S>) -> Cont<'a, 'b, I, O2, E, C, S>,
    > ParserOnce<I, O2, E, C, S> for Case<P, F, O1, E>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O2> {
        Some(self.1(self.0.run_once(args.by_ref())?, args).0?.0)
    }
}
impl<
        I: Input,
        O1,
        O2,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, O1, E, C, S>,
        F: for<'a, 'b> FnMut(O1, Args<'a, 'b, I, E, C, S>) -> Cont<'a, 'b, I, O2, E, C, S>,
    > Parser<I, O2, E, C, S> for Case<P, F, O1, E>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O2> {
        Some(self.1(self.0.run(args.by_ref())?, args).0?.0)
    }
}

/// If the first parser fails without consuming any input, try the next parser.
/// It is more efficient to assume that the syntax is determined when the first parser consumes input.
/// See also [`Cut`] for input consumption.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(char('a').or(char('b')).parse_ok("aa"), Some('a'));
/// assert_eq!(char('a').or(char('b')).parse_ok("bb"), Some('b'));
/// assert_eq!(char('a').right(char('b')).or(char('b').right(char('b'))).parse_ok("bb"), Some('b'));
/// assert_eq!(char('b').right(char('b')).or(char('b').right(char('a'))).parse_ok("ba"), None);
/// ```
#[derive(Clone, Copy)]
pub struct Or<P1, P2>(pub(crate) P1, pub(crate) P2);
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P1: ParserOnce<I, O, E, C, S>, P2: ParserOnce<I, O, E, C, S>>
    ParserOnce<I, O, E, C, S> for Or<P1, P2>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args.by_ref();
        match consume.cons((input.clone(), state.clone()), |consume| {
            self.0.run_once(Args { input, config, state, consume, error })
        }) {
            (None, None) => None,
            (Some(o), _) => Some(o),
            (None, Some((input_bak, state_bak))) => {
                *input = input_bak;
                *state = state_bak;
                self.1.run_once(Args { input, config, state, consume, error })
            },
        }
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P1: Parser<I, O, E, C, S>, P2: Parser<I, O, E, C, S>>
    Parser<I, O, E, C, S> for Or<P1, P2>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        match consume
            .cons((input.clone(), state.clone()), |consume| self.0.run(Args { input, config, state, consume, error }))
        {
            (None, None) => None,
            (Some(o), _) => Some(o),
            (None, Some((input_bak, state_bak))) => {
                *input = input_bak;
                *state = state_bak;
                self.1.run(Args { input, config, state, consume, error })
            },
        }
    }
}

/// It returns `Some` if the parser succeeds, returns `None` if the parser does not consume any input and fails, and fails if the parser consumes some input and fails.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(char('a').or_not().parse_ok("aa"), Some(Some('a')));
/// assert_eq!(char('a').or_not().parse_ok("bb"), Some(None));
/// assert_eq!(char('a').right(char('b')).or_not().parse_ok("bb"), Some(None));
/// assert_eq!(char('b').right(char('b')).or_not().parse_ok("ba"), None);
/// ```
#[derive(Clone, Copy)]
pub struct OrNot<P>(pub(crate) P);
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>> ParserOnce<I, Option<O>, E, C, S>
    for OrNot<P>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<Option<O>> {
        let Args { input, config, state, consume, error } = args;
        match consume.cons((input.clone(), state.clone()), |consume| {
            self.0.run_once(Args { input, config, state, consume, error })
        }) {
            (None, None) => None,
            (Some(o), _) => Some(Some(o)),
            (None, Some((input_bak, state_bak))) => {
                *input = input_bak;
                *state = state_bak;
                Some(None)
            },
        }
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>> Parser<I, Option<O>, E, C, S> for OrNot<P> {
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<Option<O>> {
        let Args { input, config, state, consume, error } = args;
        match consume
            .cons((input.clone(), state.clone()), |consume| self.0.run(Args { input, config, state, consume, error }))
        {
            (None, None) => None,
            (Some(o), _) => Some(Some(o)),
            (None, Some((input_bak, state_bak))) => {
                *input = input_bak;
                *state = state_bak;
                Some(None)
            },
        }
    }
}

#[derive(Clone, Copy)]
pub struct OrWith<P, R, F>(pub(crate) P, pub(crate) R, pub(crate) F);
impl<
        I: Input,
        O,
        E: ParseError<I>,
        C,
        S: Clone,
        P: ParserOnce<I, O, E, C, S>,
        R,
        F: FnOnce(R) -> Q,
        Q: ParserOnce<I, O, E, C, S>,
    > ParserOnce<I, O, E, C, S> for OrWith<P, R, F>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        let (input_bak, state_bak) = (input.clone(), state.clone());
        match consume.wrap(|consume| {
            consume.cons((input_bak, state_bak, self.1), |consume| {
                consume.wrap(|consume| self.0.run_once(Args { input, config, state, consume, error }))
            })
        }) {
            (Some(o), _) => Some(o),
            (None, Some((input_bak, state_bak, r))) => {
                *input = input_bak;
                *state = state_bak;
                self.2(r).run_once(Args { input, config, state, consume, error })
            },
            (None, None) => None,
        }
    }
}
impl<
        I: Input,
        O,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, O, E, C, S>,
        R: Clone,
        F: FnMut(R) -> Q,
        Q: ParserOnce<I, O, E, C, S>,
    > Parser<I, O, E, C, S> for OrWith<P, R, F>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        let (input_bak, state_bak) = (input.clone(), state.clone());
        match consume.wrap(|consume| {
            consume.cons((input_bak, state_bak, self.1.clone()), |consume| {
                consume.wrap(|consume| self.0.run(Args { input, config, state, consume, error }))
            })
        }) {
            (Some(o), _) => Some(o),
            (None, Some((input_bak, state_bak, r))) => {
                *input = input_bak;
                *state = state_bak;
                self.2(r).run_once(Args { input, config, state, consume, error })
            },
            (None, None) => None,
        }
    }
}

/// If the parser fails, it will not consume any input. This is useful for parsing tokens that have multiple parts combined.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(char('b').right(char('b')).or(char('b').right(char('a'))).parse_ok("ba"), None);
/// assert_eq!(char('b').right(char('b')).cut().or(char('b').right(char('a'))).parse_ok("ba"), Some('a'));
/// ```
#[derive(Clone, Copy)]
pub struct Cut<P>(pub(crate) P);
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>> ParserOnce<I, O, E, C, S> for Cut<P> {
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume: _, error } = args;
        self.0.run_once(Args { input, config, state, error, consume: &mut Consume::new() })
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>> Parser<I, O, E, C, S> for Cut<P> {
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume: _, error } = args;
        self.0.run(Args { input, config, state, error, consume: &mut Consume::new() })
    }
}

/// Returns the parser result with the position before and after the parse.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(char('a').ranged().parse_ok(pos_str("a")), Some(('a',1,2)));
/// assert_eq!(str("abcd").to(()).ranged().parse_ok(pos_str("abcd")), Some(((),1,5)));
/// assert_eq!(str("abcd").right(str("efg").to(()).ranged()).parse_ok(pos_str("abcdefg")), Some(((),5,8)))
/// ```
#[derive(Clone, Copy)]
pub struct Ranged<P>(pub(crate) P);
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>>
    ParserOnce<I, (O, I::Position, I::Position), E, C, S> for Ranged<P>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<(O, I::Position, I::Position)> {
        let pos = args.input.position();
        self.0.run_once(args.by_ref()).map(|o| (o, pos, args.input.position()))
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>>
    Parser<I, (O, I::Position, I::Position), E, C, S> for Ranged<P>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<(O, I::Position, I::Position)> {
        let pos = args.input.position();
        self.0.run(args.by_ref()).map(|o| (o, pos, args.input.position()))
    }
}

/// Returns together with the string accepted by the parser.
#[derive(Clone, Copy)]
pub struct GetString<P, B>(pub(crate) P, pub(crate) PhantomData<fn() -> B>);
impl<B: FromIterator<I::Token>, I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>>
    ParserOnce<I, (O, B), E, C, S> for GetString<P, B>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<(O, B)> {
        let mut input = args.input.clone();
        let o = self.0.run_once(args.by_ref())?;
        let end = args.input.position().offset();
        Some((o, InputIter { input: &mut input, end }.collect()))
    }
}

impl<B: FromIterator<I::Token>, I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>>
    Parser<I, (O, B), E, C, S> for GetString<P, B>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<(O, B)> {
        let mut input = args.input.clone();
        let o = self.0.run(args.by_ref())?;
        let end = args.input.position().offset();
        Some((o, InputIter { input: &mut input, end }.collect()))
    }
}

pub struct GetStringExtend<P, B>(pub(crate) P, pub(crate) B);
#[inline(always)]
pub fn extend_with_str<B, P>(str: B, parser: P) -> GetStringExtend<P, B> {
    GetStringExtend(parser, str)
}

impl<B: Extend<I::Token> + Clone, I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>>
    Parser<I, (O, B), E, C, S> for GetStringExtend<P, B>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<(O, B)> {
        GetStringExtend(self.0.by_ref(), self.1.clone()).run_once(args)
    }
}
impl<B: Extend<I::Token>, I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>>
    ParserOnce<I, (O, B), E, C, S> for GetStringExtend<P, B>
{
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<(O, B)> {
        let mut input = args.input.clone();
        let o = self.0.run_once(args.by_ref())?;
        let end = args.input.position().offset();
        let mut b = self.1;
        b.extend(InputIter { input: &mut input, end });
        Some((o, b))
    }
}

struct InputIter<'a, I: Input> {
    input: &'a mut I,
    end: <I::Position as Position>::Offset,
}
impl<'a, I: Input> Iterator for InputIter<'a, I> {
    type Item = I::Token;
    #[inline(always)]
    fn next(&mut self) -> Option<I::Token> {
        if self.input.position().offset() < self.end {
            self.input.uncons().ok()
        } else {
            None
        }
    }
}
/// If successful, it does not consume input. The subsequent parser will read the same part again.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(char('a').and(char('a')).parse_ok("a"), None);
/// assert_eq!(before(char('a')).and(char('a')).parse_ok("a"), Some(('a','a')));
/// assert_eq!(char('a').and(char('b')).parse_ok("ab"), Some(('a','b')));
/// assert_eq!(before(char('a')).and(char('b')).parse_ok("ab"), None);
/// ```
#[derive(Clone, Copy)]
pub struct Before<P>(pub(crate) P);
#[inline(always)]
pub fn before<P>(parser: P) -> Before<P> {
    Before(parser)
}

impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>> ParserOnce<I, O, E, C, S> for Before<P> {
    #[inline(always)]
    fn run_once(self, mut args: Args<I, E, C, S>) -> Option<O> {
        let (input, state) = (args.input.clone(), args.state.clone());
        let res = self.0.run_once(args.by_ref());
        if res.is_some() {
            *args.input = input;
            *args.state = state;
        }
        res
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>> Parser<I, O, E, C, S> for Before<P> {
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O> {
        let (input, state) = (args.input.clone(), args.state.clone());
        let res = self.0.run(args.by_ref());
        if res.is_some() {
            *args.input = input;
            *args.state = state;
        }
        res
    }
}

/// Swaps parser successes with failures that do not consume any input. Results will be discarded.
/// If the original parser consumes input and fails, the whole thing will fail.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(char('a').and(char('a')).parse_ok("aa"), Some(('a','a')));
/// assert_eq!(not_followed_by(char('a'),"a").and(char('a')).parse_ok("aa"), None);
/// assert_eq!(not_followed_by(char('b'),"b").and(char('a')).parse_ok("a"), Some(((),'a')));
/// assert_eq!(not_followed_by(char('b'),"b").and(any).parse_ok("b"), None);
/// assert_eq!(not_followed_by(char('b').and(char('a')),"b").and(any).parse_ok("bb"), None);
/// ```
pub struct NotFollowedBy<P, L, O>(pub(crate) P, pub(crate) L, pub(crate) PhantomData<O>);
impl<P: Clone, L: Clone, O> Clone for NotFollowedBy<P, L, O> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P: Copy, L: Copy, O> Copy for NotFollowedBy<P, L, O> {}
#[inline(always)]
pub fn not_followed_by<P, L: Into<Cow<'static, str>>, O>(parser: P, label: L) -> NotFollowedBy<P, L, O> {
    NotFollowedBy(parser, label, PhantomData)
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, O, E, C, S>, L> ParserOnce<I, (), E, C, S>
    for NotFollowedBy<P, L, O>
where
    E::Message: From<error::Unexpected<error::Format<L>>>,
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<()> {
        let Args { input, config, state, consume, error } = args;
        let start = input.position();
        match consume.cons((input.clone(), state.clone()), |consume| {
            self.0.run_once(Args { input, config, state, consume, error })
        }) {
            (None, None) => None,
            (Some(_), _) => {
                let end = input.position();
                if error.add(Some(start), end) {
                    error.set(error::unexpected(error::format(self.1)).into())
                }
                None
            },
            (None, Some((input_bak, state_bak))) => {
                *input = input_bak;
                *state = state_bak;
                Some(())
            },
        }
    }
}
impl<I: Input, O, E: ParseError<I>, C, S: Clone, P: Parser<I, O, E, C, S>, L: Clone> Parser<I, (), E, C, S>
    for NotFollowedBy<P, L, O>
where
    E::Message: From<error::Unexpected<error::Format<L>>>,
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<()> {
        let Args { input, config, state, consume, error } = args;
        let start = input.position();
        match consume
            .cons((input.clone(), state.clone()), |consume| self.0.run(Args { input, config, state, consume, error }))
        {
            (None, None) => None,
            (Some(_), _) => {
                let end = input.position();
                if error.add(Some(start), end) {
                    error.set(error::unexpected(error::format(self.1.clone())).into())
                }
                None
            },
            (None, Some((input_bak, state_bak))) => {
                *input = input_bak;
                *state = state_bak;
                Some(())
            },
        }
    }
}
