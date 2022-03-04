use std::{
    iter::once,
};

use crate::{
    error::{CustomBuilder as Cb},
    util::run_drop,
    ICont, IOk, IResult, Input, Parser, ParserOnce,
};

fn run_fold<O, I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>>(
    o: O, p: P, cont: ICont<I, C, S, M>, f: impl Fn(O, P::Output) -> O,
) -> IResult<O, I, S, M> {
    let ICont { ok, config, drop } = cont;
    let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
    match run_drop(p.to_ref(), ICont { ok, config, drop }, (input, state)) {
        (Ok((o2, ok)), _) => run_fold(f(o, o2), p, ok.to_cont(config, drop), f),
        (Err(e), None) => Err(e),
        (Err(e), Some((input, state))) => Ok((o, IOk { input, state, cutted, err: Some(e) })),
    }
}

/// Run the parser greedily as many times as possible and fold the results.
/// # Example
/// ```
/// use chasa::*;
/// let d = one_of("0123456789").and_then(|c: char| c.to_string().parse::<usize>().map_err(message));
/// let d = d.to_ref();
/// assert_eq!(d.fold(0,|a,b| a+b).parse_ok("12345"), Some(15));
/// assert_eq!(d.fold(0,|a,b| a+b).parse_ok(""), Some(0));
/// assert_eq!(d.fold(0,|a,b| a+b).and(d).parse_ok("12345"), None);
/// ```
#[derive(Clone, Copy)]
pub struct Fold<T, P, F>(T, P, F);
#[inline]
pub fn fold<T, P, F>(init: T, parser: P, succ: F) -> Fold<T, P, F> {
    Fold(init, parser, succ)
}
impl<I: Input, C, S: Clone, M: Cb, T, P: Parser<I, C, S, M>, F: Fn(T, P::Output) -> T> ParserOnce<I, C, S, M>
    for Fold<T, P, F>
{
    type Output = T;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<T, I, S, M> {
        run_fold(self.0, self.1, cont, self.2)
    }
}
impl<I: Input, C, S: Clone, M: Cb, T: Clone, P: Parser<I, C, S, M>, F: Fn(T, P::Output) -> T> Parser<I, C, S, M>
    for Fold<T, P, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<T, I, S, M> {
        run_fold(self.0.clone(), self.1.to_ref(), cont, &self.2)
    }
}

/// Run the parser greedily as many times as possible and fold the results. Require more than one success.
/// # Example
/// ```
/// use chasa::*;
/// let d = one_of("0123456789").and_then(|c: char| c.to_string().parse::<usize>().map_err(message));
/// let d = d.to_ref();
/// assert_eq!(d.fold1(0,|a,b| a+b).parse_ok("12345"), Some(15));
/// assert_eq!(d.fold1(0,|a,b| a+b).parse_ok(""), None);
/// assert_eq!(d.fold1(0,|a,b| a+b).and(d).parse_ok("12345"), None);
/// ```
#[derive(Clone, Copy)]
pub struct Fold1<T, P, F>(T, P, F);
#[inline]
pub fn fold1<T, P, F>(init: T, parser: P, succ: F) -> Fold1<T, P, F> {
    Fold1(init, parser, succ)
}
impl<I: Input, C, S: Clone, M: Cb, T, P: Parser<I, C, S, M>, F: Fn(T, P::Output) -> T> ParserOnce<I, C, S, M>
    for Fold1<T, P, F>
{
    type Output = T;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<T, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.1
            .run(ICont { ok, config, drop })
            .and_then(|(o, ok)| run_fold(self.2(self.0, o), self.1, ok.to_cont(config, drop), self.2))
    }
}
impl<I: Input, C, S: Clone, M: Cb, T: Clone, P: Parser<I, C, S, M>, F: Fn(T, P::Output) -> T> Parser<I, C, S, M>
    for Fold1<T, P, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<T, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.1
            .run(ICont { ok, config, drop })
            .and_then(|(o, ok)| run_fold(self.2(self.0.clone(), o), self.1.to_ref(), ok.to_cont(config, drop), &self.2))
    }
}

// sepが成功した後に本体がempty failしても元に戻る仕組み
fn run_left_sep<
    I: Input,
    C,
    S: Clone,
    M: Cb,
    T,
    P1: Parser<I, C, S, M>,
    P2: Parser<I, C, S, M>,
    F: Fn(T, P1::Output) -> T,
>(
    t: T, p: P1, sep: P2, succ: F, cont: ICont<I, C, S, M>,
) -> IResult<T, I, S, M> {
    let ICont { ok, config, drop } = cont;
    let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
    if cutted {
        drop()
    }
    match run_drop(sep.to_ref(), ICont { ok, config, drop }, ()) {
        (Err(e), None) => Err(e),
        (Err(e), Some(())) => Ok((t, IOk { input, state, cutted, err: Some(e) })),
        (Ok((_, ok)), _) => {
            if ok.cutted {
                drop()
            }
            match run_drop(p.to_ref(), ok.to_cont(config, drop), (input, state)) {
                (Err(e), None) => Err(e),
                (Err(e), Some((input, state))) => Ok((t, IOk { input, state, cutted, err: Some(e) })),
                (Ok((o, ok)), _) => run_left_sep(succ(t, o), p, sep, succ, ok.to_cont(config, drop)),
            }
        },
    }
}

/// The parser will be separated by another parser (the result will be discarded) and folded.
/// # Example
/// ```
/// use chasa::*;
/// let d = one_of("0123456789").and_then(|c: char| c.to_string().parse::<usize>().map_err(message));
/// let d = d.to_ref();
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).parse_ok("1,2,3,4,5"), Some(15));
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).parse_ok(""), Some(0));
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).and(char(',').right(d)).parse_ok("1,2,3,4,5,6"), None);
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).and(char(',')).parse_ok("1,2,3,4,"), Some((10, ',')));
/// ```
#[derive(Clone, Copy)]
pub struct SepFold<T, P1, P2, F> {
    pub(crate) init: T,
    pub(crate) p: P1,
    pub(crate) sep: P2,
    pub(crate) succ: F,
}

impl<I: Input, C, S: Clone, M: Cb, T, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>, F: Fn(T, P1::Output) -> T>
    ParserOnce<I, C, S, M> for SepFold<T, P1, P2, F>
{
    type Output = T;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<T, I, S, M> {
        let ICont { ok, config, drop } = cont;
        let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
        match run_drop(self.p.to_ref(), ICont { ok, config, drop }, (input, state)) {
            (Ok((o, ok)), _) => {
                run_left_sep((self.succ)(self.init, o), self.p, self.sep, self.succ, ok.to_cont(config, drop))
            },
            (Err(e), None) => Err(e),
            (Err(e), Some((input, state))) => Ok((self.init, IOk { input, state, cutted, err: Some(e) })),
        }
    }
}
impl<
        I: Input,
        C,
        S: Clone,
        M: Cb,
        T: Clone,
        P1: Parser<I, C, S, M>,
        P2: Parser<I, C, S, M>,
        F: Fn(T, P1::Output) -> T,
    > Parser<I, C, S, M> for SepFold<T, P1, P2, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<T, I, S, M> {
        let ICont { ok, config, drop } = cont;
        let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
        match run_drop(self.p.to_ref(), ICont { ok, config, drop }, (input, state)) {
            (Ok((o, ok)), _) => run_left_sep(
                (self.succ)(self.init.clone(), o),
                self.p.to_ref(),
                self.sep.to_ref(),
                &self.succ,
                ok.to_cont(config, drop),
            ),
            (Err(e), None) => Err(e),
            (Err(e), Some((input, state))) => Ok((self.init.clone(), IOk { input, state, cutted, err: Some(e) })),
        }
    }
}

// sepが成功した後に本体がempty failしても元に戻る仕組み
fn run_left_sep1<
    I: Input,
    C,
    S: Clone,
    M: Cb,
    P1: Parser<I, C, S, M>,
    P2: Parser<I, C, S, M>,
    F: Fn(P1::Output, P2::Output, P1::Output) -> P1::Output,
>(
    t: P1::Output, p: P1, sep: P2, succ: F, cont: ICont<I, C, S, M>,
) -> IResult<P1::Output, I, S, M> {
    let ICont { ok, config, drop } = cont;
    let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
    if cutted {
        drop()
    }
    match run_drop(sep.to_ref(), ICont { ok, config, drop }, ()) {
        (Err(e), None) => Err(e),
        (Err(e), Some(())) => Ok((t, IOk { input, state, cutted, err: Some(e) })),
        (Ok((op, ok)), _) => {
            if ok.cutted {
                drop()
            }
            match run_drop(p.to_ref(), ok.to_cont(config, drop), (input, state)) {
                (Err(e), None) => Err(e),
                (Err(e), Some((input, state))) => Ok((t, IOk { input, state, cutted, err: Some(e) })),
                (Ok((o, ok)), _) => run_left_sep1(succ(t, op, o), p, sep, succ, ok.to_cont(config, drop)),
            }
        },
    }
}

/// The parser will be separated by another parser (the result will be discarded) and folded. Require more than one success.
/// It can be used for folding without discarding the separator.
/// # Example
/// ```
/// use chasa::*;
/// let d = one_of("0123456789").and_then(|c: char| c.to_string().parse::<isize>().map_err(message));
/// let p = d.sep_fold1(char(','),|a,_,b| a+b);
/// assert_eq!(p.parse_ok("1,2,3,4,5"), Some(15));
/// assert_eq!(p.parse_ok(""), None);
/// assert_eq!(p.and(char(',').right(d)).parse_ok("1,2,3,4,5,6"), None);
/// assert_eq!(p.and(char(',')).parse_ok("1,2,3,4,"), Some((10, ',')));
///
/// let op = char('+').to(1).or(char('-').to(-1));
/// let p = d.sep_fold1(op, |a,sign,b| a + sign*b);
/// assert_eq!(p.parse_ok("1+2+3-4+5"), Some(7));
/// ```
#[derive(Clone, Copy)]
pub struct SepFold1<P1, P2, F> {
    pub(crate) p: P1,
    pub(crate) sep: P2,
    pub(crate) succ: F,
}
impl<
        I: Input,
        C,
        S: Clone,
        M: Cb,
        P1: Parser<I, C, S, M>,
        P2: Parser<I, C, S, M>,
        F: Fn(P1::Output, P2::Output, P1::Output) -> P1::Output,
    > ParserOnce<I, C, S, M> for SepFold1<P1, P2, F>
{
    type Output = P1::Output;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<P1::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.p
            .run(ICont { ok, config, drop })
            .and_then(|(o, ok)| run_left_sep1(o, self.p, self.sep, self.succ, ok.to_cont(config, drop)))
    }
}
impl<
        I: Input,
        C,
        S: Clone,
        M: Cb,
        P1: Parser<I, C, S, M>,
        P2: Parser<I, C, S, M>,
        F: Fn(P1::Output, P2::Output, P1::Output) -> P1::Output,
    > Parser<I, C, S, M> for SepFold1<P1, P2, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<P1::Output, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.p.run(ICont { ok, config, drop }).and_then(|(o, ok)| {
            run_left_sep1(o, self.p.to_ref(), self.sep.to_ref(), &self.succ, ok.to_cont(config, drop))
        })
    }
}

fn run_extend<O: Extend<P::Output>, I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>>(
    mut o: O, p: P, cont: ICont<I, C, S, M>,
) -> IResult<O, I, S, M> {
    let ICont { ok, config, drop } = cont;
    let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
    match run_drop(p.to_ref(), ICont { ok, config, drop }, (input, state)) {
        (Ok((o2, ok)), _) => {
            o.extend(once(o2));
            run_extend(o, p, ok.to_cont(config, drop))
        },
        (Err(e), None) => Err(e),
        (Err(e), Some((input, state))) => Ok((o, IOk { input, state, cutted, err: Some(e) })),
    }
}

/// Repeat extend with parser results.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(any.extend(String::new()).parse_ok("abcde"), Some("abcde".to_string()));
/// assert_eq!(char('a').extend(String::new()).parse_ok("aaabbb"), Some("aaa".to_string()));
/// assert_eq!(char('a').extend(String::new()).parse_ok("b"), Some("".to_string()));
/// ```
#[derive(Clone, Copy)]
pub struct ExtendParser<O, P>(pub(crate) O, pub(crate) P);
impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>, O: Extend<P::Output>> ParserOnce<I, C, S, M>
    for ExtendParser<O, P>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        run_extend(self.0, self.1, cont)
    }
}
impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>, O: Extend<P::Output> + Clone> Parser<I, C, S, M>
    for ExtendParser<O, P>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        run_extend(self.0.clone(), self.1.to_ref(), cont)
    }
}

/// Repeat extend with parser results. Require more than one success.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(any.extend1(String::new()).parse_ok("abcde"), Some("abcde".to_string()));
/// assert_eq!(char('a').extend1(String::new()).parse_ok("aaabbb"), Some("aaa".to_string()));
/// assert_eq!(char('a').extend1(String::new()).parse_ok("b"), None);
/// ```
#[derive(Clone, Copy)]
pub struct Extend1Parser<O, P>(pub(crate) O, pub(crate) P);
impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>, O: Extend<P::Output>> ParserOnce<I, C, S, M>
    for Extend1Parser<O, P>
{
    type Output = O;
    #[inline]
    fn run_once(mut self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.1.run(ICont { ok, config, drop }).and_then(|(o, ok)| {
            self.0.extend(once(o));
            run_extend(self.0, self.1, ok.to_cont(config, drop))
        })
    }
}
impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>, O: Extend<P::Output> + Clone> Parser<I, C, S, M>
    for Extend1Parser<O, P>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.1.run(ICont { ok, config, drop }).and_then(|(o, ok)| {
            let mut init = self.0.clone();
            init.extend(once(o));
            run_extend(init, self.1.to_ref(), ok.to_cont(config, drop))
        })
    }
}

// sepが成功した後に本体がempty failしても元に戻る仕組み
fn run_left_sep_extend<
    I: Input,
    C,
    S: Clone,
    M: Cb,
    T: Extend<P1::Output>,
    P1: Parser<I, C, S, M>,
    P2: Parser<I, C, S, M>,
>(
    mut t: T, p: P1, sep: P2, cont: ICont<I, C, S, M>,
) -> IResult<T, I, S, M> {
    let ICont { ok, config, drop } = cont;
    let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
    if cutted {
        drop()
    }
    match run_drop(sep.to_ref(), ICont { ok, config, drop }, ()) {
        (Err(e), None) => Err(e),
        (Err(e), Some(())) => Ok((t, IOk { input, state, cutted, err: Some(e) })),
        (Ok((_, ok)), _) => {
            if ok.cutted {
                drop()
            }
            match run_drop(p.to_ref(), ok.to_cont(config, drop), (input, state)) {
                (Err(e), None) => Err(e),
                (Err(e), Some((input, state))) => Ok((t, IOk { input, state, cutted, err: Some(e) })),
                (Ok((o, ok)), _) => {
                    t.extend(once(o));
                    run_left_sep_extend(t, p, sep, ok.to_cont(config, drop))
                },
            }
        },
    }
}

/// Repeat the parser, separating it with other parsers, and extend it.
#[derive(Clone, Copy)]
pub struct SepExtend<O, P1, P2> {
    pub(crate) init: O,
    pub(crate) p: P1,
    pub(crate) sep: P2,
}
impl<I: Input, C, S: Clone, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>, O: Extend<P1::Output>>
    ParserOnce<I, C, S, M> for SepExtend<O, P1, P2>
{
    type Output = O;
    #[inline]
    fn run_once(mut self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
        match run_drop(self.p.to_ref(), ICont { ok, config, drop }, (input, state)) {
            (Ok((o, ok)), _) => {
                self.init.extend(once(o));
                run_left_sep_extend(self.init, self.p, self.sep, ok.to_cont(config, drop))
            },
            (Err(e), None) => Err(e),
            (Err(e), Some((input, state))) => Ok((self.init, IOk { input, state, cutted, err: Some(e) })),
        }
    }
}
impl<I: Input, C, S: Clone, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>, O: Extend<P1::Output> + Clone>
    Parser<I, C, S, M> for SepExtend<O, P1, P2>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
        match run_drop(self.p.to_ref(), ICont { ok, config, drop }, (input, state)) {
            (Ok((o, ok)), _) => {
                let mut init = self.init.clone();
                init.extend(once(o));
                run_left_sep_extend(init, self.p.to_ref(), self.sep.to_ref(), ok.to_cont(config, drop))
            },
            (Err(e), None) => Err(e),
            (Err(e), Some((input, state))) => Ok((self.init.clone(), IOk { input, state, cutted, err: Some(e) })),
        }
    }
}

/// Repeat the parser, separating it with other parsers, and extend it. Require more than one success.
#[derive(Clone, Copy)]
pub struct SepExtend1<O, P1, P2> {
    pub(crate) init: O,
    pub(crate) p: P1,
    pub(crate) sep: P2,
}
impl<I: Input, C, S: Clone, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>, O: Extend<P1::Output>>
    ParserOnce<I, C, S, M> for SepExtend1<O, P1, P2>
{
    type Output = O;
    #[inline]
    fn run_once(mut self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.p.run(ICont { ok, config, drop }).and_then(|(o, ok)| {
            self.init.extend(once(o));
            run_left_sep_extend(self.init, self.p, self.sep, ok.to_cont(config, drop))
        })
    }
}
impl<I: Input, C, S: Clone, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>, O: Extend<P1::Output> + Clone>
    Parser<I, C, S, M> for SepExtend1<O, P1, P2>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.p.run(ICont { ok, config, drop }).and_then(|(o, ok)| {
            let mut init = self.init.clone();
            init.extend(once(o));
            run_left_sep_extend(init, self.p.to_ref(), self.sep.to_ref(), ok.to_cont(config, drop))
        })
    }
}

/// A recursive parser. It takes `T` and processes the continuation with `Err(T)` and the output with `Ok(U)`.
/// # Example
/// ```
/// use chasa::*;
/// let d = one_of("0123456789").and_then(|c: char| c.to_string().parse::<usize>().map_err(message));
/// let d = d.to_ref();
/// let p = char('(').right(tail_rec(0, move |n| d.map(move |m| Err(m+n)).or(char(')').to(Ok(n)))));
/// assert_eq!(p.to_ref().parse_ok("(12345)"), Some(15));
/// assert_eq!(p.to_ref().parse_ok("(12)345)"), Some(3));
/// assert_eq!(p.to_ref().parse_ok("()"), Some(0));
/// assert_eq!(p.to_ref().parse_ok("(12a345)"), None);
/// ```
#[derive(Clone, Copy)]
pub struct TailRec<O, F>(pub(crate) O, pub(crate) F);
#[inline]
pub fn tail_rec<I: Input, C, S, M: Cb, O1, O2, P: ParserOnce<I, C, S, M, Output = Result<O2, O1>>, F: Fn(O1) -> P>(
    init: O1, f: F,
) -> TailRec<O1, F> {
    TailRec(init, f)
}
impl<I: Input, C, S, M: Cb, O1, O2, P: ParserOnce<I, C, S, M, Output = Result<O2, O1>>, F: Fn(O1) -> P>
    ParserOnce<I, C, S, M> for TailRec<O1, F>
{
    type Output = O2;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O2, I, S, M> {
        run_tail_rec(self.0, self.1, cont)
    }
}
impl<I: Input, C, S, M: Cb, O1: Clone, O2, P: ParserOnce<I, C, S, M, Output = Result<O2, O1>>, F: Fn(O1) -> P>
    Parser<I, C, S, M> for TailRec<O1, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O2, I, S, M> {
        run_tail_rec(self.0.clone(), &self.1, cont)
    }
}
fn run_tail_rec<I: Input, C, S, M: Cb, O1, O2, P: ParserOnce<I, C, S, M, Output = Result<O2, O1>>, F: Fn(O1) -> P>(
    o: O1, f: F, cont: ICont<I, C, S, M>,
) -> IResult<O2, I, S, M> {
    let ICont { ok, config, drop } = cont;
    f(o).run_once(ICont { ok, config, drop }).and_then(|(o, ok)| match o {
        Err(o) => run_tail_rec(o, f, ok.to_cont(config, drop)),
        Ok(o) => Ok((o, ok)),
    })
}
