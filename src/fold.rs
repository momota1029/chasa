use std::{iter::once, marker::PhantomData};

use crate::{error::CustomBuilder as Cb, util::run_drop, ICont, IOk, IResult, Input, Parser, ParserOnce};

fn run_fold<O, I: Input, O2, C, S: Clone, M: Cb, P: Parser<I, O2, C, S, M>>(
    o: O, p: P, cont: ICont<I, C, S, M>, f: impl Fn(O, O2) -> O,
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
pub struct Fold<T, P, F, O>(T, P, F, PhantomData<fn() -> O>);
impl<T: Clone, P: Clone, F: Clone, O> Clone for Fold<T, P, F, O> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), self.2.clone(), PhantomData)
    }
}
impl<T: Copy, P: Copy, F: Copy, O> Copy for Fold<T, P, F, O> {}
#[inline]
pub fn fold<T, P, F, O>(init: T, parser: P, succ: F) -> Fold<T, P, F, O> {
    Fold(init, parser, succ, PhantomData)
}
impl<I: Input, O, C, S: Clone, M: Cb, T, P: Parser<I, O, C, S, M>, F: Fn(T, O) -> T> ParserOnce<I, T, C, S, M>
    for Fold<T, P, F, O>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<T, I, S, M> {
        run_fold(self.0, self.1, cont, self.2)
    }
}
impl<I: Input, O, C, S: Clone, M: Cb, T: Clone, P: Parser<I, O, C, S, M>, F: Fn(T, O) -> T> Parser<I, T, C, S, M>
    for Fold<T, P, F, O>
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
pub struct Fold1<T, P, F, O>(T, P, F, PhantomData<fn() -> O>);
impl<T: Clone, P: Clone, F: Clone, O> Clone for Fold1<T, P, F, O> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), self.2.clone(), PhantomData)
    }
}
impl<T: Copy, P: Copy, F: Copy, O> Copy for Fold1<T, P, F, O> {}
#[inline]
pub fn fold1<T, P, F, O>(init: T, parser: P, succ: F) -> Fold1<T, P, F, O> {
    Fold1(init, parser, succ, PhantomData)
}
impl<I: Input, O, C, S: Clone, M: Cb, T, P: Parser<I, O, C, S, M>, F: Fn(T, O) -> T> ParserOnce<I, T, C, S, M>
    for Fold1<T, P, F, O>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<T, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.1
            .run(ICont { ok, config, drop })
            .and_then(|(o, ok)| run_fold(self.2(self.0, o), self.1, ok.to_cont(config, drop), self.2))
    }
}
impl<I: Input, O, C, S: Clone, M: Cb, T: Clone, P: Parser<I, O, C, S, M>, F: Fn(T, O) -> T> Parser<I, T, C, S, M>
    for Fold1<T, P, F, O>
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
    O1,
    O2,
    C,
    S: Clone,
    M: Cb,
    T,
    P1: Parser<I, O1, C, S, M>,
    P2: Parser<I, O2, C, S, M>,
    F: Fn(T, O1) -> T,
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
/// let d = one_of("0123456789").and_then(|c:char| c.to_string().parse::<usize>().map_err(message));
/// let d = d.to_ref();
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).parse_ok("1,2,3,4,5"), Some(15));
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).parse_ok(""), Some(0));
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).and(char(',').right(d)).parse_ok("1,2,3,4,5,6"), None);
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).and(char(',')).parse_ok("1,2,3,4,"), Some((10, ',')));
/// ```
pub struct SepFold<T, P1, P2, F, O1, O2> {
    pub(crate) init: T,
    pub(crate) p: P1,
    pub(crate) sep: P2,
    pub(crate) succ: F,
    pub(crate) _marker: PhantomData<fn() -> (O1, O2)>,
}
impl<T: Clone, P1: Clone, P2: Clone, F: Clone, O1, O2> Clone for SepFold<T, P1, P2, F, O1, O2> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            init: self.init.clone(),
            p: self.p.clone(),
            sep: self.sep.clone(),
            succ: self.succ.clone(),
            _marker: PhantomData,
        }
    }
}
impl<T: Copy, P1: Copy, P2: Copy, F: Copy, O1, O2> Copy for SepFold<T, P1, P2, F, O1, O2> {}

impl<
        I: Input,
        O1,
        O2,
        C,
        S: Clone,
        M: Cb,
        T,
        P1: Parser<I, O1, C, S, M>,
        P2: Parser<I, O2, C, S, M>,
        F: Fn(T, O1) -> T,
    > ParserOnce<I, T, C, S, M> for SepFold<T, P1, P2, F, O1, O2>
{
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
        O1,
        O2,
        C,
        S: Clone,
        M: Cb,
        T: Clone,
        P1: Parser<I, O1, C, S, M>,
        P2: Parser<I, O2, C, S, M>,
        F: Fn(T, O1) -> T,
    > Parser<I, T, C, S, M> for SepFold<T, P1, P2, F, O1, O2>
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
    O1,
    O2,
    C,
    S: Clone,
    M: Cb,
    P1: Parser<I, O1, C, S, M>,
    P2: Parser<I, O2, C, S, M>,
    F: Fn(O1, O2, O1) -> O1,
>(
    t: O1, p: P1, sep: P2, succ: F, cont: ICont<I, C, S, M>,
) -> IResult<O1, I, S, M> {
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
pub struct SepFold1<P1, P2, F, O2> {
    pub(crate) p: P1,
    pub(crate) sep: P2,
    pub(crate) succ: F,
    pub(crate) _marker: PhantomData<fn() -> O2>,
}
impl<P1: Clone, P2: Clone, F: Clone, O2> Clone for SepFold1<P1, P2, F, O2> {
    #[inline]
    fn clone(&self) -> Self {
        Self { p: self.p.clone(), sep: self.sep.clone(), succ: self.succ.clone(), _marker: PhantomData }
    }
}
impl<P1: Copy, P2: Copy, F: Copy, O2> Copy for SepFold1<P1, P2, F, O2> {}
impl<
        I: Input,
        O1,
        O2,
        C,
        S: Clone,
        M: Cb,
        P1: Parser<I, O1, C, S, M>,
        P2: Parser<I, O2, C, S, M>,
        F: Fn(O1, O2, O1) -> O1,
    > ParserOnce<I, O1, C, S, M> for SepFold1<P1, P2, F, O2>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O1, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.p
            .run(ICont { ok, config, drop })
            .and_then(|(o, ok)| run_left_sep1(o, self.p, self.sep, self.succ, ok.to_cont(config, drop)))
    }
}
impl<
        I: Input,
        O1,
        O2,
        C,
        S: Clone,
        M: Cb,
        P1: Parser<I, O1, C, S, M>,
        P2: Parser<I, O2, C, S, M>,
        F: Fn(O1, O2, O1) -> O1,
    > Parser<I, O1, C, S, M> for SepFold1<P1, P2, F, O2>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O1, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.p.run(ICont { ok, config, drop }).and_then(|(o, ok)| {
            run_left_sep1(o, self.p.to_ref(), self.sep.to_ref(), &self.succ, ok.to_cont(config, drop))
        })
    }
}

fn run_extend<B: Extend<O>, I: Input, O, C, S: Clone, M: Cb, P: Parser<I, O, C, S, M>>(
    mut o: B, p: P, cont: ICont<I, C, S, M>,
) -> IResult<B, I, S, M> {
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
pub struct ExtendParser<B, P, O>(pub(crate) B, pub(crate) P, pub(crate) PhantomData<fn() -> O>);
impl<B: Clone, P: Clone, O> Clone for ExtendParser<B, P, O> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<B: Copy, P: Copy, O> Copy for ExtendParser<B, P, O> {}
impl<I: Input, O, C, S: Clone, M: Cb, P: Parser<I, O, C, S, M>, B: Extend<O>> ParserOnce<I, B, C, S, M>
    for ExtendParser<B, P, O>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<B, I, S, M> {
        run_extend(self.0, self.1, cont)
    }
}
impl<I: Input, O, C, S: Clone, M: Cb, P: Parser<I, O, C, S, M>, B: Extend<O> + Clone> Parser<I, B, C, S, M>
    for ExtendParser<B, P, O>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<B, I, S, M> {
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
pub struct Extend1Parser<B, P, O>(pub(crate) B, pub(crate) P, pub(crate) PhantomData<fn() -> O>);
impl<B: Clone, P: Clone, O> Clone for Extend1Parser<B, P, O> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<B: Copy, P: Copy, O> Copy for Extend1Parser<B, P, O> {}
impl<I: Input, O, C, S: Clone, M: Cb, P: Parser<I, O, C, S, M>, B: Extend<O>> ParserOnce<I, B, C, S, M>
    for Extend1Parser<B, P, O>
{
    #[inline]
    fn run_once(mut self, cont: ICont<I, C, S, M>) -> IResult<B, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.1.run(ICont { ok, config, drop }).and_then(|(o, ok)| {
            self.0.extend(once(o));
            run_extend(self.0, self.1, ok.to_cont(config, drop))
        })
    }
}
impl<I: Input, O, C, S: Clone, M: Cb, P: Parser<I, O, C, S, M>, B: Extend<O> + Clone> Parser<I, B, C, S, M>
    for Extend1Parser<B, P, O>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<B, I, S, M> {
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
    O1,
    O2,
    C,
    S: Clone,
    M: Cb,
    T: Extend<O1>,
    P1: Parser<I, O1, C, S, M>,
    P2: Parser<I, O2, C, S, M>,
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
pub struct SepExtend<B, P1, P2, O1, O2> {
    pub(crate) init: B,
    pub(crate) p: P1,
    pub(crate) sep: P2,
    pub(crate) _marker: PhantomData<fn() -> (O1, O2)>,
}
impl<B: Clone, P1: Clone, P2: Clone, O1, O2> Clone for SepExtend<B, P1, P2, O1, O2> {
    #[inline]
    fn clone(&self) -> Self {
        Self { init: self.init.clone(), p: self.p.clone(), sep: self.sep.clone(), _marker: PhantomData }
    }
}
impl<B: Copy, P1: Copy, P2: Copy, O1, O2> Copy for SepExtend<B, P1, P2, O1, O2> {}
impl<I: Input, O1, O2, C, S: Clone, M: Cb, P1: Parser<I, O1, C, S, M>, P2: Parser<I, O2, C, S, M>, B: Extend<O1>>
    ParserOnce<I, B, C, S, M> for SepExtend<B, P1, P2, O1, O2>
{
    #[inline]
    fn run_once(mut self, cont: ICont<I, C, S, M>) -> IResult<B, I, S, M> {
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
impl<
        I: Input,
        O1,
        O2,
        C,
        S: Clone,
        M: Cb,
        P1: Parser<I, O1, C, S, M>,
        P2: Parser<I, O2, C, S, M>,
        B: Extend<O1> + Clone,
    > Parser<I, B, C, S, M> for SepExtend<B, P1, P2, O1, O2>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<B, I, S, M> {
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
pub struct SepExtend1<B, P1, P2, O1, O2> {
    pub(crate) init: B,
    pub(crate) p: P1,
    pub(crate) sep: P2,
    pub(crate) _marker: PhantomData<fn() -> (O1, O2)>,
}
impl<B: Clone, P1: Clone, P2: Clone, O1, O2> Clone for SepExtend1<B, P1, P2, O1, O2> {
    #[inline]
    fn clone(&self) -> Self {
        Self { init: self.init.clone(), p: self.p.clone(), sep: self.sep.clone(), _marker: PhantomData }
    }
}
impl<B: Copy, P1: Copy, P2: Copy, O1, O2> Copy for SepExtend1<B, P1, P2, O1, O2> {}
impl<I: Input, O1, O2, C, S: Clone, M: Cb, P1: Parser<I, O1, C, S, M>, P2: Parser<I, O2, C, S, M>, B: Extend<O1>>
    ParserOnce<I, B, C, S, M> for SepExtend1<B, P1, P2, O1, O2>
{
    #[inline]
    fn run_once(mut self, cont: ICont<I, C, S, M>) -> IResult<B, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.p.run(ICont { ok, config, drop }).and_then(|(o, ok)| {
            self.init.extend(once(o));
            run_left_sep_extend(self.init, self.p, self.sep, ok.to_cont(config, drop))
        })
    }
}
impl<
        I: Input,
        O1,
        O2,
        C,
        S: Clone,
        M: Cb,
        P1: Parser<I, O1, C, S, M>,
        P2: Parser<I, O2, C, S, M>,
        B: Extend<O1> + Clone,
    > Parser<I, B, C, S, M> for SepExtend1<B, P1, P2, O1, O2>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<B, I, S, M> {
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
pub fn tail_rec<I: Input, C, S, M: Cb, O1, O2, P: ParserOnce<I, Result<O2, O1>, C, S, M>, F: Fn(O1) -> P>(
    init: O1, f: F,
) -> TailRec<O1, F> {
    TailRec(init, f)
}
impl<I: Input, C, S, M: Cb, O1, O2, P: ParserOnce<I, Result<O2, O1>, C, S, M>, F: Fn(O1) -> P>
    ParserOnce<I, O2, C, S, M> for TailRec<O1, F>
{
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O2, I, S, M> {
        run_tail_rec(self.0, self.1, cont)
    }
}
impl<I: Input, C, S, M: Cb, O1: Clone, O2, P: ParserOnce<I, Result<O2, O1>, C, S, M>, F: Fn(O1) -> P>
    Parser<I, O2, C, S, M> for TailRec<O1, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O2, I, S, M> {
        run_tail_rec(self.0.clone(), &self.1, cont)
    }
}
fn run_tail_rec<I: Input, C, S, M: Cb, O1, O2, P: ParserOnce<I, Result<O2, O1>, C, S, M>, F: Fn(O1) -> P>(
    o: O1, f: F, cont: ICont<I, C, S, M>,
) -> IResult<O2, I, S, M> {
    let ICont { ok, config, drop } = cont;
    f(o).run_once(ICont { ok, config, drop }).and_then(|(o, ok)| match o {
        Err(o) => run_tail_rec(o, f, ok.to_cont(config, drop)),
        Ok(o) => Ok((o, ok)),
    })
}
