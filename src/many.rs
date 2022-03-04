use std::{
    iter::once,
    marker::PhantomData,
    ops::{Bound, RangeBounds},
};

use crate::{
    error::{Builder as Eb, CustomBuilder as Cb},
    util::{run_drop, RangeWithOrd},
    ICont, IOk, IResult, Input, LazyError, Parser, ParserOnce,
};

/// Collects successive runs of the parser as if they were iterators. However, a failure that consumes input will cause the whole thing to fail.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(any.many().parse_ok("1234"), Some(vec!['1','2','3','4']));
/// assert_eq!(any.many().parse_ok("1234"), Some("1234".to_string()));
/// assert_eq!(char('a').many().parse_ok("aaabaaba"), Some("aaa".to_string()));
/// assert_eq!(char('a').many().parse_ok(""), Some("".to_string()));
/// ```
pub struct Many<P, O>(P, PhantomData<fn() -> O>);
impl<P: Clone, O> Clone for Many<P, O> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<P: Copy, O> Copy for Many<P, O> {}
#[inline]
pub fn many<O, P>(parser: P) -> Many<P, O> {
    Many(parser, PhantomData)
}
impl<I: Input, C, S: Clone, M: Cb, O: FromIterator<P::Output>, P: Parser<I, C, S, M>> ParserOnce<I, C, S, M>
    for Many<P, O>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.run(cont)
    }
}
impl<I: Input, C, S: Clone, M: Cb, O: FromIterator<P::Output>, P: Parser<I, C, S, M>> Parser<I, C, S, M>
    for Many<P, O>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let mut ret = Some(Ok(cont));
        let iter = ParserIterator { parser: &self.0, ret: &mut ret };
        let o = iter.collect::<O>();
        match ret.unwrap() {
            Ok(cont) => Ok((o, cont.ok)),
            Err(e) => Err(e),
        }
    }
}
/// Collects successive runs of the parser as if they were iterators. However, if more than one input is consumed and fails, or none of them succeeds, the whole thing will fail.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(any.many1::<Vec<_>>().parse_ok("1234"), Some(vec!['1','2','3','4']));
/// assert_eq!(any.many1::<String>().parse_ok("1234"), Some("1234".to_string()));
/// assert_eq!(char('a').many1::<String>().parse_ok("aaabaaba"), Some("aaa".to_string()));
/// assert_eq!(char('a').many1::<String>().parse_ok(""), None);
/// ```
pub struct Many1<P, O>(P, PhantomData<fn() -> O>);
impl<P: Clone, O> Clone for Many1<P, O> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<P: Copy, O> Copy for Many1<P, O> {}
#[inline]
pub fn many1<O, P>(parser: P) -> Many1<P, O> {
    Many1(parser, PhantomData)
}
impl<I: Input, C, S: Clone, M: Cb, O: FromIterator<P::Output>, P: Parser<I, C, S, M>> ParserOnce<I, C, S, M>
    for Many1<P, O>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.run(cont)
    }
}
impl<I: Input, C, S: Clone, M: Cb, O: FromIterator<P::Output>, P: Parser<I, C, S, M>> Parser<I, C, S, M>
    for Many1<P, O>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0.run(ICont { ok, config, drop }).and_then(|(o, ok)| {
            let mut ret = Some(Ok(ICont { ok, config, drop }));
            let iter = ParserIterator { parser: &self.0, ret: &mut ret };
            let o = once(o).chain(iter).collect::<O>();
            match ret.unwrap() {
                Ok(cont) => Ok((o, cont.ok)),
                Err(e) => Err(e),
            }
        })
    }
}

/// It iterates through a sequence of parses, or does not parse if dropped.
/// A failure that does not consume input will cause the iterator to terminate, a failure that consumes it will cause the whole parser to fail.
/// The required [`ParserIterator<P>`] argument to `F` is simply an iterator that returns `P::Output`.
/// # Example
/// ```
/// use chasa::*;
/// assert_eq!(
///     any.many_with(|iter| iter.enumerate().collect()).parse_ok("abcde"),
///     Some(vec![(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e')])
/// );
/// assert_eq!(
///     any.many_with(|iter| iter.take(2).collect())
///         .and(char('c'))
///     .parse_ok("abcde"), Some(("ab".to_string(), 'c'))
/// );
/// assert_eq!(
///     any.and(char('a')).many_with(|iter| iter.collect()).parse_ok("baca"),
///     Some(vec![('b','a'),('c','a')])
/// );
/// assert_eq!(
///     any.and(char('a')).many_with(|iter| iter.collect::<Vec<_>>()).parse_ok("bacahh"),
///     None
/// );
/// ```
#[derive(Clone, Copy)]
pub struct ManyWith<P, F>(pub(crate) P, pub(crate) F);
impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>, F: FnOnce(ParserIterator<P, I, C, S, M>) -> O, O>
    ParserOnce<I, C, S, M> for ManyWith<P, F>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let mut ret = Some(Ok(cont));
        let iter = ParserIterator { parser: &self.0, ret: &mut ret };
        let o = self.1(iter);
        match ret.unwrap() {
            Ok(cont) => Ok((o, cont.ok)),
            Err(e) => Err(e),
        }
    }
}
impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>, F: Fn(ParserIterator<P, I, C, S, M>) -> O, O>
    Parser<I, C, S, M> for ManyWith<P, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let mut ret = Some(Ok(cont));
        let iter = ParserIterator { parser: &self.0, ret: &mut ret };
        let o = self.1(iter);
        match ret.unwrap() {
            Ok(cont) => Ok((o, cont.ok)),
            Err(e) => Err(e),
        }
    }
}

/// Repeat a series of parses and do not parse if dropped.
/// Failures that do not consume input terminate the iterator, while failures that consume input cause the entire parser to fail.
/// A function can return an error::Builder<M> as its return value, which also causes the entire parser to fail.
/// The mandatory [`ParserIterator<P>`] argument to `F` is simply an iterator returning `P::Output`.
#[derive(Clone, Copy)]
pub struct ManyThen<P, F>(pub(crate) P, pub(crate) F);
impl<
        I: Input,
        C,
        S: Clone,
        M: Cb,
        P: Parser<I, C, S, M>,
        F: FnOnce(ParserIterator<P, I, C, S, M>) -> Result<O, Eb<M>>,
        O,
    > ParserOnce<I, C, S, M> for ManyThen<P, F>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let pos = cont.ok.input.pos();
        let mut ret = Some(Ok(cont));
        let iter = ParserIterator { parser: &self.0, ret: &mut ret };
        let o = self.1(iter);
        match ret.unwrap() {
            Ok(cont) => match o {
                Ok(o) => Ok((o, cont.ok)),
                Err(e) => Err(e.at::<I>(cont.ok.input.index(), pos, Some(cont.ok.input.pos())).or_merge(cont.ok.err)),
            },
            Err(e) => Err(e),
        }
    }
}
impl<
        I: Input,
        C,
        S: Clone,
        M: Cb,
        P: Parser<I, C, S, M>,
        F: Fn(ParserIterator<P, I, C, S, M>) -> Result<O, Eb<M>>,
        O,
    > Parser<I, C, S, M> for ManyThen<P, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let pos = cont.ok.input.pos();
        let mut ret = Some(Ok(cont));
        let iter = ParserIterator { parser: &self.0, ret: &mut ret };
        let o = self.1(iter);
        match ret.unwrap() {
            Ok(cont) => match o {
                Ok(o) => Ok((o, cont.ok)),
                Err(e) => Err(e.at::<I>(cont.ok.input.index(), pos, Some(cont.ok.input.pos())).or_merge(cont.ok.err)),
            },
            Err(e) => Err(e),
        }
    }
}

/// an iterator that returns `P::Output`. It can only be used locally within [`ManyWith`].
pub struct ParserIterator<'a, 'b, P: Parser<I, C, S, M>, I: Input, C, S, M: Cb> {
    parser: &'b P,
    ret: &'b mut Option<Result<ICont<'a, I, C, S, M>, LazyError<I, M>>>,
}
impl<'a, 'b, I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>> Iterator for ParserIterator<'a, 'b, P, I, C, S, M> {
    type Item = P::Output;
    #[inline]
    fn next(&mut self) -> Option<P::Output> {
        match self.ret.take()? {
            Err(_) => None,
            Ok(ICont { ok, config, drop }) => {
                let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
                if cutted {
                    drop()
                }
                match run_drop(self.parser.to_ref(), ICont { ok, config, drop }, (input, state)) {
                    (Ok((o, ok)), _) => {
                        *self.ret = Some(Ok(ok.to_cont(config, drop)));
                        Some(o)
                    },
                    (Err(e), None) => {
                        *self.ret = Some(Err(e));
                        None
                    },
                    (Err(e), Some((input, state))) => {
                        *self.ret = Some(Ok(IOk { input, state, err: Some(e), cutted }.to_cont(config, drop)));
                        None
                    },
                }
            },
        }
    }
}

/// Arrange several parsers in a row and separate them with another parser. The result is a [`std::iter::Iterator::collect`].
/// # Example
/// ```
/// use chasa::*;
/// let d = one_of('0'..='9').and_then(|c: char| c.to_string().parse::<isize>().map_err(message));
/// let p = d.to_ref().sep(char(','));
/// assert_eq!(p.parse_ok("1,2,3,4,5"), Some(vec![1,2,3,4,5]));
/// assert_eq!(p.parse_ok(""), Some(vec![]));
/// assert_eq!(p.and(char(',').right(d.to_ref())).parse_ok("1,2,3,4,5,6"), None);
/// assert_eq!(p.and(char(',')).parse_ok("1,2,3,4,"), Some((vec![1,2,3,4], ',')));
/// ```
pub struct Sep<P1, P2, O>(pub(crate) P1, pub(crate) P2, pub(crate) PhantomData<fn() -> O>);
impl<P1: Clone, P2: Clone, O> Clone for Sep<P1, P2, O> {
    #[inline]
    fn clone(&self) -> Self {
        Sep(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P1: Copy, P2: Copy, O> Copy for Sep<P1, P2, O> {}
impl<O: FromIterator<P1::Output>, I: Input, C, S: Clone, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>
    ParserOnce<I, C, S, M> for Sep<P1, P2, O>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.run(cont)
    }
}
impl<O: FromIterator<P1::Output>, I: Input, C, S: Clone, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>
    Parser<I, C, S, M> for Sep<P1, P2, O>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let mut ret = Some(Ok(cont));
        let iter = ParserSepIterator { parser: &self.0, sep: &self.1, is_first: true, ret: &mut ret };
        let o = iter.collect();
        match ret.unwrap() {
            Ok(cont) => Ok((o, cont.ok)),
            Err(e) => Err(e),
        }
    }
}

/// Arrange several parsers in a row and separate them with another parser. The result is a [`std::iter::Iterator::collect`]. More than one success is requested.
/// # Example
/// ```
/// use chasa::*;
/// let d = one_of('0'..='9').and_then(|c: char| c.to_string().parse::<isize>().map_err(message));
/// let p = d.to_ref().sep1(char(','));
/// assert_eq!(p.parse_ok("1,2,3,4,5"), Some(vec![1,2,3,4,5]));
/// assert_eq!(p.parse_ok(""), None);
/// assert_eq!(p.and(char(',').right(d.to_ref())).parse_ok("1,2,3,4,5,6"), None);
/// assert_eq!(p.and(char(',')).parse_ok("1,2,3,4,"), Some((vec![1,2,3,4], ',')));
/// ```
pub struct Sep1<P1, P2, O>(pub(crate) P1, pub(crate) P2, pub(crate) PhantomData<fn() -> O>);
impl<P1: Clone, P2: Clone, O> Clone for Sep1<P1, P2, O> {
    #[inline]
    fn clone(&self) -> Self {
        Sep1(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P1: Copy, P2: Copy, O> Copy for Sep1<P1, P2, O> {}
impl<O: FromIterator<P1::Output>, I: Input, C, S: Clone, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>
    ParserOnce<I, C, S, M> for Sep1<P1, P2, O>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.run(cont)
    }
}
impl<O: FromIterator<P1::Output>, I: Input, C, S: Clone, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>
    Parser<I, C, S, M> for Sep1<P1, P2, O>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let ICont { ok, config, drop } = cont;
        self.0.run(ICont { ok, config, drop }).and_then(|(o, ok)| {
            let mut ret = Some(Ok(ICont { ok, config, drop }));
            let iter = ParserSepIterator { parser: &self.0, sep: &self.1, is_first: false, ret: &mut ret };
            let o = once(o).chain(iter).collect();
            match ret.unwrap() {
                Ok(cont) => Ok((o, cont.ok)),
                Err(e) => Err(e),
            }
        })
    }
}

/// Manipulate the iterator to repeat the parse while breathing on another parser, and get the result. If it is dropped, no parsing is done.
///
/// A failure that does not consume input will terminate the iterator, a failure that consumes input will cause the entire iterator to fail.
///
/// The [`ParserSepIterator<P1,P2>`] argument, required for `F`, is simply an iterator that returns `P1:Output`.
///
/// See also [`ManyWith`] and [`Sep`]
/// # Example
/// ```
/// use chasa::*;
/// let d = one_of('0'..='9').and_then(|c: char| c.to_string().parse::<isize>().map_err(message));
/// let d = d.to_ref();
/// assert_eq!(
///     d.sep_with(char(','), |iter| iter.take(2).collect())
///         .and(str(",3,4,5").to(true))
///         .parse_ok("1,2,3,4,5"),
///     Some((vec![1,2], true))
/// );
/// assert_eq!(
///     d.sep_with(char(','), |iter| iter.collect())
///         .parse_ok("1,2,3,4,"),
///     Some(vec![1,2,3,4])
/// );
/// ```
#[derive(Clone, Copy)]
pub struct SepWith<P1, P2, F>(pub(crate) P1, pub(crate) P2, pub(crate) F);
impl<
        I: Input,
        C,
        S: Clone,
        M: Cb,
        P1: Parser<I, C, S, M>,
        P2: Parser<I, C, S, M>,
        F: FnOnce(ParserSepIterator<P1, P2, I, C, S, M>) -> O,
        O,
    > ParserOnce<I, C, S, M> for SepWith<P1, P2, F>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let mut ret = Some(Ok(cont));
        let iter = ParserSepIterator { parser: &self.0, sep: &self.1, is_first: true, ret: &mut ret };
        let o = self.2(iter);
        match ret.unwrap() {
            Ok(cont) => Ok((o, cont.ok)),
            Err(e) => Err(e),
        }
    }
}
impl<
        I: Input,
        C,
        S: Clone,
        M: Cb,
        P1: Parser<I, C, S, M>,
        P2: Parser<I, C, S, M>,
        F: Fn(ParserSepIterator<P1, P2, I, C, S, M>) -> O,
        O,
    > Parser<I, C, S, M> for SepWith<P1, P2, F>
{
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let mut ret = Some(Ok(cont));
        let iter = ParserSepIterator { parser: &self.0, sep: &self.1, is_first: true, ret: &mut ret };
        let o = self.2(iter);
        match ret.unwrap() {
            Ok(cont) => Ok((o, cont.ok)),
            Err(e) => Err(e),
        }
    }
}

#[derive(Clone, Copy)]
pub struct SepThen<P1, P2, F>(pub(crate) P1, pub(crate) P2, pub(crate) F);
impl<
        I: Input,
        C,
        S: Clone,
        M: Cb,
        P1: Parser<I, C, S, M>,
        P2: Parser<I, C, S, M>,
        F: FnOnce(ParserSepIterator<P1, P2, I, C, S, M>) -> Result<O, Eb<M>>,
        O,
    > ParserOnce<I, C, S, M> for SepThen<P1, P2, F>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let pos = cont.ok.input.pos();
        let mut ret = Some(Ok(cont));
        let iter = ParserSepIterator { parser: &self.0, sep: &self.1, is_first: true, ret: &mut ret };
        let o = self.2(iter);
        match ret.unwrap() {
            Ok(cont) => match o {
                Ok(o) => Ok((o, cont.ok)),
                Err(e) => Err(e.at::<I>(cont.ok.input.index(), pos, Some(cont.ok.input.pos())).or_merge(cont.ok.err)),
            },
            Err(e) => Err(e),
        }
    }
}
impl<
        I: Input,
        C,
        S: Clone,
        M: Cb,
        P1: Parser<I, C, S, M>,
        P2: Parser<I, C, S, M>,
        F: Fn(ParserSepIterator<P1, P2, I, C, S, M>) -> Result<O, Eb<M>>,
        O,
    > Parser<I, C, S, M> for SepThen<P1, P2, F>
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        let pos = cont.ok.input.pos();
        let mut ret = Some(Ok(cont));
        let iter = ParserSepIterator { parser: &self.0, sep: &self.1, is_first: true, ret: &mut ret };
        let o = self.2(iter);
        match ret.unwrap() {
            Ok(cont) => match o {
                Ok(o) => Ok((o, cont.ok)),
                Err(e) => Err(e.at::<I>(cont.ok.input.index(), pos, Some(cont.ok.input.pos())).or_merge(cont.ok.err)),
            },
            Err(e) => Err(e),
        }
    }
}

/// an iterator that returns `P1::Output`. It can only be used locally within [`SepWith`].
pub struct ParserSepIterator<'a, 'b, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>, I: Input, C, S, M: Cb> {
    parser: &'b P1,
    sep: &'b P2,
    is_first: bool,
    ret: &'b mut Option<Result<ICont<'a, I, C, S, M>, LazyError<I, M>>>,
}
impl<'a, 'b, I: Input, C, S: Clone, M: Cb, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>> Iterator
    for ParserSepIterator<'a, 'b, P1, P2, I, C, S, M>
{
    type Item = P1::Output;
    #[inline]
    fn next(&mut self) -> Option<P1::Output> {
        let ICont { ok, config, drop } = match self.ret.take()? {
            Ok(cont) => cont,
            Err(_) => None?,
        };
        let (input, state) = (ok.input.clone(), ok.state.clone());
        let ok = if self.is_first {
            self.is_first = false;
            ok
        } else {
            let cutted = ok.cutted;
            if cutted {
                drop()
            }
            match run_drop(self.sep.to_ref(), ICont { ok, config, drop }, ()) {
                (Err(e), None) => {
                    *self.ret = Some(Err(e));
                    None?
                },
                (Err(e), Some(())) => {
                    *self.ret = Some(Ok(IOk { input, state, err: Some(e), cutted }.to_cont(config, drop)));
                    return None;
                },
                (Ok((_, ok)), _) => ok,
            }
        };
        let cutted = ok.cutted;
        if cutted {
            drop()
        }
        match run_drop(self.parser.to_ref(), ICont { ok, config, drop }, (input, state)) {
            (Ok((o, ok)), _) => {
                *self.ret = Some(Ok(ok.to_cont(config, drop)));
                Some(o)
            },
            (Err(e), None) => {
                *self.ret = Some(Err(e));
                None
            },
            (Err(e), Some((input, state))) => {
                *self.ret = Some(Ok(IOk { input, state, err: Some(e), cutted }.to_cont(config, drop)));
                None
            },
        }
    }
}

/// Repeat a specified number of times, either range or usize.
/// ```
/// use chasa::*;
/// assert_eq!(char('a').repeat(2).parse_ok("aaa"), Some("aa".to_string()));
/// assert_eq!(char('a').repeat(4).parse_ok("aaaaa"), Some("aaaa".to_string()));
/// assert_eq!(char('a').repeat(1..=3).parse_ok("aaaaa"), Some("aaa".to_string()));
/// ```
pub struct Repeat<P, O> {
    parser: P,
    start: usize,
    end: Option<usize>,
    _marker: PhantomData<fn() -> O>,
}
#[inline]
pub fn repeat<P, N: RangeWithOrd<usize>, O>(parser: P, count: N) -> Repeat<P, O> {
    let range = count.to_pair();
    let start = match range.start_bound() {
        Bound::Included(&start) => start,
        Bound::Excluded(&start) => start + 1,
        Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        Bound::Included(&end) => Some(end),
        Bound::Excluded(&end) => Some(end - 1),
        Bound::Unbounded => None,
    };
    Repeat { parser, start, end, _marker: PhantomData }
}
impl<P: Clone, O> Clone for Repeat<P, O> {
    #[inline]
    fn clone(&self) -> Self {
        Self { parser: self.parser.clone(), start: self.start, end: self.end, _marker: PhantomData }
    }
}
impl<P: Copy, O> Copy for Repeat<P, O> {}

impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>, O: FromIterator<P::Output>> Parser<I, C, S, M>
    for Repeat<P, O>
where
    Self: ParserOnce<I, C, S, M, Output = O> + Clone,
{
    #[inline]
    fn run(&self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        self.clone().run_once(cont)
    }
}
impl<I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>, O: FromIterator<P::Output>> ParserOnce<I, C, S, M>
    for Repeat<P, O>
{
    type Output = O;
    #[inline]
    fn run_once(self, cont: ICont<I, C, S, M>) -> IResult<O, I, S, M> {
        if self.end == Some(0) {
            return Ok((std::iter::empty().collect(), cont.ok));
        }
        let mut ret = Some(Ok(cont));
        let o = RepeatIter { p: &self.parser, i: 0, start: self.start, end: self.end, ret: &mut ret }.collect::<O>();
        match ret.unwrap() {
            Ok(cont) => Ok((o, cont.ok)),
            Err(e) => Err(e),
        }
    }
}
pub struct RepeatIter<'a, 'b, I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>> {
    p: &'a P,
    i: usize,
    ret: &'a mut Option<Result<ICont<'b, I, C, S, M>, LazyError<I, M>>>,
    start: usize,
    end: Option<usize>,
}
impl<'a, 'b, I: Input, C, S: Clone, M: Cb, P: Parser<I, C, S, M>> Iterator for RepeatIter<'a, 'b, I, C, S, M, P> {
    type Item = P::Output;
    #[inline]
    fn next(&mut self) -> Option<P::Output> {
        if let Some(end) = self.end {
            if self.i >= end {
                return None;
            }
        }
        match self.ret.take()? {
            Err(_) => None,
            Ok(ICont { ok, config, drop }) => {
                let (input, state, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
                if cutted {
                    drop()
                }
                match run_drop(self.p.to_ref(), ICont { ok, config, drop }, (input, state)) {
                    (Ok((o, ok)), _) => {
                        self.i += 1;
                        *self.ret = Some(Ok(ok.to_cont(config, drop)));
                        Some(o)
                    },
                    (Err(e), None) => {
                        *self.ret = Some(Err(e));
                        None
                    },
                    (Err(e), Some((input, state))) => {
                        *self.ret = Some({
                            if self.start > self.i {
                                Err(e)
                            } else {
                                Ok(IOk { input, state, err: Some(e), cutted }.to_cont(config, drop))
                            }
                        });
                        None
                    },
                }
            },
        }
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.start, self.end)
    }
}
