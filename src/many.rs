use std::{
    iter,
    marker::PhantomData,
    ops::{Bound, RangeBounds},
};

use super::{
    error::ParseError,
    fold,
    input::Input,
    parser::{Args, Parser, ParserOnce},
    util::{Consume, RangeWithOrd},
};

pub struct ManyIterator<'a, 'b, P: Parser<I, O, E, C, S>, I: Input, O, E: ParseError<I>, C, S: Clone> {
    parser: &'a mut P,
    args: Args<'a, 'b, I, E, C, S>,
    failed: &'a mut bool,
    _marker: PhantomData<fn() -> O>,
}
impl<'a, 'b, P: Parser<I, O, E, C, S>, I: Input, O, E: ParseError<I>, C, S: Clone> Iterator
    for ManyIterator<'a, 'b, P, I, O, E, C, S>
{
    type Item = O;
    #[inline(always)]
    fn next(&mut self) -> Option<O> {
        match self.parser.by_ref().or_not().run(self.args.by_ref()) {
            None => {
                *self.failed = true;
                None
            },
            Some(o) => o,
        }
    }
}

/// Collects successive runs of the parser as if they were iterators. However, a failure that consumes input will cause the whole thing to fail.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(any.many().parse_ok("1234"), Some(vec!['1','2','3','4']));
/// assert_eq!(any.many().parse_ok("1234"), Some("1234".to_string()));
/// assert_eq!(char('a').many().parse_ok("aaabaaba"), Some("aaa".to_string()));
/// assert_eq!(char('a').many().parse_ok(""), Some("".to_string()));
/// ```
pub struct Many<P, O, T>(P, PhantomData<fn() -> (O, T)>);
impl<P: Clone, O, T> Clone for Many<P, O, T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<P: Copy, O, T> Copy for Many<P, O, T> {}
#[inline(always)]
pub fn many<O, T, I: Input, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>>(parser: P) -> Many<P, O, T> {
    Many(parser, PhantomData)
}
impl<I: Input, O: FromIterator<T>, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>> ParserOnce<I, O, E, C, S>
    for Many<P, O, T>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.run(args)
    }
}
impl<I: Input, O: FromIterator<T>, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>> Parser<I, O, E, C, S>
    for Many<P, O, T>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let mut failed = false;
        let o = ManyIterator { parser: &mut self.0, args, failed: &mut failed, _marker: PhantomData }.collect();
        if failed {
            None
        } else {
            Some(o)
        }
    }
}

/// Collects successive runs of the parser as if they were iterators. However, if more than one input is consumed and fails, or none of them succeeds, the whole thing will fail.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(any.many1::<Vec<_>>().parse_ok("1234"), Some(vec!['1','2','3','4']));
/// assert_eq!(any.many1::<String>().parse_ok("1234"), Some("1234".to_string()));
/// assert_eq!(char('a').many1::<String>().parse_ok("aaabaaba"), Some("aaa".to_string()));
/// assert_eq!(char('a').many1::<String>().parse_ok(""), None);
/// ```

pub struct Many1<P, O, T>(P, PhantomData<fn() -> (O, T)>);
impl<P: Clone, O, T> Clone for Many1<P, O, T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<P: Copy, O, T> Copy for Many1<P, O, T> {}
#[inline(always)]
pub fn many1<O, T, I: Input, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>>(parser: P) -> Many1<P, O, T> {
    Many1(parser, PhantomData)
}
impl<I: Input, O: FromIterator<T>, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>> ParserOnce<I, O, E, C, S>
    for Many1<P, O, T>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.run(args)
    }
}
impl<I: Input, O: FromIterator<T>, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>> Parser<I, O, E, C, S>
    for Many1<P, O, T>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O> {
        let first = self.0.run(args.by_ref())?;
        let mut failed = false;
        let o = iter::once(first)
            .chain(ManyIterator { parser: &mut self.0, args, failed: &mut failed, _marker: PhantomData })
            .collect();
        if failed {
            None
        } else {
            Some(o)
        }
    }
}

/// It iterates through a sequence of parses, or does not parse if dropped.
/// A failure that does not consume input will cause the iterator to terminate, a failure that consumes it will cause the whole parser to fail.
/// The required [`ManyIterator<P>`] argument to `F` is simply an iterator that returns `P::Output`.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(
///     any.many_map(|iter| iter.enumerate().collect()).parse_ok("abcde"),
///     Some(vec![(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e')])
/// );
/// assert_eq!(
///     any.many_map(|iter| iter.take(2).collect())
///         .and(char('c'))
///     .parse_ok("abcde"), Some(("ab".to_string(), 'c'))
/// );
/// assert_eq!(
///     any.and(char('a')).many_map(|iter| iter.collect()).parse_ok("baca"),
///     Some(vec![('b','a'),('c','a')])
/// );
/// assert_eq!(
///     any.and(char('a')).many_map(|iter| iter.collect::<Vec<_>>()).parse_ok("bacahh"),
///     None
/// );
/// ```
pub struct ManyMap<P, F, T>(pub(crate) P, pub(crate) F, pub(crate) PhantomData<fn() -> T>);
impl<P: Clone, F: Clone, T> Clone for ManyMap<P, F, T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P: Copy, F: Copy, T> Copy for ManyMap<P, F, T> {}
impl<
        I: Input,
        O,
        T,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        F: FnOnce(ManyIterator<P, I, T, E, C, S>) -> O,
    > ParserOnce<I, O, E, C, S> for ManyMap<P, F, T>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        let mut failed = false;
        let o = self.1(ManyIterator { parser: &mut self.0, args, failed: &mut failed, _marker: PhantomData });
        if failed {
            None
        } else {
            Some(o)
        }
    }
}
impl<
        I: Input,
        O,
        T,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        F: FnMut(ManyIterator<P, I, T, E, C, S>) -> O,
    > Parser<I, O, E, C, S> for ManyMap<P, F, T>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let mut failed = false;
        let o = self.1(ManyIterator { parser: &mut self.0, args, failed: &mut failed, _marker: PhantomData });
        if failed {
            None
        } else {
            Some(o)
        }
    }
}

pub struct ManyBind<P, F, O1>(pub(crate) P, pub(crate) F, pub(crate) PhantomData<fn() -> O1>);
impl<P: Clone, F: Clone, O1> Clone for ManyBind<P, F, O1> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P: Copy, F: Copy, O1> Copy for ManyBind<P, F, O1> {}
impl<
        I: Input,
        O1,
        O2,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, O1, E, C, S>,
        Q: ParserOnce<I, O2, E, C, S>,
        F: FnOnce(ManyIterator<P, I, O1, E, C, S>) -> Q,
    > ParserOnce<I, O2, E, C, S> for ManyBind<P, F, O1>
{
    #[inline(always)]
    fn run_once(mut self, mut args: Args<I, E, C, S>) -> Option<O2> {
        let mut failed = false;
        let q = self.1(ManyIterator {
            parser: &mut self.0,
            args: args.by_ref(),
            failed: &mut failed,
            _marker: PhantomData,
        });
        if failed {
            None
        } else {
            q.run_once(args)
        }
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
        Q: ParserOnce<I, O2, E, C, S>,
        F: FnMut(ManyIterator<P, I, O1, E, C, S>) -> Q,
    > Parser<I, O2, E, C, S> for ManyBind<P, F, O1>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O2> {
        let mut failed = false;
        let q = self.1(ManyIterator {
            parser: &mut self.0,
            args: args.by_ref(),
            failed: &mut failed,
            _marker: PhantomData,
        });
        if failed {
            None
        } else {
            q.run_once(args)
        }
    }
}

/// an iterator that returns `P::Output`. It can only be used locally within [`SepMap`].
pub struct SepIterator<
    'a,
    'b,
    P: Parser<I, O, E, C, S>,
    Q: Parser<I, T, E, C, S>,
    I: Input,
    O,
    T,
    E: ParseError<I>,
    C,
    S: Clone,
> {
    item_parser: &'a mut P,
    sep_parser: &'a mut Q,
    is_first: bool,
    failed: &'a mut bool,

    // args
    input: &'a mut I,
    config: &'a C,
    state: &'a mut S,
    consume: &'a mut Consume<'b, (I, S)>,
    error: &'a mut E,
    _marker: PhantomData<fn() -> (O, T)>,
}
impl<'a, 'b, P: Parser<I, O, E, C, S>, Q: Parser<I, T, E, C, S>, I: Input, O, T, E: ParseError<I>, C, S: Clone> Iterator
    for SepIterator<'a, 'b, P, Q, I, O, T, E, C, S>
{
    type Item = O;
    #[inline(always)]
    fn next(&mut self) -> Option<O> {
        if self.is_first {
            self.is_first = false;
            match self.item_parser.by_ref().or_not().run(Args {
                input: self.input,
                config: self.config,
                state: self.state,
                consume: self.consume,
                error: self.error,
            }) {
                None => {
                    *self.failed = true;
                    None
                },
                Some(o) => o,
            }
        } else {
            let (bak_input, bak_state) = (self.input.clone(), self.state.clone());
            match self.consume.cons((bak_input, bak_state), |consume| {
                fold::run_sep_second_part(
                    self.item_parser,
                    self.sep_parser,
                    self.input,
                    self.config,
                    self.state,
                    consume,
                    self.error,
                )
            }) {
                (Some((_, item)), _) => Some(item),
                (None, Some((bak_input, bak_state))) => {
                    *self.input = bak_input;
                    *self.state = bak_state;
                    None
                },
                (None, None) => {
                    *self.failed = true;
                    None
                },
            }
        }
    }
}

/// Arrange several parsers in a row and separate them with another parser. The result is a [`std::iter::Iterator::collect`].
/// # Example
/// ```
/// use chasa::prelude::*;
/// let d = one_of('0'..='9').and_then(|c: char| c.to_string().parse::<isize>().map_err(from_error));
/// let p = d.sep(char(','));
/// assert_eq!(p.parse_ok("1,2,3,4,5"), Some(vec![1,2,3,4,5]));
/// assert_eq!(p.parse_ok(""), Some(vec![]));
/// assert_eq!(p.and(char(',').right(d)).parse_ok("1,2,3,4,5,6"), None);
/// assert_eq!(p.and(char(',')).parse_ok("1,2,3,4,"), Some((vec![1,2,3,4], ',')));
/// ```
pub struct Sep<P, Q, O, T, U>(pub(crate) P, pub(crate) Q, pub(crate) PhantomData<fn() -> (O, T, U)>);
impl<P: Clone, Q: Clone, O, T, U> Clone for Sep<P, Q, O, T, U> {
    #[inline]
    fn clone(&self) -> Self {
        Sep(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P: Copy, Q: Copy, O, T, U> Copy for Sep<P, Q, O, T, U> {}
impl<
        I: Input,
        O: FromIterator<T>,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
    > ParserOnce<I, O, E, C, S> for Sep<P, Q, O, T, U>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.run(args)
    }
}
impl<
        I: Input,
        O: FromIterator<T>,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
    > Parser<I, O, E, C, S> for Sep<P, Q, O, T, U>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        let mut failed = false;
        let o = SepIterator {
            item_parser: &mut self.0,
            sep_parser: &mut self.1,
            is_first: true,
            failed: &mut failed,
            input,
            config,
            state,
            consume,
            error,
            _marker: PhantomData,
        }
        .collect();
        if failed {
            None
        } else {
            Some(o)
        }
    }
}

/// Arrange several parsers in a row and separate them with another parser. The result is a [`std::iter::Iterator::collect`]. More than one success is requested.
/// # Example
/// ```
/// use chasa::prelude::*;
/// let d = one_of('0'..='9').and_then(|c: char| c.to_string().parse::<isize>().map_err(from_error));
/// let p = d.sep1(char(','));
/// assert_eq!(p.parse_ok("1,2,3,4,5"), Some(vec![1,2,3,4,5]));
/// assert_eq!(p.parse_ok(""), None);
/// assert_eq!(p.and(char(',').right(d)).parse_ok("1,2,3,4,5,6"), None);
/// assert_eq!(p.and(char(',')).parse_ok("1,2,3,4,"), Some((vec![1,2,3,4], ',')));
/// ```
pub struct Sep1<P, Q, O, T, U>(pub(crate) P, pub(crate) Q, pub(crate) PhantomData<fn() -> (O, T, U)>);
impl<P: Clone, Q: Clone, O, T, U> Clone for Sep1<P, Q, O, T, U> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<P: Copy, Q: Copy, O, T, U> Copy for Sep1<P, Q, O, T, U> {}
impl<
        I: Input,
        O: FromIterator<T>,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
    > ParserOnce<I, O, E, C, S> for Sep1<P, Q, O, T, U>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.run(args)
    }
}
impl<
        I: Input,
        O: FromIterator<T>,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
    > Parser<I, O, E, C, S> for Sep1<P, Q, O, T, U>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O> {
        let first = self.0.run(args.by_ref())?;
        let Args { input, config, state, consume, error } = args;
        let mut failed = false;
        let o = iter::once(first)
            .chain(SepIterator {
                item_parser: &mut self.0,
                sep_parser: &mut self.1,
                is_first: false,
                failed: &mut failed,
                input,
                config,
                state,
                consume,
                error,
                _marker: PhantomData,
            })
            .collect();
        if failed {
            None
        } else {
            Some(o)
        }
    }
}

/// Manipulate the iterator to repeat the parse while breathing on another parser, and get the result. If it is dropped, no parsing is done.
///
/// A failure that does not consume input will terminate the iterator, a failure that consumes input will cause the entire iterator to fail.
///
/// The [`SepIterator<P,Q>`] argument, required for `F`, is simply an iterator that returns `P:Output`.
///
/// See also [`ManyMap`] and [`Sep`]
/// # Example
/// ```
/// use chasa::prelude::*;
/// let d = one_of('0'..='9').and_then(|c: char| c.to_string().parse::<isize>().map_err(from_error));
/// assert_eq!(
///     d.sep_map(char(','), |iter| iter.take(2).collect())
///         .and(str(",3,4,5").to(true))
///         .parse_ok("1,2,3,4,5"),
///     Some((vec![1,2], true))
/// );
/// assert_eq!(
///     d.sep_map(char(','), |iter| iter.collect())
///         .parse_ok("1,2,3,4,"),
///     Some(vec![1,2,3,4])
/// );
/// ```
pub struct SepMap<P, Q, F, T, U>(pub(crate) P, pub(crate) Q, pub(crate) F, pub(crate) PhantomData<fn() -> (T, U)>);
impl<P: Clone, Q: Clone, F: Clone, T, U> Clone for SepMap<P, Q, F, T, U> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), self.2.clone(), PhantomData)
    }
}
impl<P: Copy, Q: Copy, F: Copy, T, U> Copy for SepMap<P, Q, F, T, U> {}

impl<
        I: Input,
        O,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
        F: FnOnce(SepIterator<P, Q, I, T, U, E, C, S>) -> O,
    > ParserOnce<I, O, E, C, S> for SepMap<P, Q, F, T, U>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        let mut failed = false;
        let o = self.2(SepIterator {
            item_parser: &mut self.0,
            sep_parser: &mut self.1,
            is_first: true,
            failed: &mut failed,
            input,
            config,
            state,
            consume,
            error,
            _marker: PhantomData,
        });
        if failed {
            None
        } else {
            Some(o)
        }
    }
}
impl<
        I: Input,
        O,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
        F: FnMut(SepIterator<P, Q, I, T, U, E, C, S>) -> O,
    > Parser<I, O, E, C, S> for SepMap<P, Q, F, T, U>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        let mut failed = false;
        let o = self.2(SepIterator {
            item_parser: &mut self.0,
            sep_parser: &mut self.1,
            is_first: true,
            failed: &mut failed,
            input,
            config,
            state,
            consume,
            error,
            _marker: PhantomData,
        });
        if failed {
            None
        } else {
            Some(o)
        }
    }
}

pub struct SepBind<P, Q, F, O1, T>(pub(crate) P, pub(crate) Q, pub(crate) F, pub(crate) PhantomData<fn() -> (O1, T)>);
impl<P: Clone, Q: Clone, F: Clone, O1, T> Clone for SepBind<P, Q, F, O1, T> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), self.2.clone(), PhantomData)
    }
}
impl<P: Copy, Q: Copy, F: Copy, O1, T> Copy for SepBind<P, Q, F, O1, T> {}
impl<
        I: Input,
        O1,
        O2,
        T,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, O1, E, C, S>,
        Q: Parser<I, T, E, C, S>,
        R: ParserOnce<I, O2, E, C, S>,
        F: FnOnce(SepIterator<P, Q, I, O1, T, E, C, S>) -> R,
    > ParserOnce<I, O2, E, C, S> for SepBind<P, Q, F, O1, T>
{
    #[inline(always)]
    fn run_once(mut self, mut args: Args<I, E, C, S>) -> Option<O2> {
        let Args { input, config, state, consume, error } = args.by_ref();
        let mut failed = false;
        let r = self.2(SepIterator {
            item_parser: &mut self.0,
            sep_parser: &mut self.1,
            is_first: true,
            failed: &mut failed,
            input,
            config,
            state,
            consume,
            error,
            _marker: PhantomData,
        });
        if failed {
            None
        } else {
            r.run_once(args)
        }
    }
}
impl<
        I: Input,
        O1,
        O2,
        T,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, O1, E, C, S>,
        Q: Parser<I, T, E, C, S>,
        R: ParserOnce<I, O2, E, C, S>,
        F: FnMut(SepIterator<P, Q, I, O1, T, E, C, S>) -> R,
    > Parser<I, O2, E, C, S> for SepBind<P, Q, F, O1, T>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O2> {
        let Args { input, config, state, consume, error } = args.by_ref();
        let mut failed = false;
        let r = self.2(SepIterator {
            item_parser: &mut self.0,
            sep_parser: &mut self.1,
            is_first: true,
            failed: &mut failed,
            input,
            config,
            state,
            consume,
            error,
            _marker: PhantomData,
        });
        if failed {
            None
        } else {
            r.run_once(args)
        }
    }
}

pub struct RepeatIterator<'a, 'b, P: Parser<I, O, E, C, S>, I: Input, O, E: ParseError<I>, C, S: Clone> {
    parser: &'a mut P,
    i: usize,
    start: usize,
    end: Option<usize>,
    args: Args<'a, 'b, I, E, C, S>,
    failed: &'a mut bool,
    _marker: PhantomData<fn() -> O>,
}
impl<'a, 'b, P: Parser<I, O, E, C, S>, I: Input, O, E: ParseError<I>, C, S: Clone> Iterator
    for RepeatIterator<'a, 'b, P, I, O, E, C, S>
{
    type Item = O;
    #[inline(always)]
    fn next(&mut self) -> Option<O> {
        if let Some(end) = self.end {
            if self.i >= end {
                return None;
            }
        }
        match self.parser.by_ref().or_not().run(self.args.by_ref()) {
            None => {
                *self.failed = true;
                None
            },
            Some(Some(o)) => {
                self.i += 1;
                Some(o)
            },
            Some(None) => {
                if self.i < self.start {
                    *self.failed = true;
                }
                None
            },
        }
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.start, self.end)
    }
}

/// Repeat a specified number of times, either range or usize.
/// ```
/// use chasa::prelude::*;
/// assert_eq!(char('a').repeat(2).parse_ok("aaa"), Some("aa".to_string()));
/// assert_eq!(char('a').repeat(4).parse_ok("aaaaa"), Some("aaaa".to_string()));
/// assert_eq!(char('a').repeat(1..=3).parse_ok("aaaaa"), Some("aaa".to_string()));
/// ```
pub struct Repeat<P, O, T> {
    parser: P,
    start: usize,
    end: Option<usize>,
    _marker: PhantomData<fn() -> (O, T)>,
}
#[inline]
pub fn take<O, P, N: RangeWithOrd<usize>, T>(parser: P, count: N) -> Repeat<P, O, T> {
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
impl<P: Clone, O, T> Clone for Repeat<P, O, T> {
    #[inline]
    fn clone(&self) -> Self {
        Self { parser: self.parser.clone(), start: self.start, end: self.end, _marker: PhantomData }
    }
}
impl<P: Copy, O, T> Copy for Repeat<P, O, T> {}
impl<I: Input, O: FromIterator<T>, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>> ParserOnce<I, O, E, C, S>
    for Repeat<P, O, T>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        self.run(args)
    }
}
impl<I: Input, O: FromIterator<T>, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>> Parser<I, O, E, C, S>
    for Repeat<P, O, T>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let mut failed = false;
        let o = RepeatIterator {
            parser: &mut self.parser,
            start: self.start,
            end: self.end,
            i: 0,
            args,
            failed: &mut failed,
            _marker: PhantomData,
        }
        .collect();
        if failed {
            None
        } else {
            Some(o)
        }
    }
}
