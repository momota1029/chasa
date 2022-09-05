use std::{iter, marker::PhantomData, ops::ControlFlow};

use super::{
    cont::Cont,
    error::ParseError,
    input::Input,
    parser::{Args, Parser,ParserOnce},
    util::Consume,
};

fn run_fold<I: Input, O, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>>(
    init: O, mut p: P, mut accum: impl FnMut(O, T) -> O, input: &mut I, config: &C, state: &mut S,
    consume: &mut Consume<(I, S)>, error: &mut E,
) -> Option<O> {
    let (bak_input, bak_state) = (input.clone(), state.clone());
    match consume.cons((bak_input, bak_state), |consume| p.run(Args { input, config, state, consume, error: error })) {
        (Some(item), _) => run_fold(accum(init, item), p, accum, input, config, state, consume, error),
        (None, None) => None,
        (None, Some((bak_input, bak_state))) => {
            *input = bak_input;
            *state = bak_state;
            Some(init)
        },
    }
}

impl<'a, 'b, I: Input, E: ParseError<I>, C, S: Clone> Args<'a, 'b, I, E, C, S> {
    #[inline(always)]
    pub fn fold1<T, O, P: Parser<I, T, E, C, S>>(
        self, init: O, mut p: P, accum: impl Fn(O, T) -> O,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        self.then(p.by_ref()).case(|item, k| k.fold(accum(init, item), p, accum))
    }

    #[inline(always)]
    pub fn fold<T, O, P: Parser<I, T, E, C, S>>(
        self, init: O, p: P, accum: impl Fn(O, T) -> O,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        let Args { input, config, state, consume, error } = self;
        Cont(
            run_fold(init, p, accum, input, config, state, consume, error)
                .map(|o| (o, Args { input, config, state, consume, error })),
        )
    }
}

/// Run the parser greedily as many times as possible and fold the results.
/// # Example
/// ```
/// use chasa::prelude::*;
/// let d = one_of("0123456789").and_then(|c: char| c.to_string().parse::<usize>().map_err(|e| message(error(e))));
/// assert_eq!(d.fold(0,|a,b| a+b).parse_ok("12345"), Some(15));
/// assert_eq!(d.fold(0,|a,b| a+b).parse_ok(""), Some(0));
/// assert_eq!(d.fold(0,|a,b| a+b).and(d).parse_ok("12345"), None);
/// ```
pub struct Fold<T, P, F, O>(T, P, F, PhantomData<fn() -> O>);
impl<T: Clone, P: Clone, F: Clone, O> Clone for Fold<T, P, F, O> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), self.2.clone(), PhantomData)
    }
}
impl<T: Copy, P: Copy, F: Copy, O> Copy for Fold<T, P, F, O> {}
#[inline(always)]
pub fn fold<T, P, F, O>(init: T, parser: P, succ: F) -> Fold<T, P, F, O> {
    Fold(init, parser, succ, PhantomData)
}
impl<I: Input, O, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>, F: Fn(O, T) -> O>
    ParserOnce<I, O, E, C, S> for Fold<O, P, F, T>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        run_fold(self.0, self.1, self.2, input, config, state, consume, error)
    }
}
impl<I: Input, O: Clone, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>, F: Fn(O, T) -> O>
    Parser<I, O, E, C, S> for Fold<O, P, F, T>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        run_fold(self.0.clone(), self.1.by_ref(), &mut self.2, input, config, state, consume, error)
    }
}

/// Run the parser greedily as many times as possible and fold the results. Require more than one success.
/// # Example
/// ```
/// use chasa::prelude::*;
/// let d = one_of("0123456789").and_then(|c: char| c.to_string().parse::<usize>().map_err(|e| message(error(e))));
/// assert_eq!(d.fold1(0,|a,b| a+b).parse_ok("12345"), Some(15));
/// assert_eq!(d.fold1(0,|a,b| a+b).parse_ok(""), None);
/// assert_eq!(d.fold1(0,|a,b| a+b).and(d).parse_ok("12345"), None);
/// ```
pub struct Fold1<T, P, F, O>(T, P, F, PhantomData<fn() -> O>);
impl<T: Clone, P: Clone, F: Clone, O> Clone for Fold1<T, P, F, O> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), self.2.clone(), PhantomData)
    }
}
impl<T: Copy, P: Copy, F: Copy, O> Copy for Fold1<T, P, F, O> {}
#[inline(always)]
pub fn fold1<T, P, F, O>(init: T, parser: P, succ: F) -> Fold1<T, P, F, O> {
    Fold1(init, parser, succ, PhantomData)
}
impl<I: Input, O, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>, F: Fn(O, T) -> O>
    ParserOnce<I, O, E, C, S> for Fold1<O, P, F, T>
{
    #[inline(always)]
    fn run_once(mut self, mut args: Args<I, E, C, S>) -> Option<O> {
        let init = self.2(self.0, self.1.run(args.by_ref())?);
        let Args { input, config, state, consume, error } = args;
        run_fold(init, self.1, self.2, input, config, state, consume, error)
    }
}
impl<I: Input, O: Clone, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>, F: Fn(O, T) -> O>
    Parser<I, O, E, C, S> for Fold1<O, P, F, T>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O> {
        let init = self.2(self.0.clone(), self.1.run(args.by_ref())?);
        let Args { input, config, state, consume, error } = args;
        run_fold(init, self.1.by_ref(), &mut self.2, input, config, state, consume, error)
    }
}

#[inline(always)]
pub(crate) fn run_sep_second_part<O, T, I: Input, E: ParseError<I>, C, S: Clone>(
    item_parser: &mut impl Parser<I, O, E, C, S>, sep_parser: &mut impl Parser<I, T, E, C, S>, input: &mut I,
    config: &C, state: &mut S, consume: &mut Consume<(I, S)>, error: &mut E,
) -> Option<(T, O)> {
    let (bak_input, bak_state) = (input.clone(), state.clone());
    match Consume::new().drop_test(|consume| sep_parser.run(Args { input, config, state, consume, error: error })) {
        (None, consumed) => {
            if consumed {
                consume.drop();
            }
            None
        },
        (Some(sep), _) => match consume.cons((bak_input, bak_state), |consume| {
            item_parser.run(Args { input, config, state, consume, error: error })
        }) {
            (None, None) => None,
            (Some(item), _) => Some((sep, item)),
            (None, Some((bak_input, bak_state))) => {
                *input = bak_input;
                *state = bak_state;
                None
            },
        },
    }
}

#[inline(always)]
fn run_sep_fold<
    I: Input,
    O,
    T,
    E: ParseError<I>,
    C,
    S: Clone,
    P: Parser<I, T, E, C, S>,
    D,
    Q: Parser<I, D, E, C, S>,
>(
    init: O, mut item_parser: P, sep_parser: Q, mut accum: impl FnMut(O, T) -> O, input: &mut I, config: &C,
    state: &mut S, consume: &mut Consume<(I, S)>, error: &mut E,
) -> Option<O> {
    match item_parser.by_ref().or_not().run(Args { input, config, state, consume, error: error })? {
        None => Some(init),
        Some(item) => {
            run_sep_fold_second(accum(init, item), item_parser, sep_parser, accum, input, config, state, consume, error)
        },
    }
}
#[inline(always)]
fn run_sep_fold1<
    I: Input,
    O,
    T,
    E: ParseError<I>,
    C,
    S: Clone,
    P: Parser<I, T, E, C, S>,
    D,
    Q: Parser<I, D, E, C, S>,
>(
    init: O, mut item_parser: P, sep_parser: Q, mut accum: impl FnMut(O, T) -> O, input: &mut I, config: &C,
    state: &mut S, consume: &mut Consume<(I, S)>, error: &mut E,
) -> Option<O> {
    run_sep_fold_second(
        accum(init, item_parser.run(Args { input, config, state, consume, error: error })?),
        item_parser,
        sep_parser,
        accum,
        input,
        config,
        state,
        consume,
        error,
    )
}
fn run_sep_fold_second<
    I: Input,
    O,
    T,
    E: ParseError<I>,
    C,
    S: Clone,
    P: Parser<I, T, E, C, S>,
    D,
    Q: Parser<I, D, E, C, S>,
>(
    init: O, mut item_parser: P, mut sep_parser: Q, mut accum: impl FnMut(O, T) -> O, input: &mut I, config: &C,
    state: &mut S, consume: &mut Consume<(I, S)>, error: &mut E,
) -> Option<O> {
    let (bak_input, bak_state) = (input.clone(), state.clone());
    match consume.cons((bak_input, bak_state), |consume| {
        run_sep_second_part(&mut item_parser, &mut sep_parser, input, config, state, consume, error)
    }) {
        (Some((_, item)), _) => {
            run_sep_fold_second(accum(init, item), item_parser, sep_parser, accum, input, config, state, consume, error)
        },
        (None, None) => None,
        (None, Some((bak_input, bak_state))) => {
            *input = bak_input;
            *state = bak_state;
            Some(init)
        },
    }
}

impl<'a, 'b, I: Input, E: ParseError<I>, C, S: Clone> Args<'a, 'b, I, E, C, S> {
    #[inline(always)]
    pub fn sep_fold1<T, U, O, P: Parser<I, T, E, C, S>, Q: Parser<I, U, E, C, S>>(
        self, init: O, item_parser: P, sep_parser: Q, accum: impl Fn(O, T) -> O,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        let Args { input, config, state, consume, error } = self;
        Cont(
            run_sep_fold1(init, item_parser, sep_parser, accum, input, config, state, consume, error)
                .map(|o| (o, Args { input, config, state, consume, error })),
        )
    }
    #[inline(always)]
    pub fn sep_fold<T, U, O, P: Parser<I, T, E, C, S>, Q: Parser<I, U, E, C, S>>(
        self, init: O, item_parser: P, sep_parser: Q, accum: impl Fn(O, T) -> O,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        let Args { input, config, state, consume, error } = self;
        Cont(
            run_sep_fold(init, item_parser, sep_parser, accum, input, config, state, consume, error)
                .map(|o| (o, Args { input, config, state, consume, error })),
        )
    }
}
impl<'a, 'b, I: Input, O, E: ParseError<I>, C, S: Clone> Cont<'a, 'b, I, O, E, C, S> {
    #[inline(always)]
    pub fn sep_fold1<T, U, P: Parser<I, T, E, C, S>, Q: Parser<I, U, E, C, S>>(
        self, item_parser: P, sep_parser: Q, accum: impl Fn(O, T) -> O,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        self.case(|o, k| k.sep_fold1(o, item_parser, sep_parser, accum))
    }
    #[inline(always)]
    pub fn sep_fold<T, U, P: Parser<I, T, E, C, S>, Q: Parser<I, U, E, C, S>>(
        self, item_parser: P, sep_parser: Q, accum: impl Fn(O, T) -> O,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        self.case(|o, k| k.sep_fold(o, item_parser, sep_parser, accum))
    }
}

/// The parser will be separated by another parser (the result will be discarded) and folded.
/// # Example
/// ```
/// use chasa::prelude::*;
/// let d = one_of("0123456789").and_then(|c:char| c.to_string().parse::<usize>().map_err(error).map_err(message));
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).parse_ok("1,2,3,4,5"), Some(15));
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).parse_ok(""), Some(0));
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).and(char(',').right(d)).parse_ok("1,2,3,4,5,6"), None);
/// assert_eq!(d.sep_fold(0, char(','),|a,b| a+b).and(char(',')).parse_ok("1,2,3,4,"), Some((10, ',')));
/// ```
pub struct SepFold<O, P, Q, F, T, U> {
    pub(crate) init: O,
    pub(crate) p: P,
    pub(crate) sep: Q,
    pub(crate) succ: F,
    pub(crate) _marker: PhantomData<fn() -> (T, U)>,
}
impl<O: Clone, P: Clone, Q: Clone, F: Clone, T, U> Clone for SepFold<O, P, Q, F, T, U> {
    #[inline(always)]
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
impl<O: Copy, P: Copy, Q: Copy, F: Copy, T, U> Copy for SepFold<O, P, Q, F, T, U> {}
#[inline(always)]
pub fn sep_fold<O, P, Q, F, T, U>(init: O, p: P, sep: Q, succ: F) -> SepFold<O, P, Q, F, T, U> {
    SepFold { init, p, sep, succ, _marker: PhantomData }
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
        F: FnMut(O, T) -> O,
    > ParserOnce<I, O, E, C, S> for SepFold<O, P, Q, F, T, U>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        run_sep_fold(self.init, self.p, self.sep, self.succ, input, config, state, consume, error)
    }
}
impl<
        I: Input,
        O: Clone,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
        F: FnMut(O, T) -> O,
    > Parser<I, O, E, C, S> for SepFold<O, P, Q, F, T, U>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        run_sep_fold(
            self.init.clone(),
            self.p.by_ref(),
            self.sep.by_ref(),
            &mut self.succ,
            input,
            config,
            state,
            consume,
            error,
        )
    }
}
/// The parser will be separated by another parser (the result will be discarded) and folded.
/// # Example
/// ```
/// use chasa::prelude::*;
/// let d = one_of("0123456789").and_then(|c:char| c.to_string().parse::<usize>().map_err(error).map_err(message));
/// assert_eq!(d.sep_fold1(0, char(','),|a,b| a+b).parse_ok("1,2,3,4,5"), Some(15));
/// assert_eq!(d.sep_fold1(0, char(','),|a,b| a+b).parse_ok(""), None);
/// assert_eq!(d.sep_fold1(0, char(','),|a,b| a+b).and(char(',').right(d)).parse_ok("1,2,3,4,5,6"), None);
/// assert_eq!(d.sep_fold1(0, char(','),|a,b| a+b).and(char(',')).parse_ok("1,2,3,4,"), Some((10, ',')));
/// ```
pub struct SepFold1<O, P, Q, F, T, U> {
    pub(crate) init: O,
    pub(crate) p: P,
    pub(crate) sep: Q,
    pub(crate) succ: F,
    pub(crate) _marker: PhantomData<fn() -> (T, U)>,
}
impl<O: Clone, P: Clone, Q: Clone, F: Clone, T, U> Clone for SepFold1<O, P, Q, F, T, U> {
    #[inline(always)]
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
impl<O: Copy, P: Copy, Q: Copy, F: Copy, T, U> Copy for SepFold1<O, P, Q, F, T, U> {}
#[inline(always)]
pub fn sep_fold1<O, P, Q, F, T, U>(init: O, p: P, sep: Q, succ: F) -> SepFold1<O, P, Q, F, T, U> {
    SepFold1 { init, p, sep, succ, _marker: PhantomData }
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
        F: FnMut(O, T) -> O,
    > ParserOnce<I, O, E, C, S> for SepFold1<O, P, Q, F, T, U>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        run_sep_fold1(self.init, self.p, self.sep, self.succ, input, config, state, consume, error)
    }
}
impl<
        I: Input,
        O: Clone,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
        F: FnMut(O, T) -> O,
    > Parser<I, O, E, C, S> for SepFold1<O, P, Q, F, T, U>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        run_sep_fold1(
            self.init.clone(),
            self.p.by_ref(),
            self.sep.by_ref(),
            &mut self.succ,
            input,
            config,
            state,
            consume,
            error,
        )
    }
}

#[inline(always)]
fn run_sep_reduce<I: Input, O, T, E: ParseError<I>, C, S: Clone>(
    mut item_p: impl Parser<I, O, E, C, S>, sep_p: impl Parser<I, T, E, C, S>, accum: impl FnMut(O, T, O) -> O,
    input: &mut I, config: &C, state: &mut S, consume: &mut Consume<(I, S)>, error: &mut E,
) -> Option<O> {
    run_sep_reduce_second(
        item_p.run(Args { input, config, state, consume, error: error })?,
        item_p,
        sep_p,
        accum,
        input,
        config,
        state,
        consume,
        error,
    )
}
fn run_sep_reduce_second<I: Input, O, T, E: ParseError<I>, C, S: Clone>(
    sum: O, mut item_p: impl Parser<I, O, E, C, S>, mut sep_p: impl Parser<I, T, E, C, S>,
    mut accum: impl FnMut(O, T, O) -> O, input: &mut I, config: &C, state: &mut S, consume: &mut Consume<(I, S)>,
    error: &mut E,
) -> Option<O> {
    let (bak_input, bak_state) = (input.clone(), state.clone());
    match consume.cons((bak_input, bak_state), |consume| {
        run_sep_second_part(&mut item_p, &mut sep_p, input, config, state, consume, error)
    }) {
        (Some((op, item)), _) => {
            run_sep_reduce_second(accum(sum, op, item), item_p, sep_p, accum, input, config, state, consume, error)
        },
        (None, None) => None,
        (None, Some((bak_input, bak_state))) => {
            *input = bak_input;
            *state = bak_state;
            Some(sum)
        },
    }
}
impl<'a, 'b, I: Input, E: ParseError<I>, C, S: Clone> Args<'a, 'b, I, E, C, S> {
    #[inline(always)]
    pub fn sep_reduce<O, T>(
        self, item_parser: impl Parser<I, O, E, C, S>, sep_parser: impl Parser<I, T, E, C, S>,
        accum: impl FnMut(O, T, O) -> O,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        let Args { input, config, state, consume, error } = self;
        Cont(
            run_sep_reduce(item_parser, sep_parser, accum, input, config, state, consume, error)
                .map(|o| (o, Args { input, config, state, consume, error })),
        )
    }
}
impl<'a, 'b, I: Input, O, E: ParseError<I>, C, S: Clone> Cont<'a, 'b, I, O, E, C, S> {
    #[inline(always)]
    pub fn sep_reduce<T>(
        self, item_parser: impl Parser<I, O, E, C, S>, sep_parser: impl Parser<I, T, E, C, S>,
        accum: impl FnMut(O, T, O) -> O,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        self.case(|o, args| {
            let Args { input, config, state, consume, error } = args;
            Cont(
                run_sep_reduce_second(o, item_parser, sep_parser, accum, input, config, state, consume, error)
                    .map(|o| (o, Args { input, config, state, consume, error })),
            )
        })
    }
}

pub struct SepReduce<P, Q, F, T> {
    pub(crate) item: P,
    pub(crate) sep: Q,
    pub(crate) accum: F,
    pub(crate) _marker: PhantomData<fn() -> T>,
}
impl<P: Clone, Q: Clone, F: Clone, T> Clone for SepReduce<P, Q, F, T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self { item: self.item.clone(), sep: self.sep.clone(), accum: self.accum.clone(), _marker: PhantomData }
    }
}
impl<P: Copy, Q: Copy, F: Copy, T> Copy for SepReduce<P, Q, F, T> {}
#[inline(always)]
pub fn sep_reduce<
    O,
    T,
    I: Input,
    C,
    E: ParseError<I>,
    S: Clone,
    P: Parser<I, O, E, C, S>,
    Q: Parser<I, T, E, C, S>,
    F: FnMut(O, T, O) -> O,
>(
    item_parser: P, sep_parser: Q, accum: F,
) -> SepReduce<P, Q, F, T> {
    SepReduce { item: item_parser, sep: sep_parser, accum, _marker: PhantomData }
}

impl<
        I: Input,
        O,
        T,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, O, E, C, S>,
        Q: Parser<I, T, E, C, S>,
        F: FnMut(O, T, O) -> O,
    > ParserOnce<I, O, E, C, S> for SepReduce<P, Q, F, T>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        run_sep_reduce(self.item, self.sep, self.accum, input, config, state, consume, error)
    }
}
impl<
        I: Input,
        O,
        T,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, O, E, C, S>,
        Q: Parser<I, T, E, C, S>,
        F: FnMut(O, T, O) -> O,
    > Parser<I, O, E, C, S> for SepReduce<P, Q, F, T>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        run_sep_reduce(self.item.by_ref(), self.sep.by_ref(), &mut self.accum, input, config, state, consume, error)
    }
}

fn run_extend<O: Extend<T>, I: Input, T, E: ParseError<I>, C, S: Clone>(
    xs: &mut O, mut item_p: impl Parser<I, T, E, C, S>, input: &mut I, config: &C, state: &mut S,
    consume: &mut Consume<(I, S)>, error: &mut E,
) -> bool {
    let (bak_input, bak_state) = (input.clone(), state.clone());
    match consume
        .cons((bak_input, bak_state), |consume| item_p.run(Args { input, config, state, consume, error: error }))
    {
        (Some(item), _) => {
            xs.extend(iter::once(item));
            run_extend(xs, item_p, input, config, state, consume, error)
        },
        (None, None) => false,
        (None, Some((bak_input, bak_state))) => {
            *input = bak_input;
            *state = bak_state;
            true
        },
    }
}

impl<'a, 'b, I: Input, E: ParseError<I>, C, S: Clone> Args<'a, 'b, I, E, C, S> {
    #[inline(always)]
    pub fn extend1<O: Extend<T>, T, P: Parser<I, T, E, C, S>>(
        self, mut collection: O, mut p: P,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        self.then(p.by_ref()).case(|item, k| {
            collection.extend(iter::once(item));
            k.extend(collection, p)
        })
    }

    #[inline(always)]
    pub fn extend<O: Extend<T>, T>(
        self, mut collection: O, item_parser: impl Parser<I, T, E, C, S>,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        let Args { input, config, state, consume, error } = self;
        if run_extend(&mut collection, item_parser, input, config, state, consume, error) {
            Cont(Some((collection, Args { input, config, state, consume, error })))
        } else {
            Cont(None)
        }
    }
}

/// Repeat extend with parser results.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(any.extend(String::new()).parse_ok("abcde"), Some("abcde".to_string()));
/// assert_eq!(char('a').extend(String::new()).parse_ok("aaabbb"), Some("aaa".to_string()));
/// assert_eq!(char('a').extend(String::new()).parse_ok("b"), Some("".to_string()));
/// ```
pub struct ExtendParser<O, P, T>(pub(crate) O, pub(crate) P, pub(crate) PhantomData<fn() -> T>);
impl<O: Clone, P: Clone, T> Clone for ExtendParser<O, P, T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<O: Copy, P: Copy, T> Copy for ExtendParser<O, P, T> {}
impl<I: Input, O: Extend<T>, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>> ParserOnce<I, O, E, C, S>
    for ExtendParser<O, P, T>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        if run_extend(&mut self.0, self.1, input, config, state, consume, error) {
            Some(self.0)
        } else {
            None
        }
    }
}
impl<I: Input, O: Extend<T> + Clone, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>> Parser<I, O, E, C, S>
    for ExtendParser<O, P, T>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        let mut xs = self.0.clone();
        if run_extend(&mut xs, self.1.by_ref(), input, config, state, consume, error) {
            Some(xs)
        } else {
            None
        }
    }
}

/// Repeat extend with parser results.
/// # Example
/// ```
/// use chasa::prelude::*;
/// assert_eq!(any.extend(String::new()).parse_ok("abcde"), Some("abcde".to_string()));
/// assert_eq!(char('a').extend(String::new()).parse_ok("aaabbb"), Some("aaa".to_string()));
/// assert_eq!(char('a').extend(String::new()).parse_ok("b"), Some("".to_string()));
/// ```
pub struct Extend1Parser<O, P, T>(pub(crate) O, pub(crate) P, pub(crate) PhantomData<fn() -> T>);
impl<O: Clone, P: Clone, T> Clone for Extend1Parser<O, P, T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), PhantomData)
    }
}
impl<O: Copy, P: Copy, T> Copy for Extend1Parser<O, P, T> {}
impl<I: Input, O: Extend<T>, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>> ParserOnce<I, O, E, C, S>
    for Extend1Parser<O, P, T>
{
    #[inline(always)]
    fn run_once(mut self, mut args: Args<I, E, C, S>) -> Option<O> {
        self.0.extend(iter::once(self.1.run(args.by_ref())?));
        let Args { input, config, state, consume, error } = args;
        if run_extend(&mut self.0, self.1, input, config, state, consume, error) {
            Some(self.0)
        } else {
            None
        }
    }
}
impl<I: Input, O: Extend<T> + Clone, T, E: ParseError<I>, C, S: Clone, P: Parser<I, T, E, C, S>> Parser<I, O, E, C, S>
    for Extend1Parser<O, P, T>
{
    #[inline(always)]
    fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<O> {
        let mut xs = self.0.clone();
        xs.extend(iter::once(self.1.run(args.by_ref())?));
        let Args { input, config, state, consume, error } = args;
        if run_extend(&mut xs, self.1.by_ref(), input, config, state, consume, error) {
            Some(xs)
        } else {
            None
        }
    }
}

#[inline(always)]
fn run_sep_extend<I: Input, O: Extend<T>, T, U, E: ParseError<I>, C, S: Clone>(
    xs: &mut O, mut item_p: impl Parser<I, T, E, C, S>, sep_p: impl Parser<I, U, E, C, S>, input: &mut I, config: &C,
    state: &mut S, consume: &mut Consume<(I, S)>, error: &mut E,
) -> bool {
    match item_p.by_ref().or_not().run(Args { input, config, state, consume, error: error }) {
        None => false,
        Some(None) => true,
        Some(Some(item)) => {
            xs.extend(iter::once(item));
            run_sep_extend_second(xs, item_p, sep_p, input, config, state, consume, error)
        },
    }
}
#[inline(always)]
fn run_sep_extend1<I: Input, O: Extend<T>, T, U, E: ParseError<I>, C, S: Clone>(
    xs: &mut O, mut item_p: impl Parser<I, T, E, C, S>, sep_p: impl Parser<I, U, E, C, S>, input: &mut I, config: &C,
    state: &mut S, consume: &mut Consume<(I, S)>, error: &mut E,
) -> bool {
    match item_p.run(Args { input, config, state, consume, error: error }) {
        None => false,
        Some(item) => {
            xs.extend(iter::once(item));
            run_sep_extend_second(xs, item_p, sep_p, input, config, state, consume, error)
        },
    }
}
fn run_sep_extend_second<I: Input, O: Extend<T>, T, U, E: ParseError<I>, C, S: Clone>(
    xs: &mut O, mut item_p: impl Parser<I, T, E, C, S>, mut sep_p: impl Parser<I, U, E, C, S>, input: &mut I,
    config: &C, state: &mut S, consume: &mut Consume<(I, S)>, error: &mut E,
) -> bool {
    let (bak_input, bak_state) = (input.clone(), state.clone());
    match consume.cons((bak_input, bak_state), |consume| {
        run_sep_second_part(&mut item_p, &mut sep_p, input, config, state, consume, error)
    }) {
        (Some((_, item)), _) => {
            xs.extend(iter::once(item));
            run_sep_extend_second(xs, item_p, sep_p, input, config, state, consume, error)
        },
        (None, None) => false,
        (None, Some((bak_input, bak_state))) => {
            *input = bak_input;
            *state = bak_state;
            true
        },
    }
}

impl<'a, 'b, I: Input, E: ParseError<I>, C, S: Clone> Args<'a, 'b, I, E, C, S> {
    #[inline(always)]
    pub fn sep_extend1<T, U, O: Extend<T>>(
        self, mut collection: O, item_parser: impl Parser<I, T, E, C, S>, sep_parser: impl Parser<I, U, E, C, S>,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        let Args { input, config, state, consume, error } = self;
        if run_sep_extend1(&mut collection, item_parser, sep_parser, input, config, state, consume, error) {
            Cont(Some((collection, Args { input, config, state, consume, error })))
        } else {
            Cont(None)
        }
    }
    #[inline(always)]
    pub fn sep_extend<T, U, O: Extend<T>>(
        self, mut collection: O, item_parser: impl Parser<I, T, E, C, S>, sep_parser: impl Parser<I, U, E, C, S>,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        let Args { input, config, state, consume, error } = self;
        if run_sep_extend(&mut collection, item_parser, sep_parser, input, config, state, consume, error) {
            Cont(Some((collection, Args { input, config, state, consume, error })))
        } else {
            Cont(None)
        }
    }
}

impl<'a, 'b, I: Input, O, E: ParseError<I>, C, S: Clone> Cont<'a, 'b, I, O, E, C, S> {
    #[inline(always)]
    pub fn sep_extend1<T, U>(
        self, item_parser: impl Parser<I, T, E, C, S>, sep_parser: impl Parser<I, U, E, C, S>,
    ) -> Cont<'a, 'b, I, O, E, C, S>
    where
        O: Extend<T>,
    {
        self.case(|o, k| k.sep_extend1(o, item_parser, sep_parser))
    }
    #[inline(always)]
    pub fn sep_extend<T, U>(
        self, item_parser: impl Parser<I, T, E, C, S>, sep_parser: impl Parser<I, U, E, C, S>,
    ) -> Cont<'a, 'b, I, O, E, C, S>
    where
        O: Extend<T>,
    {
        self.case(|o, k| k.sep_extend(o, item_parser, sep_parser))
    }
}
/// Repeat the parser, separating it with other parsers, and extend it.
pub struct SepExtend<O, P, Q, T, U> {
    pub(crate) init: O,
    pub(crate) p: P,
    pub(crate) sep: Q,
    pub(crate) _marker: PhantomData<fn() -> (T, U)>,
}
impl<O: Clone, P: Clone, Q: Clone, T, U> Clone for SepExtend<O, P, Q, T, U> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self { init: self.init.clone(), p: self.p.clone(), sep: self.sep.clone(), _marker: PhantomData }
    }
}
#[inline(always)]
pub fn sep_extend<O, P, Q, T, U>(init: O, p: P, sep: Q) -> SepExtend<O, P, Q, T, U> {
    SepExtend { init, p, sep, _marker: PhantomData }
}
impl<
        I: Input,
        O: Extend<T>,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
    > ParserOnce<I, O, E, C, S> for SepExtend<O, P, Q, T, U>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        if run_sep_extend(&mut self.init, self.p, self.sep, input, config, state, consume, error) {
            Some(self.init)
        } else {
            None
        }
    }
}
impl<
        I: Input,
        O: Extend<T> + Clone,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
    > Parser<I, O, E, C, S> for SepExtend<O, P, Q, T, U>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        let mut xs = self.init.clone();
        if run_sep_extend(&mut xs, self.p.by_ref(), self.sep.by_ref(), input, config, state, consume, error) {
            Some(xs)
        } else {
            None
        }
    }
}

/// Repeat the parser, separating it with other parsers, and extend it. Require more than one success.
pub struct SepExtend1<O, P, Q, T, U> {
    pub(crate) init: O,
    pub(crate) p: P,
    pub(crate) sep: Q,
    pub(crate) _marker: PhantomData<fn() -> (T, U)>,
}
impl<O: Clone, P: Clone, Q: Clone, T, U> Clone for SepExtend1<O, P, Q, T, U> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self { init: self.init.clone(), p: self.p.clone(), sep: self.sep.clone(), _marker: PhantomData }
    }
}
#[inline(always)]
pub fn sep_extend1<O, P, Q, T, U>(init: O, p: P, sep: Q) -> SepExtend1<O, P, Q, T, U> {
    SepExtend1 { init, p, sep, _marker: PhantomData }
}
impl<
        I: Input,
        O: Extend<T>,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
    > ParserOnce<I, O, E, C, S> for SepExtend1<O, P, Q, T, U>
{
    #[inline(always)]
    fn run_once(mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        if run_sep_extend1(&mut self.init, self.p, self.sep, input, config, state, consume, error) {
            Some(self.init)
        } else {
            None
        }
    }
}
impl<
        I: Input,
        O: Extend<T> + Clone,
        T,
        U,
        E: ParseError<I>,
        C,
        S: Clone,
        P: Parser<I, T, E, C, S>,
        Q: Parser<I, U, E, C, S>,
    > Parser<I, O, E, C, S> for SepExtend1<O, P, Q, T, U>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        let Args { input, config, state, consume, error } = args;
        let mut xs = self.init.clone();
        if run_sep_extend1(&mut xs, self.p.by_ref(), self.sep.by_ref(), input, config, state, consume, error) {
            Some(xs)
        } else {
            None
        }
    }
}

fn run_tail_rec<I: Input, O, T, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, ControlFlow<O, T>, E, C, S>>(
    init: T, mut parser: impl FnMut(T) -> P, mut args: Args<I, E, C, S>,
) -> Option<O> {
    match parser(init).run_once(args.by_ref())? {
        ControlFlow::Break(o) => Some(o),
        ControlFlow::Continue(k) => run_tail_rec(k, parser, args),
    }
}
impl<'a, 'b, I: Input, E: ParseError<I>, C, S: Clone> Args<'a, 'b, I, E, C, S> {
    pub fn tail_rec<T, O>(
        self, init: T, mut f: impl FnMut(T, Self) -> Cont<'a, 'b, I, ControlFlow<O, T>, E, C, S>,
    ) -> Cont<'a, 'b, I, O, E, C, S> {
        match f(init, self).0 {
            None => Cont(None),
            Some((ControlFlow::Break(o), k)) => Cont(Some((o, k))),
            Some((ControlFlow::Continue(s), k)) => k.tail_rec(s, f),
        }
    }
}
impl<'a, 'b, I: Input, O, E: ParseError<I>, C, S: Clone> Cont<'a, 'b, I, O, E, C, S> {
    #[inline(always)]
    pub fn tail_rec<T>(
        self, f: impl FnMut(O, Args<'a, 'b, I, E, C, S>) -> Cont<'a, 'b, I, ControlFlow<T, O>, E, C, S>,
    ) -> Cont<'a, 'b, I, T, E, C, S> {
        self.case(|o, k| k.tail_rec(o, f))
    }
}

/// A recursive parser. It takes `T` and processes the continuation with `Err(T)` and the output with `Ok(U)`.
/// # Example
/// ```
/// use {chasa::prelude::*, std::ops::ControlFlow::*};
/// let d = one_of("0123456789").and_then(|c: char| c.to_string().parse::<usize>().map_err(error).map_err(message));
/// let mut p = char('(').right(tail_rec(0, move |n| d.map(move |m| Continue(m+n)).or(char(')').to(Break(n)))));
/// assert_eq!(p.by_ref().parse_ok("(12345)"), Some(15));
/// assert_eq!(p.by_ref().parse_ok("(12)345)"), Some(3));
/// assert_eq!(p.by_ref().parse_ok("()"), Some(0));
/// assert_eq!(p.by_ref().parse_ok("(12a345)"), None);
/// ```
#[derive(Clone, Copy)]
pub struct TailRec<T, F> {
    pub(crate) init: T,
    pub(crate) f: F,
}
#[inline(always)]
pub fn tail_rec<
    I: Input,
    O,
    T,
    E: ParseError<I>,
    C,
    S: Clone,
    P: ParserOnce<I, ControlFlow<O, T>, E, C, S>,
    F: FnMut(T) -> P,
>(
    init: T, f: F,
) -> TailRec<T, F> {
    TailRec { init, f }
}
impl<I: Input, O, T, E: ParseError<I>, C, S: Clone, P: ParserOnce<I, ControlFlow<O, T>, E, C, S>, F: FnMut(T) -> P>
    ParserOnce<I, O, E, C, S> for TailRec<T, F>
{
    #[inline(always)]
    fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
        run_tail_rec(self.init, self.f, args)
    }
}
impl<
        I: Input,
        O,
        T: Clone,
        E: ParseError<I>,
        C,
        S: Clone,
        P: ParserOnce<I, ControlFlow<O, T>, E, C, S>,
        F: FnMut(T) -> P,
    > Parser<I, O, E, C, S> for TailRec<T, F>
{
    #[inline(always)]
    fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
        run_tail_rec(self.init.clone(), &mut self.f, args)
    }
}
