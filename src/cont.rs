use crate::error;

use super::{
    error::ParseError,
    input::Input,
    parser::{Args, ParserOnce},
};

pub struct Cont<'a, 'b, I: Input, O, E: ParseError<I>, C, S: Clone>(pub(crate) Option<(O, Args<'a, 'b, I, E, C, S>)>);

impl<'a, 'b, I: Input, E: ParseError<I>, C, S: Clone> Args<'a, 'b, I, E, C, S> {
    #[inline(always)]
    pub fn to<O>(self, value: O) -> Cont<'a, 'b, I, O, E, C, S> {
        Cont(Some((value, self)))
    }
    #[inline(always)]
    pub fn done(self) -> Cont<'a, 'b, I, (), E, C, S> {
        Cont(Some(((), self)))
    }
    #[inline(always)]
    pub fn fail<M, O>(self, message: M) -> Cont<'a, 'b, I, O, E, C, S>
    where
        E::Message: From<M>,
    {
        if self.error.add(None, self.input.position()) {
            self.error.set(message.into())
        }
        Cont(None)
    }

    #[inline(always)]
    pub fn satisfy_cont<O>(
        mut self, f: impl FnOnce(&I::Token, Args<'a, 'b, I, E, C, S>) -> Option<Cont<'a, 'b, I, O, E, C, S>>,
    ) -> Cont<'a, 'b, I, O, E, C, S>
    where
        E::Message: From<error::Unexpected<error::Token<I::Token>>>,
    {
        match self.uncons() {
            None => Cont(None),
            Some((c, start)) => {
                let Args { input, config, state, consume, error } = self;
                // When `f` returns `None`, the reference to the `args` no longer exists, but the compiler takes the lifetime into account. To deal with this, unsafe is used. A better solution is sought.
                let (input2, error2) = unsafe { (&mut *(input as *mut I), &mut *(error as *mut E)) };
                match f(&c, Args { input, config, state, consume, error }) {
                    Some(k) => k,
                    None => {
                        let end = input2.position();
                        if error2.add(Some(start), end) {
                            error2.set(error::unexpected(error::token(c)).into());
                        }
                        Cont(None)
                    },
                }
            },
        }
    }

    #[inline(always)]
    pub fn then<P: ParserOnce<I, O, E, C, S>, O>(mut self, p: P) -> Cont<'a, 'b, I, O, E, C, S> {
        Cont(p.run_once(self.by_ref()).map(move |o| (o, self)))
    }

    #[inline(always)]
    pub fn config<O>(self, f: impl FnOnce(&'a C, Self) -> Cont<'a, 'b, I, O, E, C, S>) -> Cont<'a, 'b, I, O, E, C, S> {
        f(self.config, self)
    }
}

impl<'a, 'b, I: Input, O, E: ParseError<I>, C, S: Clone> Cont<'a, 'b, I, O, E, C, S> {
    #[inline(always)]
    pub fn to<O2>(self, value: O2) -> Cont<'a, 'b, I, O2, E, C, S> {
        self.case(|_, k| k.to(value))
    }
    #[inline(always)]
    pub fn case<T>(
        self, f: impl FnOnce(O, Args<'a, 'b, I, E, C, S>) -> Cont<'a, 'b, I, T, E, C, S>,
    ) -> Cont<'a, 'b, I, T, E, C, S> {
        Cont(self.0.and_then(|(o, k)| f(o, k).0))
    }
    #[inline(always)]
    pub fn bind<F: FnOnce(O) -> P, P: ParserOnce<I, O2, E, C, S>, O2>(self, f: F) -> Cont<'a, 'b, I, O2, E, C, S> {
        self.case(|o, k| k.then(f(o)))
    }

    #[inline(always)]
    pub fn map<T>(self, f: impl FnOnce(O) -> T) -> Cont<'a, 'b, I, T, E, C, S> {
        Cont(self.0.map(|(o, k)| (f(o), k)))
    }
    #[inline(always)]
    pub fn and<P: ParserOnce<I, O2, E, C, S>, O2>(self, p: P) -> Cont<'a, 'b, I, (O, O2), E, C, S> {
        self.case(|o1, k| k.then(p).map(|o2| (o1, o2)))
    }
    #[inline(always)]
    pub fn left<P: ParserOnce<I, O2, E, C, S>, O2>(self, p: P) -> Cont<'a, 'b, I, O, E, C, S> {
        self.case(|o, k| k.then(p).to(o))
    }
    #[inline(always)]
    pub fn right<P: ParserOnce<I, O2, E, C, S>, O2>(self, p: P) -> Cont<'a, 'b, I, O2, E, C, S> {
        self.case(|_, k| k.then(p))
    }
}
