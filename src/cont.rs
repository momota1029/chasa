use std::{fmt::Display, iter::once};

use crate::{
    combi,
    error::{Builder as Eb, CustomBuilder as Cb},
    prim::Error,
    util::run_drop,
    ICont, IOk, IReturn, Input, Parser, ParserOnce,
};

impl<'a, I: Input, C, S, M: Cb> ICont<'a, I, C, S, M> {
    #[inline]
    pub fn pure<O>(self, value: O) -> IReturn<'a, O, I, C, S, M> {
        IReturn(Ok((value, self)))
    }
    #[inline]
    pub fn pure_or<P: Parser<I, C, S, M>>(self, o: Option<P::Output>, p: P) -> IReturn<'a, P::Output, I, C, S, M> {
        match o {
            Some(o) => self.pure(o),
            None => self.then(p),
        }
    }
    #[inline]
    pub fn then<P: ParserOnce<I, C, S, M>>(self, p: P) -> IReturn<'a, P::Output, I, C, S, M> {
        let ICont { config, drop, ok } = self;
        IReturn(p.run_once(ICont { config, drop, ok }).map(|(o, ok)| (o, ok.to_cont(config, drop))))
    }
    #[inline]
    pub fn fail<O, E: Display + 'static>(self, err: Error<E>) -> IReturn<'a, O, I, C, S, M> {
        IReturn(Err(match err {
            Error::Unexpect(token) => Eb::unexpected(token),
            Error::Message(msg) => Eb::message(msg),
        }
        .at::<I>(self.ok.input.index(), self.ok.input.pos(), None)))
    }
    #[inline]
    pub fn config<O, F: FnOnce(&C, Self) -> IReturn<'a, O, I, C, S, M>>(self, f: F) -> IReturn<'a, O, I, C, S, M> {
        f(self.config, self)
    }

    pub fn tail_rec<O1, O2>(self, o1: O1, f: impl Fn(O1, Self) -> IReturn<'a, Result<O2, O1>, I, C, S, M>) -> IReturn<'a, O2, I, C, S, M> {
        match f(o1, self).0 {
            Err(e) => IReturn(Err(e)),
            Ok((Err(o), ok)) => ok.tail_rec(o, f),
            Ok((Ok(o), ok)) => IReturn(Ok((o, ok))),
        }
    }
}
impl<'a, I: Input + Clone, C, S: Clone, M: Cb> ICont<'a, I, C, S, M> {
    pub fn fold<O, P: Parser<I, C, S, M>>(self, o1: O, p: P, f: impl Fn(O, P::Output) -> O) -> IReturn<'a, O, I, C, S, M> {
        let ICont { ok, config, drop } = self;
        let (input2, state2, cutted) = (ok.input.clone(), ok.state.clone(), ok.cutted);
        match run_drop(p.to_ref(), ICont { ok, config, drop }, (input2, state2)) {
            (Err(e), None) => IReturn(Err(e)),
            (Err(e), Some((input, state))) => IReturn(Ok((o1, IOk { input, state, err: Some(e), cutted }.to_cont(config, drop)))),
            (Ok((o2, ok)), _) => ok.to_cont(config, drop).fold(f(o1, o2), p, f),
        }
    }
    #[inline]
    pub fn fold1<O, P: Parser<I, C, S, M>>(self, o1: O, p: P, f: impl Fn(O, P::Output) -> O) -> IReturn<'a, O, I, C, S, M> {
        self.then(p.to_ref()).case(|o2, k| k.fold(f(o1, o2), p, f))
    }

    #[inline]
    pub fn extend<O: Extend<P::Output>, P: Parser<I, C, S, M>>(self, o: O, p: P) -> IReturn<'a, O, I, C, S, M> {
        self.fold(o, p, |mut o, v| {
            o.extend(once(v));
            o
        })
    }
    #[inline]
    pub fn extend1<O: Extend<P::Output>, P: Parser<I, C, S, M>>(self, o: O, p: P) -> IReturn<'a, O, I, C, S, M> {
        self.fold1(o, p, |mut o, v| {
            o.extend(once(v));
            o
        })
    }

    #[inline]
    pub fn sep_fold<O, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>(
        self, o: O, p: P1, sep: P2, f: impl Fn(O, P1::Output) -> O,
    ) -> IReturn<'a, O, I, C, S, M> {
        self.then(combi::SepFold { init: o, p, sep, succ: f })
    }

    // #[inline]
    // pub fn sep1<O, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>(
    //     self, o: O, p: P1, sep: P2, f: impl Fn(O, P1::Output) -> O,
    // ) -> IReturn<'a, O, I, C, S, M> {
    //     self.then(combi::Sep1 { init: o, p, sep, succ: f })
    // }

    #[inline]
    pub fn sep_extend<O: Extend<P1::Output>, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>(
        self, o: O, p: P1, sep: P2,
    ) -> IReturn<'a, O, I, C, S, M> {
        self.then(combi::SepExtend { init: o, p, sep })
    }
    #[inline]
    pub fn sep_extend1<O: Extend<P1::Output>, P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>(
        self, o: O, p: P1, sep: P2,
    ) -> IReturn<'a, O, I, C, S, M> {
        self.then(combi::SepExtend1 { init: o, p, sep })
    }
}
impl<'a, O, I: Input, C, S, M: Cb> IReturn<'a, O, I, C, S, M> {
    #[inline]
    pub fn case<O2>(self, f: impl FnOnce(O, ICont<'a, I, C, S, M>) -> IReturn<'a, O2, I, C, S, M>) -> IReturn<'a, O2, I, C, S, M> {
        IReturn(self.0.and_then(|(o, k)| f(o, k).0))
    }
    #[inline]
    pub fn map<O2>(self, f: impl FnOnce(O) -> O2) -> IReturn<'a, O2, I, C, S, M> {
        IReturn(self.0.map(|(o, k)| (f(o), k)))
    }
    #[inline]
    pub fn value<O2>(self, o: O2) -> IReturn<'a, O2, I, C, S, M> {
        IReturn(self.0.map(|(_, k)| (o, k)))
    }
    #[inline]
    pub fn bind<P: ParserOnce<I, C, S, M>>(self, f: impl Fn(O) -> P) -> IReturn<'a, P::Output, I, C, S, M> {
        self.case(|o, k| k.then(f(o)))
    }
    #[inline]
    pub fn and<P: ParserOnce<I, C, S, M>>(self, p: P) -> IReturn<'a, (O, P::Output), I, C, S, M> {
        self.case(|o1, k| k.then(p).map(|o2| (o1, o2)))
    }
    #[inline]
    pub fn left<P: ParserOnce<I, C, S, M>>(self, p: P) -> IReturn<'a, O, I, C, S, M> {
        self.case(|o, k| k.then(p).value(o))
    }
    #[inline]
    pub fn right<P: ParserOnce<I, C, S, M>>(self, p: P) -> IReturn<'a, P::Output, I, C, S, M> {
        self.case(|_, k| k.then(p))
    }
    #[inline]
    pub fn tail_rec<O2>(self, f: impl Fn(O, ICont<I, C, S, M>) -> IReturn<'a, Result<O2, O>, I, C, S, M>) -> IReturn<'a, O2, I, C, S, M> {
        self.case(|o, k| k.tail_rec(o, f))
    }
    #[inline]
    pub fn label<L: Display + 'static>(self, label: L) -> Self {
        IReturn(self.0.map_err(|e| e.label(label)))
    }
}

impl<'a, O, I: Input + Clone + 'a, C, S: Clone + 'a, M: Cb> IReturn<'a, O, I, C, S, M> {
    #[inline]
    pub fn fold<P: Parser<I, C, S, M>>(self, p: P, f: impl Fn(O, P::Output) -> O) -> IReturn<'a, O, I, C, S, M> {
        self.case(|o, k| k.fold(o, p, f))
    }
    #[inline]
    pub fn fold1<P: Parser<I, C, S, M>>(self, p: P, f: impl Fn(O, P::Output) -> O) -> IReturn<'a, O, I, C, S, M> {
        self.case(|o, k| k.fold1(o, p, f))
    }
    #[inline]
    pub fn extend<P: Parser<I, C, S, M>>(self, p: P) -> IReturn<'a, O, I, C, S, M>
    where
        O: Extend<P::Output>,
    {
        self.case(|o, k| k.extend(o, p))
    }
    #[inline]
    pub fn extend1<P: Parser<I, C, S, M>>(self, p: P) -> IReturn<'a, O, I, C, S, M>
    where
        O: Extend<P::Output>,
    {
        self.case(|o, k| k.extend1(o, p))
    }

    #[inline]
    pub fn sep_fold<P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>(
        self, p: P1, sep: P2, f: impl Fn(O, P1::Output) -> O,
    ) -> IReturn<'a, O, I, C, S, M> {
        self.case(|o, k| k.sep_fold(o, p, sep, f))
    }

    // #[inline]
    // pub fn sep1<P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>(self, p: P1, sep: P2, f: impl Fn(O, P1::Output) -> O) -> IReturn<'a, O, I, C, S, M> {
    //     self.case(|o, k| k.sep1(o, p, sep, f))
    // }

    #[inline]
    pub fn sep_extend<P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>(self, p: P1, sep: P2) -> IReturn<'a, O, I, C, S, M>
    where
        O: Extend<P1::Output>,
    {
        self.case(|o, k| k.sep_extend(o, p, sep))
    }
    #[inline]
    pub fn sep_extend1<P1: Parser<I, C, S, M>, P2: Parser<I, C, S, M>>(self, p: P1, sep: P2) -> IReturn<'a, O, I, C, S, M>
    where
        O: Extend<P1::Output>,
    {
        self.case(|o, k| k.sep_extend1(o, p, sep))
    }
}
