use std::marker::PhantomData;

use super::{
    combi::Value,
    error::ParseError,
    parser::Args,
    prelude::{Input, Parser, ParserOnce},
};

/**
The parsers in the tuple are invoked in turn, and if all succeed, their contents are returned as a tuple.
# Example
```
use chasa::prelude::*;
let p = tuple((char('a'), char('b'), char('c')));
assert_eq!(p.parse_ok("abc"), Some(('a','b','c')));
assert_eq!(p.parse_ok("abd"), None);
```
*/
#[derive(Clone, Copy)]
pub struct Chain<PS>(PS);
#[inline(always)]
pub fn tuple<PS>(parsers: PS) -> Chain<PS> {
    Chain(parsers)
}
macro_rules! chain_derive_fold {
    ($p1:ident $p1t:ident $o1t:ident,$($ps:ident $pst:ident $ost:ident),*) => {
        chain_derive_fold!(($p1 $p1t $o1t,),($($ps $pst $ost,)*));
    };
    (($($ps:ident $pst:ident $ost:ident,)*),()) => {};
    (($($ps1:ident $ps1t:ident $os1t:ident,)+), ($p:ident $pt:ident $ot:ident, $($ps2:ident $ps2t:ident $os2t:ident,)*)) => {
        chain_derive!($($ps1 $ps1t $os1t),+);
        chain_derive_fold!(($($ps1 $ps1t $os1t,)+ $p $pt $ot,), ($($ps2 $ps2t $os2t,)*));
    }
}
macro_rules! chain_derive {
    ($($ps:ident $pst:ident $ost:ident),+) => {
        impl<I: Input, $($ost),+, E: ParseError<I>, C, S: Clone, $($pst: ParserOnce<I,$ost,E,C,S>),+> ParserOnce<I,($($ost,)+),E,C,S> for Chain<($($pst,)+)> {
            #[inline(always)]
            fn run_once(self, mut args: Args<I, E, C, S>) -> Option<($($ost,)+)> {
                let ($($ps,)+) = self.0;
                Some(($($ps.run_once(args.by_ref())?,)+))
            }
        }
        impl<I: Input, $($ost),+, E: ParseError<I>, C, S: Clone, $($pst: Parser<I,$ost,E,C,S>),+> Parser<I,($($ost,)+),E,C,S> for Chain<($($pst,)+)> {
            #[inline(always)]
            fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<($($ost,)+)> {
                let ($($ps,)+) = &mut self.0;
                Some(($($ps.run(args.by_ref())?,)+))
            }
        }
    }
}

chain_derive_fold!(
    p1 P1 O1,
    p2 P2 O2,
    p3 P3 O3,
    p4 P4 O4,
    p5 P5 O5,
    p6 P6 O6,
    p7 P7 O7,
    p8 P8 O8,
    p9 P9 O9,
    p10 P10 O10,
    p11 P11 O11,
    p12 P12 O12,
    p13 P13 O13,
    p14 P14 O14,
    p15 P15 O15,
    p16 P16 O16,
    p17 P17 O17,
    p18 P18 O18,
    p19 P19 O19,
    p20 P20 O20,
    p21 P21 O21,
    p22 P22 O22,
    p23 P23 O23,
    p24 P24 O24,
    p25 P25 O25,
    p26 P26 O26,
    p27 P27 O27,
    p28 P28 O28,
    p29 P29 O29,
    p30 P30 O30
);

/**
 * The parsers in the tuple are invoked in turn and only the last result is returned if all succeed.
# Example
```
use chasa::prelude::*;
let p = chain((char('a'), char('b'), char('c')));
assert_eq!(p.parse_ok("abc"), Some('c'));
assert_eq!(p.parse_ok("abd"), None);
```
*/
pub struct ChainRight<Ps, Os>(Ps, PhantomData<fn() -> Os>);
#[inline]
pub fn chain<Ps, Os>(parsers: Ps) -> ChainRight<Ps, Os> {
    ChainRight(parsers, PhantomData)
}
#[inline]
pub fn skip_chain<Ps, Os, O2>(parsers: Ps) -> Value<ChainRight<Ps, Os>, (), O2> {
    Value(ChainRight(parsers, PhantomData), (), PhantomData)
}
impl<Ps: Clone, Os> Clone for ChainRight<Ps, Os> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}
impl<Ps: Copy, Os> Copy for ChainRight<Ps, Os> {}
macro_rules! chain_right_derive_fold {
    ($($ps:ident $pst:ident $ost:ident),*) => {
        chain_right_derive_fold!((),($($ps $pst $ost,)*));
    };
    (($($ps:ident $pst:ident $ost:ident,)*),()) => {};
    (($($ps1:ident $ps1t:ident $os1t:ident,)*), ($p:ident $pt:ident $ot:ident, $($ps2:ident $ps2t:ident $os2t:ident,)*)) => {
        chain_right_derive!($ot; $($ps1 $ps1t $os1t,)* $p $pt $ot; $($os1t),*);
        chain_right_derive_fold!(($($ps1 $ps1t $os1t,)* $p $pt $ot,), ($($ps2 $ps2t $os2t,)*));
    }
}
macro_rules! chain_right_derive {
    ($olt:ident; $($ps:ident $pst:ident $ost:ident),+; $($oet:ident),*) => {
        impl<I: Input, $($ost),+, E: ParseError<I>, C, S: Clone, $($pst: ParserOnce<I,$ost,E,C,S>),+> ParserOnce<I,$olt,E,C,S> for ChainRight<($($pst,)+),($($oet,)*)> {
            #[inline(always)]
            fn run_once(self, mut args: Args<I, E, C, S>) -> Option<$olt> {
                let ($($ps,)+) = self.0;
                Some({$($ps.run_once(args.by_ref())?);+})
            }
        }
        impl<I: Input, $($ost),+, E: ParseError<I>, C, S: Clone, $($pst: Parser<I,$ost,E,C,S>),+> Parser<I,$olt,E,C,S> for ChainRight<($($pst,)+),($($oet,)*)> {
            #[inline(always)]
            fn run(&mut self, mut args: Args<I, E, C, S>) -> Option<$olt> {
                let ($($ps,)+) = &mut self.0;
                Some({$($ps.run(args.by_ref())?);+})
            }
        }
    }
}

chain_right_derive_fold!(
    p1 P1 O1,
    p2 P2 O2,
    p3 P3 O3,
    p4 P4 O4,
    p5 P5 O5,
    p6 P6 O6,
    p7 P7 O7,
    p8 P8 O8,
    p9 P9 O9,
    p10 P10 O10,
    p11 P11 O11,
    p12 P12 O12,
    p13 P13 O13,
    p14 P14 O14,
    p15 P15 O15,
    p16 P16 O16,
    p17 P17 O17,
    p18 P18 O18,
    p19 P19 O19,
    p20 P20 O20,
    p21 P21 O21,
    p22 P22 O22,
    p23 P23 O23,
    p24 P24 O24,
    p25 P25 O25,
    p26 P26 O26,
    p27 P27 O27,
    p28 P28 O28,
    p29 P29 O29,
    p30 P30 O30
);

/**
Try several parsers in sequence, which may result in fewer input,state clones than or's chain.
# Example
```
use chasa::prelude::*;
let p = choice((str("123").to(1), str("456").to(2), str("789").to(3)));
assert_eq!(p.parse_ok("123"), Some(1));
assert_eq!(p.parse_ok("456"), Some(2));
```
*/
#[derive(Clone, Copy)]
pub struct Choice<PS>(PS);
#[inline]
pub fn choice<PS>(parsers: PS) -> Choice<PS> {
    Choice(parsers)
}
macro_rules! choice_derive_fold {
    ($p1:ident $p1t:ident,$($ps:ident $pst:ident),*) => {
        choice_derive_fold!(($p1 $p1t,),($($ps $pst,)*));
    };
    (($($ps:ident $pst:ident,)*),()) => {};
    (($($ps1:ident $ps1t:ident,)+), ($p:ident $pt:ident, $($ps2:ident $ps2t:ident,)*)) => {
        choice_derive!($($ps1 $ps1t),+);
        choice_derive_fold!(($($ps1 $ps1t,)+ $p $pt,), ($($ps2 $ps2t ,)*));
    }
}
macro_rules! choice_run {
    ($run:ident, $input:ident, $config:ident, $state:ident, $consume:ident, $error:ident, $p1:ident) => {
        $p1.$run(Args {
            input: $input, config: $config, state: $state, consume: $consume, error: $error
        })
    };
    ($run:ident, $input:ident, $config:ident, $state:ident, $consume:ident, $error:ident, $p1:ident, $($ps:ident),+) => {
        match $consume
            .cons(($input.clone(), $state.clone()), |consume|
                $p1.$run(Args { input: $input, config: $config, state: $state, consume, error: $error })
            )
        {
            (None, None) => None,
            (Some(o), _) => Some(o),
            (None, Some((input_bak, state_bak))) => {
                *$input = input_bak;
                *$state = state_bak;
                choice_run!($run,$input,$config,$state,$consume,$error,$($ps),+)
            },
        }
    }
}
macro_rules! choice_derive {
    ($($p:ident $t:ident),+) => {
        impl<I: Input, O, E: ParseError<I>, C, S: Clone, $($t: ParserOnce<I, O, E, C, S>),+> ParserOnce<I, O, E, C, S> for Choice<($($t,)+)> {
            #[inline(always)]
            fn run_once(self, args: Args<I, E, C, S>) -> Option<O> {
                let ($($p,)+) = self.0;
                #[allow(unused_mut)]
                let Args { input, config, state, consume, mut error } = args;
                choice_run!(run_once, input, config, state, consume, error, $($p),+)
            }
        }
        impl<I: Input, O, E: ParseError<I>, C, S: Clone, $($t: Parser<I, O, E, C, S>),+> Parser<I, O, E, C, S> for Choice<($($t,)+)> {
            #[inline(always)]
            fn run(&mut self, args: Args<I, E, C, S>) -> Option<O> {
                let ($($p,)+) = &mut self.0;
                #[allow(unused_mut)]
                let Args { input, config, state, consume, mut error } = args;
                choice_run!(run, input, config, state, consume, error, $($p),+)
            }
        }
    }
}
choice_derive_fold!(
    p1 P1,
    p2 P2,
    p3 P3,
    p4 P4,
    p5 P5,
    p6 P6,
    p7 P7,
    p8 P8,
    p9 P9,
    p10 P10,
    p11 P11,
    p12 P12,
    p13 P13,
    p14 P14,
    p15 P15,
    p16 P16,
    p17 P17,
    p18 P18,
    p19 P19,
    p20 P20,
    p21 P21,
    p22 P22,
    p23 P23,
    p24 P24,
    p25 P25,
    p26 P26,
    p27 P27,
    p28 P28,
    p29 P29,
    p30 P30
);
