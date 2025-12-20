use reborrow_generic::short::Rb;

use crate::{
    Input,
    input::{
        In,
        error::{ErrMode, OutOf},
        inner::{Back, RbBack},
    },
    parser::{Parser, ParserMut, ParserOnce},
};

/// Ordered choice between parsers.
///
/// This tries each parser in order:
///
/// - If a branch succeeds, the choice succeeds and stops.
/// - If a branch fails **without** `cut`, input is rolled back and the next branch is tried.
/// - If a branch fails **with** `cut`, the choice stops immediately with that error.
///
/// If all branches fail softly, the input is rolled back to the **most advanced** failure position,
/// and the error from that branch is returned.
pub struct Choice<Ps>(Ps);
impl<Ps> Choice<Ps> {
    /// Create a new choice from a tuple of parsers.
    pub fn new(parsers: Ps) -> Self {
        Self(parsers)
    }
}

/// Ordered choice between parsers.
///
/// This is a convenience wrapper for [`Choice::new`].
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "b";
/// let out = parse_ok_once(&mut input, choice((item('a'), item('b')))).unwrap();
/// assert_eq!(out, 'b');
/// assert_eq!(input, "");
/// ```
pub fn choice<Ps>(parsers: Ps) -> Choice<Ps> {
    Choice::new(parsers)
}

macro_rules! choice_fn {
    ($run:ident, $base:ident, $input:ident, $best:ident, $p1:expr, $($p:expr),+ $(,)?) => {{
        let (out, did_cut) = $input.capture_cut(|input| $p1.$run(input));
        match out.as_result() {
            Ok(v) => {
                return <_ as OutOf<I, E>>::from_result(Ok(v));
            }
            Err(e) => {
                if did_cut {
                    return <_ as OutOf<I, E>>::from_result(Err(e));
                }
                let end_index = $input.input.index();
                let end_checkpoint = $input.checkpoint();
                let replace = match &$best {
                    None => true,
                    Some((best_index, _, _)) => end_index >= *best_index,
                };
                if replace {
                    $best = Some((end_index, end_checkpoint, e));
                }
                $input.rollback($base.clone());
                choice_fn!($run, $base, $input, $best, $($p),+)
            }
        }
    }};
    ($run:ident, $base:ident, $input:ident, $best:ident, $p1:expr $(,)?) => {{
        let (out, did_cut) = $input.capture_cut(|input| $p1.$run(input));
        match out.as_result() {
            Ok(v) => <_ as OutOf<I, E>>::from_result(Ok(v)),
            Err(e) => {
                if did_cut {
                    <_ as OutOf<I, E>>::from_result(Err(e))
                } else {
                    let end_index = $input.input.index();
                    let end_checkpoint = $input.checkpoint();
                    let replace = match &$best {
                        None => true,
                        Some((best_index, _, _)) => end_index >= *best_index,
                    };
                    if replace {
                        $best = Some((end_index, end_checkpoint, e));
                        let (_, best_checkpoint, best_err) = $best.take().expect("best is set");
                        $input.rollback(best_checkpoint);
                        <_ as OutOf<I, E>>::from_result(Err(best_err))
                    } else if let Some((_, best_checkpoint, best_err)) = $best.take() {
                        $input.rollback(best_checkpoint);
                        <_ as OutOf<I, E>>::from_result(Err(best_err))
                    } else {
                        <_ as OutOf<I, E>>::from_result(Err(e))
                    }
                }
            }
        }
    }};
}

macro_rules! choice_impl {
    ($p1:ident : $P1:ident $(, $p2:ident : $P2:ident)* $(,)?) => {
        impl<$P1, $($P2,)* I: Input, E: ErrMode<I>, N: Rb, L: RbBack> ParserOnce<I, E, N, L> for Choice<($P1, $($P2,)*)>
        where
            $P1: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
            $($P2: ParserOnce<I, E, N, L, Out = $P1::Out>,)*
        {
            type Out = $P1::Out;
            fn run_once(self, mut input: In<I, E, N, L>) -> Self::Out {
                let ($p1, $($p2,)*) = self.0;
                let checkpoint = input.checkpoint();
                let mut best = None;
                choice_fn!(run_once, checkpoint, input, best, $p1, $($p2,)*)
            }
        }

        impl<$P1, $($P2,)* I: Input, E: ErrMode<I>, N: Rb, L: RbBack> ParserMut<I, E, N, L> for Choice<($P1, $($P2,)*)>
        where
            $P1: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
            $($P2: ParserMut<I, E, N, L, Out = $P1::Out>,)*
        {
            fn run_mut(&mut self, mut input: In<I, E, N, L>) -> Self::Out {
                let ($p1, $($p2,)*) = &mut self.0;
                let checkpoint = input.checkpoint();
                let mut best = None;
                choice_fn!(run_mut, checkpoint, input, best, $p1, $($p2,)*)
            }
        }

        impl<$P1, $($P2,)* I: Input, E: ErrMode<I>, N: Rb, L: RbBack> Parser<I, E, N, L> for Choice<($P1, $($P2,)*)>
        where
            $P1: Parser<I, E, N, L, Out: OutOf<I, E>>,
            $($P2: Parser<I, E, N, L, Out = $P1::Out>,)*
        {
            fn run(&self, mut input: In<I, E, N, L>) -> Self::Out {
                let ($p1, $($p2,)*) = &self.0;
                let checkpoint = input.checkpoint();
                let mut best = None;
                choice_fn!(run, checkpoint, input, best, $p1, $($p2,)*)
            }
        }
    };
}

choice_impl!(p1: P1, p2: P2);
choice_impl!(p1: P1, p2: P2, p3: P3);
choice_impl!(p1: P1, p2: P2, p3: P3, p4: P4);
choice_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5);
choice_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6);
choice_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7);
choice_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8);
