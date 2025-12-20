use reborrow_generic::short::Rb;

use crate::{
    input::{
        In,
        error::{ErrMode, OutOf},
        inner::{Input, RbBack},
    },
    parser::{ErrOf, Parser, ParserMut, ParserOnce, ValueOf},
};

macro_rules! tuple_parser_impl {
    ($p1:ident : $P1:ident $(, $p:ident : $P:ident)* $(,)?) => {
        impl<I, E, N, L, $P1, $($P,)*> ParserOnce<I, E, N, L> for ($P1, $($P,)*)
        where
            I: Input,
            E: ErrMode<I>,
            N: Rb,
            L: RbBack,
            $P1: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
            $( $P: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<$P1, I, E, N, L>>>, )*
        {
            type Out = E::Out<(ValueOf<$P1, I, E, N, L>, $(ValueOf<$P, I, E, N, L>,)*), ErrOf<$P1, I, E, N, L>>;

            #[allow(unused_assignments, unused_variables)]
            fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
                let ($p1, $($p,)*) = self;
                let r1 = $p1.run_once(i.rb()).as_result();
                let r1 = match r1 {
                    Ok(v) => v,
                    Err(e) => return <$P1::Out as OutOf<I, E>>::embed_result(Err(e)),
                };
                $(
                    let r = $p.run_once(i.rb()).as_result();
                    let $p = match r {
                        Ok(v) => v,
                        Err(e) => return <$P1::Out as OutOf<I, E>>::embed_result(Err(e)),
                    };
                )*
                <$P1::Out as OutOf<I, E>>::embed_result(Ok((r1, $($p,)*)))
            }
        }

        impl<I, E, N, L, $P1, $($P,)*> ParserMut<I, E, N, L> for ($P1, $($P,)*)
        where
            I: Input,
            E: ErrMode<I>,
            N: Rb,
            L: RbBack,
            $P1: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
            $( $P: ParserMut<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<$P1, I, E, N, L>>>, )*
        {
            #[allow(unused_assignments, unused_variables)]
            fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
                let ($p1, $($p,)*) = self;
                let r1 = $p1.run_mut(i.rb()).as_result();
                let r1 = match r1 {
                    Ok(v) => v,
                    Err(e) => return <$P1::Out as OutOf<I, E>>::embed_result(Err(e)),
                };
                $(
                    let r = $p.run_mut(i.rb()).as_result();
                    let $p = match r {
                        Ok(v) => v,
                        Err(e) => return <$P1::Out as OutOf<I, E>>::embed_result(Err(e)),
                    };
                )*
                <$P1::Out as OutOf<I, E>>::embed_result(Ok((r1, $($p,)*)))
            }
        }

        impl<I, E, N, L, $P1, $($P,)*> Parser<I, E, N, L> for ($P1, $($P,)*)
        where
            I: Input,
            E: ErrMode<I>,
            N: Rb,
            L: RbBack,
            $P1: Parser<I, E, N, L, Out: OutOf<I, E>>,
            $( $P: Parser<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<$P1, I, E, N, L>>>, )*
        {
            #[allow(unused_assignments, unused_variables)]
            fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
                let ($p1, $($p,)*) = self;
                let r1 = $p1.run(i.rb()).as_result();
                let r1 = match r1 {
                    Ok(v) => v,
                    Err(e) => return <$P1::Out as OutOf<I, E>>::embed_result(Err(e)),
                };
                $(
                    let r = $p.run(i.rb()).as_result();
                    let $p = match r {
                        Ok(v) => v,
                        Err(e) => return <$P1::Out as OutOf<I, E>>::embed_result(Err(e)),
                    };
                )*
                <$P1::Out as OutOf<I, E>>::embed_result(Ok((r1, $($p,)*)))
            }
        }
    };
}
tuple_parser_impl!(p1: P1);
tuple_parser_impl!(p1: P1, p2: P2);
tuple_parser_impl!(p1: P1, p2: P2, p3: P3);
tuple_parser_impl!(p1: P1, p2: P2, p3: P3, p4: P4);
tuple_parser_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5);
tuple_parser_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6);
tuple_parser_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7);
tuple_parser_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8);
tuple_parser_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9);
tuple_parser_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10);
tuple_parser_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11);
tuple_parser_impl!(p1: P1, p2: P2, p3: P3, p4: P4, p5: P5, p6: P6, p7: P7, p8: P8, p9: P9, p10: P10, p11: P11, p12: P12);
