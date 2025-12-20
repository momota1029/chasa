//! Sequential (`then`) combinators for `OutOf` parsers.
//!
//! These combinators compose parsers *in order* on the same [`In`] stream.
//! They do **not** perform rollback by themselves.
//!
//! Rollback / backtracking is provided by higher-level combinators such as:
//!
//! - [`crate::parser::choice::Choice`] (tries next branch only on non-cut failure)
//! - [`crate::input::In::maybe`]
//!
//! ## Interaction with `cut`
//!
//! This crate treats `cut` as an explicit branch-pruning marker.
//! If you want a successful prefix to commit, wrap it with
//! [`crate::parser::ParserOnce::cut`] or insert [`crate::parser::prim::cut`].

mod macros;

use reborrow_generic::short::Rb;

use crate::{
    input::{
        In,
        error::{ErrMode, OutOf},
        inner::{Input, RbBack},
    },
    parser::{ErrOf, Parser, ParserMut, ParserOnce, ValueOf},
};

/// Shorthand for sequencing two parsers and returning both outputs.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let out = parse_ok_once(&mut input, then(item('a'), item('b'))).unwrap();
/// assert_eq!(out, ('a', 'b'));
/// assert_eq!(input, "");
/// ```
pub fn then<P, Q>(left: P, right: Q) -> (P, Q) {
    (left, right)
}

/// Run a tuple of parsers sequentially and return all outputs as a tuple.
///
/// Tuples short-circuit on the first error and return `E::Out<(A, B, ...), Err>` when each
/// parser returns an `OutOf` value.

impl<I: Input, E: ErrMode<I>, N: Rb, L: RbBack> ParserOnce<I, E, N, L> for () {
    type Out = E::Out<(), ()>;
    fn run_once(self, _input: In<I, E, N, L>) -> Self::Out {
        E::value(())
    }
}

impl<I: Input, E: ErrMode<I>, N: Rb, L: RbBack> ParserMut<I, E, N, L> for () {
    fn run_mut(&mut self, _input: In<I, E, N, L>) -> Self::Out {
        E::value(())
    }
}

impl<I: Input, E: ErrMode<I>, N: Rb, L: RbBack> Parser<I, E, N, L> for () {
    fn run(&self, _input: In<I, E, N, L>) -> Self::Out {
        E::value(())
    }
}

/// Keep the left output and discard the right output.
pub struct Left<P, Q>(P, Q);
/// Create a [`Left`] combinator.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let out = parse_ok_once(&mut input, left(item('a'), item('b'))).unwrap();
/// assert_eq!(out, 'a');
/// assert_eq!(input, "");
/// ```
pub fn left<P, Q>(p: P, q: Q) -> Left<P, Q> {
    Left(p, q)
}
impl<I, E, N, L, P, Q> ParserOnce<I, E, N, L> for Left<P, Q>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    Q: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    type Out = P::Out;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        P::Out::from_result(self.0.run_once(i.rb()).as_result().and_then(|o| self.1.run_once(i).as_result().map(|_| o)))
    }
}

impl<I, E, N, L, P, Q> ParserMut<I, E, N, L> for Left<P, Q>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        P::Out::from_result(self.0.run_mut(i.rb()).as_result().and_then(|o| self.1.run_mut(i).as_result().map(|_| o)))
    }
}

impl<I, E, N, L, P, Q> Parser<I, E, N, L> for Left<P, Q>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    Q: Parser<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        P::Out::from_result(self.0.run(i.rb()).as_result().and_then(|o| self.1.run(i).as_result().map(|_| o)))
    }
}

/// Parse `left`, then `inner`, then `right`, returning the `inner` output.
pub struct Between<Lp, P, Rp>(Lp, P, Rp);
/// Create a [`Between`] combinator.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "(a)";
/// let out = parse_ok_once(
///     &mut input,
///     between(item('('), item(')'), item('a')),
/// )
/// .unwrap();
/// assert_eq!(out, 'a');
/// assert_eq!(input, "");
/// ```
pub fn between<Lp, P, Rp>(left: Lp, right: Rp, inner: P) -> Between<Lp, P, Rp> {
    Between(left, inner, right)
}

impl<I, E, N, L, Lp, P, Rp> ParserOnce<I, E, N, L> for Between<Lp, P, Rp>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    Lp: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    Rp: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    type Out = P::Out;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        P::Out::from_result(
            self.0
                .run_once(i.rb())
                .as_result()
                .and_then(|_| self.1.run_once(i.rb()).as_result().and_then(|o| self.2.run_once(i).as_result().map(|_| o))),
        )
    }
}

impl<I, E, N, L, Lp, P, Rp> ParserMut<I, E, N, L> for Between<Lp, P, Rp>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    Lp: ParserMut<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    Rp: ParserMut<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        P::Out::from_result(
            self.0
                .run_mut(i.rb())
                .as_result()
                .and_then(|_| self.1.run_mut(i.rb()).as_result().and_then(|o| self.2.run_mut(i).as_result().map(|_| o))),
        )
    }
}

impl<I, E, N, L, Lp, P, Rp> Parser<I, E, N, L> for Between<Lp, P, Rp>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    Lp: Parser<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    Rp: Parser<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        P::Out::from_result(
            self.0.run(i.rb()).as_result().and_then(|_| self.1.run(i.rb()).as_result().and_then(|o| self.2.run(i).as_result().map(|_| o))),
        )
    }
}

/// Map the output of a parser.
pub struct Map<P, F>(P, F);
/// Create a [`Map`] combinator.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a";
/// let out = parse_ok_once(&mut input, map(item('a'), |c| if c == 'a' { 'A' } else { c })).unwrap();
/// assert_eq!(out, 'A');
/// assert_eq!(input, "");
/// ```
pub fn map<P, F>(parser: P, f: F) -> Map<P, F> {
    Map(parser, f)
}

impl<P, F, I, E, N, L, O> ParserOnce<I, E, N, L> for Map<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    F: FnOnce(ValueOf<P, I, E, N, L>) -> O,
{
    type Out = E::Out<O, ErrOf<P, I, E, N, L>>;
    fn run_once(self, i: In<I, E, N, L>) -> Self::Out {
        <P::Out as OutOf<I, E>>::embed_result(self.0.run_once(i).as_result().map(self.1))
    }
}

impl<P, F, I, E, N, L, O> ParserMut<I, E, N, L> for Map<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: FnMut(ValueOf<P, I, E, N, L>) -> O,
{
    fn run_mut(&mut self, i: In<I, E, N, L>) -> Self::Out {
        <P::Out as OutOf<I, E>>::embed_result(self.0.run_mut(i).as_result().map(&mut self.1))
    }
}

impl<P, F, I, E, N, L, O> Parser<I, E, N, L> for Map<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    F: Fn(ValueOf<P, I, E, N, L>) -> O,
{
    fn run(&self, i: In<I, E, N, L>) -> Self::Out {
        P::Out::embed_result(self.0.run(i).as_result().map(&self.1))
    }
}

/// Replace the output of a parser with a constant value.
pub struct To<P, O>(P, O);
/// Create a [`To`] combinator.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "a";
/// let out = parse_ok_once(&mut input, item('a').to(123)).unwrap();
/// assert_eq!(out, 123);
/// assert_eq!(input, "");
/// ```
pub fn to<P, O>(parser: P, value: O) -> To<P, O> {
    To(parser, value)
}

impl<P, O, I, E, N, L> ParserOnce<I, E, N, L> for To<P, O>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
{
    type Out = E::Out<O, ErrOf<P, I, E, N, L>>;
    fn run_once(self, i: In<I, E, N, L>) -> Self::Out {
        P::Out::embed_result(self.0.run_once(i).as_result().map(|_| self.1))
    }
}

impl<P, O, I, E, N, L> ParserMut<I, E, N, L> for To<P, O>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    O: Clone,
{
    fn run_mut(&mut self, i: In<I, E, N, L>) -> Self::Out {
        P::Out::embed_result(self.0.run_mut(i).as_result().map(|_| self.1.clone()))
    }
}

impl<P, O, I, E, N, L> Parser<I, E, N, L> for To<P, O>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    O: Clone,
{
    fn run(&self, i: In<I, E, N, L>) -> Self::Out {
        P::Out::embed_result(self.0.run(i).as_result().map(|_| self.1.clone()))
    }
}

/// Sequential bind (flat_map).
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let p = bind(item('a'), |_| item('b'));
/// let out = parse_ok_once(&mut input, p).unwrap();
/// assert_eq!(out, 'b');
/// assert_eq!(input, "");
/// ```
pub struct Bind<P, F>(P, F);
/// Create a sequential bind combinator.
pub fn bind<P, F>(parser: P, f: F) -> Bind<P, F> {
    Bind(parser, f)
}

impl<P, F, P2, I, E, N, L> ParserOnce<I, E, N, L> for Bind<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    F: FnOnce(ValueOf<P, I, E, N, L>) -> P2,
    P2: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    type Out = P2::Out;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        P2::Out::from_result(self.0.run_once(i.rb()).as_result().and_then(|v| self.1(v).run_once(i).as_result()))
    }
}

impl<P, F, P2, I, E, N, L> ParserMut<I, E, N, L> for Bind<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    F: FnMut(ValueOf<P, I, E, N, L>) -> P2,
    P2: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        P2::Out::from_result(self.0.run_mut(i.rb()).as_result().and_then(|v| (self.1)(v).run_once(i).as_result()))
    }
}

impl<P, F, P2, I, E, N, L> Parser<I, E, N, L> for Bind<P, F>
where
    I: Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    F: Fn(ValueOf<P, I, E, N, L>) -> P2,
    P2: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        P2::Out::from_result(self.0.run(i.rb()).as_result().and_then(|v| (self.1)(v).run_once(i).as_result()))
    }
}

/// Discard the left output and keep the right output.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "ab";
/// let out = parse_ok_once(&mut input, right(item('a'), item('b'))).unwrap();
/// assert_eq!(out, 'b');
/// assert_eq!(input, "");
/// ```
pub fn right<P, Q>(p: P, q: Q) -> Right<P, Q> {
    Right(p, q)
}

/// Parser that discards the left output and keeps the right output.
pub struct Right<P, Q>(P, Q);

#[cfg(test)]
mod tests {
    use crate::parser::{ParserOnce, item, prim, then as then_mod};

    #[test]
    fn tuple_sequences_two_item_parsers() {
        let mut input = "ab";
        let p = then_mod::then(item::item('a'), item::item('b')).cut();
        let r = crate::parse::parse_once::<_, crate::input::error::std::Unexpected<char>, _, _>(&mut input, p);
        assert_eq!(r.out, Some(('a', 'b')));
        assert_eq!(input, "");
        assert!(r.did_cut);
    }

    #[test]
    fn tuple_of_parsers_is_short_circuiting() {
        let mut input = "ab";
        let p = (item::item('a'), item::item('b'));
        let r = crate::parse::parse_once::<_, crate::input::error::std::Unexpected<char>, _, _>(&mut input, p);
        assert_eq!(r.out, Some(('a', 'b')));
        assert_eq!(input, "");
        assert!(!r.did_cut);
    }

    #[test]
    fn tuple_stops_after_first_err() {
        let mut input = "ab";
        let p = (item::item('x'), prim::cut);
        let r = crate::parse::parse_once::<_, crate::input::error::std::Unexpected<char>, _, _>(&mut input, p);
        assert_eq!(r.out, None);
        assert_eq!(r.did_cut, false);
    }
}

impl<I, E, N, L, P, Q> ParserOnce<I, E, N, L> for Right<P, Q>
where
    I: crate::Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    Q: ParserOnce<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    type Out = Q::Out;
    fn run_once(self, mut i: In<I, E, N, L>) -> Self::Out {
        Q::Out::from_result(self.0.run_once(i.rb()).as_result().and_then(|_| self.1.run_once(i).as_result()))
    }
}

impl<I, E, N, L, P, Q> ParserMut<I, E, N, L> for Right<P, Q>
where
    I: crate::Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: ParserMut<I, E, N, L, Out: OutOf<I, E>>,
    Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    fn run_mut(&mut self, mut i: In<I, E, N, L>) -> Self::Out {
        Q::Out::from_result(self.0.run_mut(i.rb()).as_result().and_then(|_| self.1.run_mut(i).as_result()))
    }
}

impl<I, E, N, L, P, Q> Parser<I, E, N, L> for Right<P, Q>
where
    I: crate::Input,
    E: ErrMode<I>,
    N: Rb,
    L: RbBack,
    P: Parser<I, E, N, L, Out: OutOf<I, E>>,
    Q: Parser<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
{
    fn run(&self, mut i: In<I, E, N, L>) -> Self::Out {
        Q::Out::from_result(self.0.run(i.rb()).as_result().and_then(|_| self.1.run(i).as_result()))
    }
}
