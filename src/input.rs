//! Input abstraction (playground core).
//!
//! This module is intentionally kept small so you can redesign the rest freely.

use std::ops::Range;

use reborrow_generic::{Reborrow, Reborrowed, short::Rb};

use crate::{
    input::{
        error::{
            ErrMode, ErrSink, Merger, OutOf,
            std::{StdErr, Unexpected, UnexpectedEndOfInput, UnexpectedItem},
        },
        inner::{Back, Input, RbBack},
        is_cut::IsCut,
    },
    parser::{self, ErrOf, ParserOnce, ValueOf, item, many, prim, sep},
    prelude::Expected,
};

pub mod error;
pub mod inner;
pub mod is_cut;
pub mod stream;

/// Input wrapper passed through parsers.
///
/// `In` bundles:
///
/// - the underlying input (`I`)
/// - an error accumulator (`E::Target`)
/// - a `cut` marker (`IsCut`)
/// - user-provided `env` and `local` states (reborrowed)
///
/// ## `cut` and `commit`
///
/// In this crate, `cut` is a control signal:
///
/// - It prevents backtracking across the cut point (branch pruning).
/// - When called in a **root** scope (`IsCut::is_root == true`), it also calls
///   [`Input::commit`] on the underlying input. This allows streaming inputs to discard
///   already-accepted prefixes.
///
/// Nested scopes (created by [`In::capture_cut`]) may or may not be considered root depending on
/// how they were created. See [`is_cut::IsCut`] for details.
#[derive(Reborrow)]
pub struct In<'a, I: Input, E: 'a + ErrMode<I> = Merger<I, StdErr<<I as Input>::Item>>, N: 'a + Reborrow = (), L: 'a + RbBack = ()> {
    pub input: &'a mut I,
    pub env: N::Target<'a>,
    pub local: L::Target<'a>,
    pub errors: E::Target<'a>,
    pub(crate) is_cut: IsCut<'a>,
}

/// Checkpoint for [`In`] (input, local, and error states).
pub struct InputCheckpoint<I: Input, E: ErrMode<I>, L: RbBack> {
    input: I::Checkpoint,
    local: L::Checkpoint,
    errors: E::Checkpoint,
}
impl<I: Input, E: ErrMode<I>, L: RbBack> Clone for InputCheckpoint<I, E, L> {
    fn clone(&self) -> Self {
        Self { input: self.input.clone(), local: self.local.clone(), errors: self.errors.clone() }
    }
}

impl<'a, I: Input, E: 'a + ErrMode<I>, N: 'a + Reborrow, L: 'a + RbBack> Back for In<'a, I, E, N, L> {
    type Checkpoint = InputCheckpoint<I, E, L>;
    fn checkpoint(&mut self) -> Self::Checkpoint {
        InputCheckpoint {
            input: self.input.checkpoint(),
            local: L::checkpoint(L::shorten_mut(&mut self.local)),
            errors: E::checkpoint(E::shorten_mut(&mut self.errors)),
        }
    }
    fn rollback(&mut self, checkpoint: Self::Checkpoint) {
        self.input.rollback(checkpoint.input);
        L::rollback(L::shorten_mut(&mut self.local), checkpoint.local);
        E::rollback(E::shorten_mut(&mut self.errors), checkpoint.errors);
    }
}
impl<'a, I: Input, E: 'a + ErrMode<I>> In<'a, I, E, (), ()> {
    /// Create a new `In` with empty `env`/`local`.
    pub fn new(input: &'a mut I, errors: E::Target<'a>, is_cut: IsCut<'a>) -> Self {
        Self { input, env: (), local: (), errors, is_cut }
    }
}
impl<'a, I: Input, E: 'a + ErrMode<I>, N: 'a + Reborrow, L: 'a + RbBack> In<'a, I, E, N, L> {
    /// Reborrow this input with a different environment value.
    pub fn set_env<'b, N2: Reborrowed<'b>>(&'b mut self, env: N2) -> In<'b, I, E, N2, L> {
        In {
            input: self.input,
            env,
            local: L::shorten_mut(&mut self.local),
            errors: E::shorten_mut(&mut self.errors),
            is_cut: self.is_cut.rb(),
        }
    }

    /// Reborrow this input with a different local value.
    pub fn set_local<'b, L2: Reborrowed<'b> + RbBack>(&'b mut self, local: L2) -> In<'b, I, E, N, L2> {
        In {
            input: self.input,
            env: N::shorten_mut(&mut self.env),
            local,
            errors: E::shorten_mut(&mut self.errors),
            is_cut: self.is_cut.rb(),
        }
    }

    /// Run a parser on this input.
    ///
    /// This is an ergonomic wrapper to avoid writing `parser.run_once(i)` at call sites.
    pub fn run<P: ParserOnce<I, E, N, L>>(&mut self, parser: P) -> P::Out {
        parser.run_once(self.rb())
    }

    /// Run a closure while capturing whether it performed a `cut`.
    ///
    /// Returns `(output, did_cut)`.
    ///
    /// This is the low-level primitive used by `choice`/`maybe`/`many` to decide whether a
    /// failure is "soft" (can backtrack) or "hard" (must not backtrack).
    pub fn capture_cut<O>(&mut self, f: impl FnOnce(In<I, E, N, L>) -> O) -> (O, bool) {
        self.is_cut.capture_cut(|is_cut| {
            f(In {
                input: &mut self.input,
                env: N::shorten_mut(&mut self.env),
                local: L::shorten_mut(&mut self.local),
                errors: E::shorten_mut(&mut self.errors),
                is_cut,
            })
        })
    }

    /// Try a parser with rollback-on-soft-failure semantics.
    ///
    /// - `Ok(v)` becomes `Ok(Some(v))`.
    /// - `Err(e)` **with** `cut` becomes `Err(e)` (hard failure).
    /// - `Err(e)` **without** `cut` rolls back and becomes `Ok(None)` (soft failure).
    pub fn maybe<P>(&mut self, parser: P) -> E::Out<Option<ValueOf<P, I, E, N, L>>, ErrOf<P, I, E, N, L>>
    where
        P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    {
        let checkpoint = self.checkpoint();
        let (out, did_cut) = self.capture_cut(|i| parser.run_once(i));
        E::map(out.into(), Some).recover(|e| {
            if did_cut {
                Err(e)
            } else {
                if E::rollback_on_soft_failure() {
                    self.rollback(checkpoint);
                }
                Ok(None)
            }
        })
    }

    /// Run a parser as lookahead.
    ///
    /// On success, input is rolled back. On failure, input may be consumed and errors are kept.
    pub fn lookahead<P>(&mut self, parser: P) -> P::Out
    where
        P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    {
        let checkpoint = self.checkpoint();
        match parser.run_once(self.rb()).as_result() {
            Err(e) => <P::Out>::from_result(Err(e)),
            Ok(o) => {
                self.rollback(checkpoint);
                <P::Out>::from_result(Ok(o))
            }
        }
    }

    /// Negative lookahead (succeeds only on soft failure).
    ///
    /// - On inner success: pushes `NotErr::Exists`.
    /// - On cut failure: pushes `NotErr::CutErr`.
    /// - On soft failure: rolls back and succeeds with `()`.
    pub fn not<P>(&mut self, parser: P) -> E::Out<(), prim::NotErr<ErrOf<P, I, E, N, L>>>
    where
        P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
        E: ErrSink<I, prim::NotErr<ErrOf<P, I, E, N, L>>>,
    {
        let (checkpoint, p0) = (self.checkpoint(), self.pos());
        let (o, is_cut) = self.capture_cut(|i| parser.run_once(i));
        match o.as_result() {
            Ok(_) => self.push_err(p0..self.pos(), prim::NotErr::Exists),
            Err(e) if is_cut => self.push_err(p0..self.pos(), prim::NotErr::CutErr(e)),
            Err(_) => {
                self.rollback(checkpoint);
                E::value(())
            }
        }
    }

    /// Run this parser in a non-root cut scope and then propagate the cut flag.
    pub fn no_cut<P>(&mut self, parser: P) -> P::Out
    where
        P: ParserOnce<I, E, N, L>,
    {
        let mut did_cut = false;
        let out = parser.run_once(In {
            input: &mut self.input,
            env: N::shorten_mut(&mut self.env),
            local: L::shorten_mut(&mut self.local),
            errors: E::shorten_mut(&mut self.errors),
            is_cut: IsCut::non_root(&mut did_cut),
        });
        if did_cut {
            self.cut();
        }
        out
    }

    /// Attach a Parsec-like expected label to a parser failure.
    pub fn label<P>(
        &mut self, parser: P, label: impl Into<std::borrow::Cow<'static, str>>,
    ) -> E::Out<ValueOf<P, I, E, N, L>, Expected<ErrOf<P, I, E, N, L>>>
    where
        P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
        E: ErrSink<I, Expected<ErrOf<P, I, E, N, L>>>,
    {
        let index = self.input.index();
        let out = parser.run_once(self.rb());
        match out.as_result() {
            Ok(o) => E::value(o),
            Err(e) => {
                let expected = Expected::new(index, label, e);
                match E::push_at_current(E::shorten_mut(&mut self.errors), expected) {
                    Ok(o) => o,
                    Err(e) => {
                        let (p0, p1) = (self.pos(), self.pos());
                        self.push_err(p0..p1, e)
                    }
                }
            }
        }
    }

    /// Attach a lazily-evaluated expected label to a parser failure.
    pub fn label_with<P, F, S>(&mut self, parser: P, f: F) -> E::Out<ValueOf<P, I, E, N, L>, Expected<ErrOf<P, I, E, N, L>>>
    where
        P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
        E: ErrSink<I, Expected<ErrOf<P, I, E, N, L>>>,
        F: FnOnce() -> S,
        S: Into<std::borrow::Cow<'static, str>>,
    {
        let index = self.input.index();
        let out = parser.run_once(self.rb());
        match out.as_result() {
            Ok(o) => E::value(o),
            Err(e) => {
                let label = f().into();
                let expected = Expected::new(index, label, e);
                match E::push_at_current(E::shorten_mut(&mut self.errors), expected) {
                    Ok(o) => o,
                    Err(e) => {
                        let (p0, p1) = (self.pos(), self.pos());
                        self.push_err(p0..p1, e)
                    }
                }
            }
        }
    }

    #[allow(unused_variables)]
    #[track_caller]
    /// Attach the consumed sequence to a parser output.
    pub fn with_seq<P, S>(&mut self, parser: P) -> E::Out<(ValueOf<P, I, E, N, L>, S), ErrOf<P, I, E, N, L>>
    where
        I: crate::input::inner::SeqInput<S>,
        P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    {
        let c0 = self.checkpoint().input;
        <P::Out>::embed_result(parser.run_once(self.rb()).as_result().map(|o| {
            let c1 = self.input.checkpoint();
            let seq = I::seq(c0, c1);
            (o, seq)
        }))
    }

    #[allow(unused_variables)]
    #[track_caller]
    /// Attach the consumed range (start and end positions) to a parser output.
    pub fn with_range<P>(&mut self, parser: P) -> E::Out<(ValueOf<P, I, E, N, L>, Range<I::Pos>), ErrOf<P, I, E, N, L>>
    where
        P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    {
        let p0 = self.pos();
        <P::Out>::embed_result(parser.run_once(self.rb()).as_result().map(|o| {
            let p1 = self.pos();
            (o, p0..p1)
        }))
    }

    // TODO: convert_err

    #[allow(unused_variables)]
    #[track_caller]
    /// Commit (cut) when this parser succeeds.
    pub fn cut_if_ok<P>(&mut self, parser: P) -> P::Out
    where
        P: ParserOnce<I, E, N, L, Out: OutOf<I, E>>,
    {
        let res = parser.run_once(self.rb());
        if res.is_ok() {
            self.cut();
        }
        res
    }

    /// Mark the current branch as committed (`cut`).
    pub fn cut(&mut self) {
        self.is_cut.cut();
        if self.is_cut.is_root {
            self.input.commit();
            E::commit(E::shorten_mut(&mut self.errors));
        }
    }

    /// Current input position.
    pub fn pos(&self) -> I::Pos {
        self.input.pos()
    }

    /// Push an error into the error sink.
    pub fn push_err<O, EI>(&mut self, range: Range<I::Pos>, err: EI) -> E::Out<O, EI>
    where
        E: ErrSink<I, EI>,
    {
        E::push(E::shorten_mut(&mut self.errors), range, err)
    }

    /// Ordered choice between parsers (runs immediately).
    pub fn choice<Ps>(&mut self, parsers: Ps) -> <parser::choice::Choice<Ps> as ParserOnce<I, E, N, L>>::Out
    where
        parser::choice::Choice<Ps>: ParserOnce<I, E, N, L>,
    {
        parser::choice::choice(parsers).run_once(self.rb())
    }

    /// Match an exact sequence.
    pub fn tag<S>(&mut self, expected: S) -> E::Out<(), Unexpected<I::Item>>
    where
        I: Input,
        E: ErrSink<I, Unexpected<I::Item>>,
        N: Rb,
        L: RbBack,
        S: crate::parser::seq::ItemSeq<I::Item>,
    {
        crate::parser::token::tag::<S, (I, E, N, L)>(expected).run_once(self.rb())
    }

    /// Expect end-of-input.
    pub fn eoi(&mut self) -> E::Out<(), UnexpectedItem<I::Item>>
    where
        I: Input,
        E: ErrSink<I, UnexpectedItem<I::Item>>,
        N: Rb,
        L: RbBack,
    {
        item::eoi(self.rb())
    }

    /// Consume any single item.
    pub fn any(&mut self) -> E::Out<I::Item, UnexpectedEndOfInput>
    where
        I: Input,
        E: ErrSink<I, UnexpectedEndOfInput>,
        N: Rb,
        L: RbBack,
    {
        item::any(self.rb())
    }

    /// Consume one item that satisfies a predicate.
    pub fn satisfy<F, It>(&mut self, f: F) -> E::Out<It, Unexpected<It>>
    where
        I: Input<Item = It>,
        E: ErrSink<I, Unexpected<It>>,
        N: Rb,
        L: RbBack,
        F: FnOnce(It) -> bool,
        It: Clone,
    {
        item::satisfy::<F, It, (I, E, N, L)>(f).run_once(self.rb())
    }

    /// Expect one exact item.
    pub fn item<T, It>(&mut self, expected: T) -> E::Out<It, Unexpected<It>>
    where
        I: Input<Item = It>,
        E: ErrSink<I, Unexpected<It>>,
        N: Rb,
        L: RbBack,
        T: PartialEq<It>,
        It: Clone,
    {
        item::item_of::<T, It, (I, E, N, L)>(expected).run_once(self.rb())
    }

    /// Expect one item from a set.
    pub fn one_of<S, It>(&mut self, items: S) -> E::Out<It, Unexpected<It>>
    where
        I: Input<Item = It>,
        E: ErrSink<I, Unexpected<It>>,
        N: Rb,
        L: RbBack,
        S: item::set::ItemSet<It>,
    {
        item::one_of::<S, It, (I, E, N, L)>(items).run_once(self.rb())
    }

    /// Expect one item not in a set.
    pub fn none_of<S, It>(&mut self, items: S) -> E::Out<It, Unexpected<It>>
    where
        I: Input<Item = It>,
        E: ErrSink<I, Unexpected<It>>,
        N: Rb,
        L: RbBack,
        S: item::set::ItemSet<It>,
    {
        item::none_of::<S, It, (I, E, N, L)>(items).run_once(self.rb())
    }

    /// Consume zero or more whitespace characters (char inputs).
    pub fn ws(&mut self) -> E::Out<(), <E::Out<char, Unexpected<char>> as OutOf<I, E>>::Error>
    where
        I: Input<Item = char>,
        E: ErrMode<I> + ErrSink<I, Unexpected<char>>,
        E::Out<char, Unexpected<char>>: OutOf<I, E>,
        N: Rb,
        L: RbBack,
    {
        many::many_skip(item::satisfy::<_, char, (I, E, N, L)>(|c: char| c.is_whitespace())).run_once(self.rb())
    }

    /// Consume one or more whitespace characters (char inputs).
    pub fn ws1(&mut self) -> E::Out<(), <E::Out<char, Unexpected<char>> as OutOf<I, E>>::Error>
    where
        I: Input<Item = char>,
        E: ErrMode<I> + ErrSink<I, Unexpected<char>>,
        E::Out<char, Unexpected<char>>: OutOf<I, E>,
        N: Rb,
        L: RbBack,
    {
        many::many1_skip(item::satisfy::<_, char, (I, E, N, L)>(|c: char| c.is_whitespace())).run_once(self.rb())
    }

    /// Repeat a parser until soft failure, collecting outputs.
    pub fn many<P, O>(&mut self, parser: P) -> <many::Many<P, O> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        O: FromIterator<ValueOf<P, I, E, N, L>>,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<O, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        many::many(parser).run_once(self.rb())
    }

    /// Repeat a parser one or more times, collecting outputs.
    pub fn many1<P, O>(&mut self, parser: P) -> <many::Many1<P, O> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        O: FromIterator<ValueOf<P, I, E, N, L>>,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<O, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        many::many1(parser).run_once(self.rb())
    }

    /// Repeat a parser until soft failure, discarding outputs.
    pub fn many_skip<P>(&mut self, parser: P) -> <many::SkipMany<P> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        E: ErrMode<I>,
        N: Rb,
        L: RbBack,
    {
        many::many_skip(parser).run_once(self.rb())
    }

    /// Repeat a parser one or more times, discarding outputs.
    pub fn many1_skip<P>(&mut self, parser: P) -> <many::SkipMany1<P> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        E: ErrMode<I>,
        N: Rb,
        L: RbBack,
    {
        many::many1_skip(parser).run_once(self.rb())
    }

    /// Repeat a parser a specified number of times.
    pub fn count<R, P, O>(&mut self, range: R, parser: P) -> <many::Count<P, O> as ParserOnce<I, E, N, L>>::Out
    where
        R: many::CountRange,
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E>>,
        O: FromIterator<ValueOf<P, I, E, N, L>>,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<O, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        many::count(range, parser).run_once(self.rb())
    }

    /// Parse a separated list: `item (sep item)*`.
    pub fn sep<O, P, Q, T>(
        &mut self, item: P, sep_p: Q,
    ) -> <sep::Sep<O, sep::iter::Zero, sep::iter::Allow, P, Q, T> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
        O: FromIterator<T>,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<O, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        sep::sep(item, sep_p).run_once(self.rb())
    }

    /// Like [`In::sep`], but requires at least one item (`1+`).
    pub fn sep1<O, P, Q, T>(
        &mut self, item: P, sep_p: Q,
    ) -> <sep::Sep<O, sep::iter::One, sep::iter::Allow, P, Q, T> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
        O: FromIterator<T>,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<O, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        sep::sep1(item, sep_p).run_once(self.rb())
    }

    /// Like [`In::sep`], but lets you fold via a streaming iterator.
    pub fn sep_map<P, Q, F, T, S, O>(
        &mut self, item: P, sep_p: Q, f: F,
    ) -> <sep::SepMap<sep::iter::Zero, sep::iter::Allow, P, Q, F, T> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
        F: for<'p, 'q> FnOnce(sep::SepMapIterator<'p, 'q, P, Q, I, E, N, L, sep::iter::Zero, sep::iter::Allow>) -> O,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<O, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        sep::sep_map(item, sep_p, f).run_once(self.rb())
    }

    /// Like [`In::sep_map`], but requires at least one item (`1+`).
    pub fn sep1_map<P, Q, F, T, S, O>(
        &mut self, item: P, sep_p: Q, f: F,
    ) -> <sep::SepMap<sep::iter::One, sep::iter::Allow, P, Q, F, T> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
        F: for<'p, 'q> FnOnce(sep::SepMapIterator<'p, 'q, P, Q, I, E, N, L, sep::iter::One, sep::iter::Allow>) -> O,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<O, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        sep::sep1_map(item, sep_p, f).run_once(self.rb())
    }

    /// Like [`In::sep_map`], but intended for single-shot mappers (`FnOnce`).
    pub fn sep_map_once<P, Q, F, T, S, O>(
        &mut self, item: P, sep_p: Q, f: F,
    ) -> <sep::SepMap<sep::iter::Zero, sep::iter::Allow, P, Q, F, T> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
        F: for<'p, 'q> FnOnce(sep::SepMapIterator<'p, 'q, P, Q, I, E, N, L, sep::iter::Zero, sep::iter::Allow>) -> O,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<O, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        sep::sep_map_once(item, sep_p, f).run_once(self.rb())
    }

    /// Like [`In::sep_map`], but intended for mutable/repeatable mappers (`FnMut`).
    pub fn sep_map_mut<P, Q, F, T, S, O>(
        &mut self, item: P, sep_p: Q, f: F,
    ) -> <sep::SepMap<sep::iter::Zero, sep::iter::Allow, P, Q, F, T> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
        F: for<'p, 'q> FnMut(sep::SepMapIterator<'p, 'q, P, Q, I, E, N, L, sep::iter::Zero, sep::iter::Allow>) -> O,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<O, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        sep::sep_map_mut(item, sep_p, f).run_once(self.rb())
    }

    /// Left-associative separator reduction: `term (op term)*`.
    pub fn sep_reduce<P, Q, F, T, Op>(&mut self, term: P, op: Q, f: F) -> <sep::SepReduce<P, Q, F> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = Op, Error = ErrOf<P, I, E, N, L>>>,
        F: FnMut(T, Op, T) -> T,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<Option<T>, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        sep::sep_reduce(term, op, f).run_once(self.rb())
    }

    /// Like [`In::sep`], but returns `(out, did_trail)` to indicate a trailing separator.
    pub fn sep_use_trail<O, P, Q, T>(
        &mut self, item: P, sep_p: Q,
    ) -> <sep::UseTrailSep<O, sep::iter::Zero, sep::iter::Allow, P, Q, T> as ParserOnce<I, E, N, L>>::Out
    where
        P: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: parser::ParserMut<I, E, N, L, Out: OutOf<I, E, Error = ErrOf<P, I, E, N, L>>>,
        O: FromIterator<T>,
        E: ErrMode<I> + crate::input::error::ErrSink<I, many::ManyErr<O, ErrOf<P, I, E, N, L>>>,
        N: Rb,
        L: RbBack,
    {
        sep::sep(item, sep_p).use_trail().run_once(self.rb())
    }
}
