//! Mode types for separated-list iteration.

use reborrow_generic::short::Rb;

use crate::{
    Input,
    input::In,
    input::error::ErrMode,
    input::error::OutOf,
    input::inner::{Back as _, RbBack},
    parser::{ErrOf, ParserMut},
};

/// Minimum count mode for the first item.
pub trait Count {
    fn first<I, E, N, L, P, T>(input: &mut In<I, E, N, L>, item: &mut P) -> Result<Option<T>, ErrOf<P, I, E, N, L>>
    where
        I: Input,
        E: ErrMode<I>,
        N: Rb,
        L: RbBack,
        P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>;
}

/// `0+` items.
#[derive(Debug, Clone, Copy, Hash, Default)]
pub struct Zero;
impl Count for Zero {
    fn first<I, E, N, L, P, T>(input: &mut In<I, E, N, L>, item: &mut P) -> Result<Option<T>, ErrOf<P, I, E, N, L>>
    where
        I: Input,
        E: ErrMode<I>,
        N: Rb,
        L: RbBack,
        P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    {
        match <P::Out as OutOf<I, E>>::project_result(input.maybe(item.by_mut())) {
            Ok(Some(item)) => Ok(Some(item)),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }
}

/// `1+` items.
#[derive(Debug, Clone, Copy, Hash, Default)]
pub struct One;
impl Count for One {
    fn first<I, E, N, L, P, T>(input: &mut In<I, E, N, L>, item: &mut P) -> Result<Option<T>, ErrOf<P, I, E, N, L>>
    where
        I: Input,
        E: ErrMode<I>,
        N: Rb,
        L: RbBack,
        P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
    {
        let checkpoint = input.checkpoint();
        match item.run_mut(input.rb()).as_result() {
            Ok(item) => Ok(Some(item)),
            Err(e) => {
                input.rollback(checkpoint);
                Err(e)
            }
        }
    }
}

/// Trailing-separator mode for the remaining items.
pub trait Trailling {
    fn validate_end(did_trail: bool) -> bool;

    fn second<I, E, N, L, P, Q, T, S>(
        input: &mut In<I, E, N, L>, sep: &mut Q, item: &mut P, did_trail: &mut bool,
    ) -> Result<Option<T>, ErrOf<P, I, E, N, L>>
    where
        I: Input,
        E: ErrMode<I>,
        N: Rb,
        L: RbBack,
        P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>;
}

/// Allow a trailing separator (and consume it if present).
#[derive(Debug, Clone, Copy, Hash, Default)]
pub struct Allow;
impl Trailling for Allow {
    fn validate_end(_: bool) -> bool {
        true
    }

    fn second<I, E, N, L, P, Q, T, S>(
        input: &mut In<I, E, N, L>, sep: &mut Q, item: &mut P, did_trail: &mut bool,
    ) -> Result<Option<T>, ErrOf<P, I, E, N, L>>
    where
        I: Input,
        E: ErrMode<I>,
        N: Rb,
        L: RbBack,
        P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    {
        match <Q::Out as OutOf<I, E>>::project_result(input.maybe(sep.by_mut())) {
            Ok(Some(_)) => match <P::Out as OutOf<I, E>>::project_result(input.maybe(item.by_mut())) {
                Ok(Some(item)) => Ok(Some(item)),
                Ok(None) => {
                    *did_trail = true;
                    Ok(None)
                }
                Err(e) => Err(e),
            },
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }
}

/// Disallow a trailing separator (separator must be followed by an item).
#[derive(Debug, Clone, Copy, Hash, Default)]
pub struct No;
impl Trailling for No {
    fn validate_end(_: bool) -> bool {
        true
    }

    fn second<I, E, N, L, P, Q, T, S>(
        input: &mut In<I, E, N, L>, sep: &mut Q, item: &mut P, _did_trail: &mut bool,
    ) -> Result<Option<T>, ErrOf<P, I, E, N, L>>
    where
        I: Input,
        E: ErrMode<I>,
        N: Rb,
        L: RbBack,
        P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    {
        match <Q::Out as OutOf<I, E>>::project_result(input.maybe(sep.by_mut())) {
            Ok(Some(_)) => item.run_mut(input.rb()).as_result().map(Some),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }
}

/// Require a trailing separator.
#[derive(Debug, Clone, Copy, Hash, Default)]
pub struct Must;
impl Trailling for Must {
    fn validate_end(did_trail: bool) -> bool {
        did_trail
    }

    fn second<I, E, N, L, P, Q, T, S>(
        input: &mut In<I, E, N, L>, sep: &mut Q, item: &mut P, did_trail: &mut bool,
    ) -> Result<Option<T>, ErrOf<P, I, E, N, L>>
    where
        I: Input,
        E: ErrMode<I>,
        N: Rb,
        L: RbBack,
        P: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = T>>,
        Q: ParserMut<I, E, N, L, Out: OutOf<I, E, Value = S, Error = ErrOf<P, I, E, N, L>>>,
    {
        let checkpoint = input.checkpoint();
        match sep.run_mut(input.rb()).as_result() {
            Ok(_) => {}
            Err(e) => {
                input.rollback(checkpoint);
                return Err(e);
            }
        }
        match <P::Out as OutOf<I, E>>::project_result(input.maybe(item.by_mut())) {
            Ok(Some(item)) => Ok(Some(item)),
            Ok(None) => {
                *did_trail = true;
                Ok(None)
            }
            Err(e) => Err(e),
        }
    }
}
