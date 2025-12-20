//! `char`-level convenience parsers and sets.
//!
//! This module is intended for inputs where `I::Item = char` (e.g. `&str`).
//!
//! The building blocks are:
//!
//! - predicate sets like [`SPACE`], [`ASCII_ALPHA`], ...
//! - single-character parsers like [`space`], [`ascii_digit`], ...
//! - common whitespace consumers [`ws`] / [`ws1`]
//!
//! ## Mismatch and rollback
//!
//! These helpers are built on [`crate::parser::item::one_of`]. As a result:
//!
//! - On mismatch, they return failure and record an "unexpected" error in the input's merger.
//! - They may consume one character while determining the mismatch.
//!
//! If you need *non-consuming* failure, run them under a rollback-providing combinator such as
//! [`crate::input::In::maybe`], [`crate::parser::choice::Choice`], or
//! [`crate::parser::prim::lookahead`].

use reborrow_generic::short::Rb;

use crate::{
    input::{
        In,
        error::std::Unexpected,
        error::{ErrSink, OutOf},
        inner::{Input, RbBack},
    },
    parser::{ParserOnce as _, item},
};

/// `char::is_whitespace` as an [`item::set::ItemSet`] predicate.
pub const SPACE: fn(char) -> bool = is_space;
fn is_space(c: char) -> bool {
    c.is_whitespace()
}

/// Parse one whitespace character.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = " \t";
/// let out = parse_ok_once(&mut input, space).unwrap();
/// assert_eq!(out, ' ');
/// assert_eq!(input, "\t");
/// ```
pub fn space<I, E, N, L>(i: In<I, E, N, L>) -> E::Out<char, Unexpected<char>>
where
    I: Input<Item = char>,
    E: ErrSink<I, Unexpected<char>>,
    N: Rb,
    L: RbBack,
{
    item::one_of(SPACE).run_once(i)
}

/// Consume zero or more whitespace characters.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = " \t\na";
/// let out = parse_ok_once(&mut input, ws).unwrap();
/// assert_eq!(out, ());
/// assert_eq!(input, "a");
/// ```
pub fn ws<I, E, N, L>(mut i: In<I, E, N, L>) -> E::Out<(), Unexpected<char>>
where
    I: Input<Item = char>,
    E: ErrSink<I, Unexpected<char>>,
    N: Rb,
    L: RbBack,
{
    E::retag::<(), char, Unexpected<char>>(<E::Out<char, Unexpected<char>> as OutOf<I, E>>::project_result(
        i.many_skip(item::one_of(SPACE)),
    ))
}

/// Consume one or more whitespace characters.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = " \t\na";
/// let out = parse_ok_once(&mut input, ws1).unwrap();
/// assert_eq!(out, ());
/// assert_eq!(input, "a");
/// ```
pub fn ws1<I, E, N, L>(mut i: In<I, E, N, L>) -> E::Out<(), Unexpected<char>>
where
    I: Input<Item = char>,
    E: ErrSink<I, Unexpected<char>>,
    N: Rb,
    L: RbBack,
{
    E::retag::<_, _, Unexpected<char>>(<E::Out<_, Unexpected<char>>>::project_result(i.many1_skip(item::one_of(SPACE))))
}

/// `char::is_ascii` as an [`item::set::ItemSet`] predicate.
pub const ASCII: fn(char) -> bool = is_ascii;
fn is_ascii(c: char) -> bool {
    c.is_ascii()
}

/// Parse one ASCII character.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "aβ";
/// let out = parse_ok_once(&mut input, ascii).unwrap();
/// assert_eq!(out, 'a');
/// assert_eq!(input, "β");
/// ```
pub fn ascii<I, E, N, L>(i: In<I, E, N, L>) -> E::Out<char, Unexpected<char>>
where
    I: Input<Item = char>,
    E: ErrSink<I, Unexpected<char>>,
    N: Rb,
    L: RbBack,
{
    item::one_of(ASCII).run_once(i)
}

/// `char::is_ascii_alphabetic` as an [`item::set::ItemSet`] predicate.
pub const ASCII_ALPHA: fn(char) -> bool = is_ascii_alpha;
fn is_ascii_alpha(c: char) -> bool {
    c.is_ascii_alphabetic()
}

/// Parse one ASCII alphabetic character.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "Z9";
/// let out = parse_ok_once(&mut input, ascii_alpha).unwrap();
/// assert_eq!(out, 'Z');
/// assert_eq!(input, "9");
/// ```
pub fn ascii_alpha<I, E, N, L>(i: In<I, E, N, L>) -> E::Out<char, Unexpected<char>>
where
    I: Input<Item = char>,
    E: ErrSink<I, Unexpected<char>>,
    N: Rb,
    L: RbBack,
{
    item::one_of(ASCII_ALPHA).run_once(i)
}

/// `char::is_ascii_digit` as an [`item::set::ItemSet`] predicate.
pub const ASCII_DIGIT: fn(char) -> bool = is_ascii_digit;
fn is_ascii_digit(c: char) -> bool {
    c.is_ascii_digit()
}

/// Parse one ASCII digit.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "7a";
/// let out = parse_ok_once(&mut input, ascii_digit).unwrap();
/// assert_eq!(out, '7');
/// assert_eq!(input, "a");
/// ```
pub fn ascii_digit<I, E, N, L>(i: In<I, E, N, L>) -> E::Out<char, Unexpected<char>>
where
    I: Input<Item = char>,
    E: ErrSink<I, Unexpected<char>>,
    N: Rb,
    L: RbBack,
{
    item::one_of(ASCII_DIGIT).run_once(i)
}

/// `char::is_ascii_alphanumeric` as an [`item::set::ItemSet`] predicate.
pub const ASCII_ALPHANUM: fn(char) -> bool = is_ascii_alphanumeric;
fn is_ascii_alphanumeric(c: char) -> bool {
    c.is_ascii_alphanumeric()
}

/// Parse one ASCII alphanumeric character.
pub fn ascii_alphanumeric<I, E, N, L>(i: In<I, E, N, L>) -> E::Out<char, Unexpected<char>>
where
    I: Input<Item = char>,
    E: ErrSink<I, Unexpected<char>>,
    N: Rb,
    L: RbBack,
{
    item::one_of(ASCII_ALPHANUM).run_once(i)
}
