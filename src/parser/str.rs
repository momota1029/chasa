//! String-oriented parsers for `char` inputs.
//!
//! This module is intentionally small: it provides a `tag` parser to match a fixed string.
//! Internally this is implemented by the unified token tag parser.

/// Match an exact string.
///
/// - On success: consumes the whole tag and returns `()`.
/// - On failure: pushes an `Unexpected` error at the mismatch location.
///
/// This does not cut by itself.
///
/// ## Examples
///
/// ```
/// use chasa::prelude::*;
///
/// let mut input = "hello!";
/// let out = parse_ok_once(&mut input, tag("hello")).unwrap();
/// assert_eq!(out, ());
/// assert_eq!(input, "!");
/// ```
pub fn tag<A>(tag: &'static str) -> Tag<A> {
    crate::parser::token::tag(tag)
}

pub type Tag<A> = crate::parser::token::Tag<&'static str, A>;
