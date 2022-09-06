use super::{
    error::{self, ParseError},
    parser::{Parser, ParserOnce},
    prim::{char, no_state, satisfy, satisfy_map},
};

pub mod prelude {
    pub use super::{
        ascii, ascii_latin, ascii_latin_num, ascii_num, newline, no_break, no_break_ws, no_break_ws1, space, ws, ws1,
        Input,
    };

    #[doc(inline)]
    pub use super::super::combi::{
        before, chain, choice, extend_with_str, not_followed_by, pure_or, skip_chain, tuple,
    };
    #[doc(inline)]
    pub use super::super::error::{error, expected, format, message, token, unexpected, ParseError};
    #[doc(inline)]
    pub use super::super::fold::{fold, fold1, sep_extend, sep_extend1, sep_fold, sep_fold1, sep_reduce, tail_rec};
    #[doc(inline)]
    pub use super::super::input::pos_str;
    #[doc(inline)]
    pub use super::super::many::{many, many1, take};
    #[doc(inline)]
    pub use super::super::parser::{Parser, ParserOnce, Pat};
    #[doc(inline)]
    pub use super::super::prim::{
        any, char, config, eoi, local_state, no_state, none_of, one_of, parser, parser_once, pos, pure, satisfy,
        satisfy_map, satisfy_map_once, satisfy_once, set_config, state, str,
    };
    pub use super::super::util::{run, run_once};
}

pub trait Input: super::input::Input<Token = char> {}
impl<I: super::input::Input<Token = char>> Input for I {}

/// Accepts one uppercase and one lowercase Latin letter.
#[inline]
pub fn ascii_latin<I: Input, E: ParseError<I>, C, S: Clone>() -> impl Parser<I, char, E, C, S> {
    satisfy(|c: &char| matches!(c, 'a'..='z' | 'A'..='Z'))
}
/// One Indian Arabic numeral is accepted.
#[inline]
pub fn ascii_num<I: Input, E: ParseError<I>, C, S: Clone>() -> impl ParserOnce<I, char, E, C, S> {
    satisfy(|c: &char| matches!(c, '0'..='9'))
}
/// Accept upper and lower case Latin letters or one of the Indo-Arabic numerals.
#[inline]
pub fn ascii_latin_num<I: Input, E: ParseError<I>, C, S: Clone>() -> impl ParserOnce<I, char, E, C, S> {
    satisfy(|c: &char| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9'))
}
/// Accepts a single ASCII character.
#[inline]
pub fn ascii<I: Input, E: ParseError<I>, C, S: Clone>() -> impl ParserOnce<I, char, E, C, S> {
    satisfy(|c: &char| c < &'\u{128}')
}

pub enum IndentableSpace {
    Space,
    Tab,
}
#[derive(PartialEq, Eq)]
pub enum NLKind {
    CarriageReturn,
    LineFeed,
    Other,
} // 空白も返す、とかだとわからぬところがある
pub enum SpaceKind {
    NL(NLKind),
    Indentable(IndentableSpace),
    Other(char),
}
#[inline]
pub fn get_nl_kind(c: &char) -> Option<NLKind> {
    use std::cmp::Ordering::*;
    Some(match c.cmp(&'\u{000d}') {
        Equal => NLKind::CarriageReturn,
        Less => match c.cmp(&'\u{000a}') {
            Equal => NLKind::LineFeed,
            Less => None?,
            Greater => NLKind::Other,
        },
        Greater => match c.cmp(&'\u{0085}') {
            Equal => NLKind::Other,
            Less => None?,
            Greater => match c.cmp(&'\u{2028}') {
                Equal => NLKind::Other,
                Less => None?,
                Greater => {
                    if c == &'\u{2029}' {
                        NLKind::Other
                    } else {
                        None?
                    }
                },
            },
        },
    })
}
// none, [0009(tab), 000a(\r), 000b, 000c, 000d(\n)], none, 0020(space), none, 00a0, none, [0085],
// none,
// 2002..200b, none, [2028, 2029], none, 3000, none, feff, none
#[inline]
pub fn get_sp_kind(c: &char) -> Option<SpaceKind> {
    use std::cmp::Ordering::*;
    Some(match c.cmp(&'\u{0085}') {
        Equal => SpaceKind::NL(NLKind::Other),
        Less => match c.cmp(&'\u{0020}') {
            Equal => SpaceKind::Indentable(IndentableSpace::Space),
            Less => match c.cmp(&'\u{000d}') {
                Equal => SpaceKind::NL(NLKind::LineFeed),
                Greater => None?,
                Less => match c.cmp(&'\u{0009}') {
                    Equal => SpaceKind::Indentable(IndentableSpace::Tab),
                    Less => None?,
                    Greater => match c.cmp(&'\u{000b}') {
                        Less => SpaceKind::NL(NLKind::CarriageReturn),
                        _ => SpaceKind::NL(NLKind::Other),
                    },
                },
            },
            Greater => {
                if c == &'\u{00a0}' {
                    SpaceKind::Other(*c)
                } else {
                    None?
                }
            },
        },
        Greater => match c.cmp(&'\u{2002}') {
            Equal => SpaceKind::Other(*c),
            Less => None?,
            Greater => match c.cmp(&'\u{3000}') {
                Equal => SpaceKind::Other(*c),
                Less => match c.cmp(&'\u{2028}') {
                    Equal => SpaceKind::NL(NLKind::Other),
                    Less => match c.cmp(&'\u{200b}') {
                        Greater => None?,
                        _ => SpaceKind::Other(*c),
                    },
                    Greater => {
                        if c == &'\u{2029}' {
                            SpaceKind::NL(NLKind::Other)
                        } else {
                            None?
                        }
                    },
                },
                Greater => {
                    if c == &'\u{feff}' {
                        SpaceKind::Other(*c)
                    } else {
                        None?
                    }
                },
            },
        },
    })
}

#[inline(always)]
pub fn is_space(c: &char) -> bool {
    get_sp_kind(c).is_some()
}

/// Accepts a single newline character, but treats `"\r\n"` as one.
#[inline(always)]
pub fn newline<I: Input, E: ParseError<I>, S: Clone, C>() -> impl ParserOnce<I, (), E, C, S>
where
    E::Message: From<error::Unexpected<error::Token<char>>>
        + From<error::Expected<error::Token<char>>>
        + From<error::Expected<error::Format<&'static str>>>,
{
    satisfy_map(get_nl_kind)
        .case(|kind, k| match kind {
            NLKind::LineFeed | NLKind::Other => k.to(()),
            NLKind::CarriageReturn => k.then(char('\n').or_not().to(())),
        })
        .label_with(|| "newline")
}

/// A single whitespace character is accepted, excluding newline characters.
#[inline]
pub fn no_break<I: Input, E: ParseError<I>, S: Clone, C>() -> impl ParserOnce<I, (), E, C, S> {
    satisfy_map(|c: &char| match get_sp_kind(c)? {
        SpaceKind::Indentable(_) | SpaceKind::Other(_) => Some(()),
        _ => None,
    })
}

// Take one non-space character
#[inline]
pub fn no_ws<I: Input, E: ParseError<I>, S: Clone, C>() -> impl ParserOnce<I, char, E, C, S> {
    satisfy(|c: &char| get_sp_kind(c).is_none())
}

/// Accept any single space character.
#[inline]
pub fn space<I: Input, E: ParseError<I>, S: Clone, C>() -> impl ParserOnce<I, (), E, C, S> {
    satisfy_map(get_sp_kind).to(())
}

/// Greedily accepts a sequence of whitespace characters that do not contain a newline character.
#[inline]
pub fn no_break_ws<I: Input, E: ParseError<I>, S: Clone, C>() -> impl ParserOnce<I, (), E, C, S> {
    no_state(no_break.skip_many())
}

/// It greedily accepts a sequence of one or more whitespace characters, not including newline characters.
#[inline]
pub fn no_break_ws1<I: Input, E: ParseError<I>, S: Clone, C>() -> impl ParserOnce<I, (), E, C, S> {
    no_state(no_break.skip_many1())
}

/// A sequence of whitespace characters is greedily accepted.
#[inline(always)]
pub fn ws<I: Input, E: ParseError<I>, S: Clone, C>() -> impl ParserOnce<I, (), E, C, S> {
    no_state(satisfy_map(get_sp_kind).skip_many())
}
/// A sequence of one or more whitespace characters will be greedily accepted.
#[inline(always)]
pub fn ws1<I: Input, E: ParseError<I>, S: Clone, C>() -> impl ParserOnce<I, (), E, C, S> {
    no_state(satisfy_map(get_sp_kind).skip_many1())
}
