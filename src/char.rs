use std::fmt::{self, Display};

use crate::{
    error::{CustomBuilder as Cb, Nil},
    fold::tail_rec,
    input::Counter,
    prim::{char, no_state, satisfy, satisfy_map},
    parser::{Parser, ParserOnce},
};

pub mod prelude {
    pub use crate::char::{
        ascii, ascii_latin, ascii_latin_num, ascii_num, newline, nls, no_break, no_break_ws, no_break_ws1, space, ws,
        ws1, Input,
    };
    pub use crate::combi::{before, chain, choice, extend_with_str, not_followed_by, pure_or, skip_chain, tuple};
    pub use crate::error::{message, message_with, unexpect, unexpect_with};
    pub use crate::fold::{fold, fold1, sep_fold, sep_fold1, tail_rec};
    pub use crate::many::{many, many1, take};
    pub use crate::prim::{
        any, char, config, eoi, fail, get_state, local_state, no_state, none_of, one_of, parser, parser_mv, pos, pure,
        satisfy, satisfy_map, set_config, set_state, state, str,
    };
    pub use crate::parser::{Parser, ParserOnce, Pat, PatMv, SimpleParser};
    pub use crate::util::{run, run_mv};
}

pub trait Input: crate::input::Input<Item = char> {}
impl<I: crate::input::Input<Item = char>> Input for I {}

/// Accepts one uppercase and one lowercase Latin letter.
#[inline]
pub fn ascii_latin<I: Input, C, S, M: Cb>() -> impl ParserOnce<I, char, C, S, M> {
    satisfy(|c: &char| matches!(c, 'a'..='z' | 'A'..='Z'))
}
/// One Indian Arabic numeral is accepted.
#[inline]
pub fn ascii_num<I: Input, C, S, M: Cb>() -> impl ParserOnce<I, char, C, S, M> {
    satisfy(|c: &char| matches!(c, '0'..='9'))
}
/// Accept upper and lower case Latin letters or one of the Indo-Arabic numerals.
#[inline]
pub fn ascii_latin_num<I: Input, C, S, M: Cb>() -> impl ParserOnce<I, char, C, S, M> {
    satisfy(|c: &char| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9'))
}
/// Accepts a single ASCII character.
#[inline]
pub fn ascii<I: Input, C, S, M: Cb>() -> impl ParserOnce<I, char, C, S, M> {
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
} // ????????????????????????????????????????????????????????????
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
pub fn newline<I: Input, S, C, M: Cb>() -> impl ParserOnce<I, (), C, S, M> {
    no_state(satisfy_map(get_nl_kind).case(|kind, k| match kind {
        NLKind::LineFeed | NLKind::Other => k.to(()),
        NLKind::CarriageReturn => k.then(char('\n').or_not().to(())),
    }))
    .label_with(|| "newline")
}

/// A single whitespace character is accepted, excluding newline characters.
#[inline]
pub fn no_break<I: Input, S, C, M: Cb>() -> impl ParserOnce<I, (), C, S, M> {
    satisfy_map(|c: &char| match get_sp_kind(c)? {
        SpaceKind::Indentable(_) | SpaceKind::Other(_) => Some(()),
        _ => None,
    })
}

// Take one non-space character
#[inline]
pub fn no_ws<I: Input, S, C, M: Cb>() -> impl ParserOnce<I, char, C, S, M> {
    satisfy(|c: &char| get_sp_kind(c).is_none())
}

/// Accept any single space character.
#[inline]
pub fn space<I: Input, S, C, M: Cb>() -> impl ParserOnce<I, (), C, S, M> {
    satisfy_map(get_sp_kind).to(())
}

/// Greedily accepts a sequence of whitespace characters that do not contain a newline character.
#[inline]
pub fn no_break_ws<I: Input, S, C, M: Cb>() -> impl ParserOnce<I, (), C, S, M> {
    no_state(no_break.skip_many())
}

/// It greedily accepts a sequence of one or more whitespace characters, not including newline characters.
#[inline]
pub fn no_break_ws1<I: Input, S, C, M: Cb>() -> impl ParserOnce<I, (), C, S, M> {
    no_state(no_break.skip_many1())
}

/// A sequence of whitespace characters is greedily accepted.
#[inline(always)]
pub fn ws<I: Input, S, C, M: Cb>() -> impl ParserOnce<I, (), C, S, M> {
    no_state(satisfy_map(get_sp_kind).skip_many())
}
/// A sequence of one or more whitespace characters will be greedily accepted.
#[inline(always)]
pub fn ws1<I: Input, S, C, M: Cb>() -> impl ParserOnce<I, (), C, S, M> {
    no_state(satisfy_map(get_sp_kind).skip_many1())
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NLS {
    pub newline: usize,
    pub space: usize,
}
impl NLS {
    #[inline]
    pub fn new() -> NLS {
        NLS { newline: 0, space: 0 }
    }
}
// ??????space???????????????????????????????????????
/// Takes a sequence of whitespace characters and returns the number of times a newline was broken followed by a non-newline whitespace.
#[inline]
pub fn nls<I: Input, S, C, M: Cb>() -> impl ParserOnce<I, NLS, C, S, M> {
    no_state(tail_rec((NLS::new(), false), |(nls @ NLS { newline, space }, after_r)| {
        satisfy_map(get_sp_kind).or_not().map(move |kind| match kind {
            None => Ok(nls),
            Some(SpaceKind::NL(NLKind::CarriageReturn)) => Err((NLS { newline: newline + 1, space: 0 }, true)),
            Some(SpaceKind::NL(NLKind::LineFeed)) => {
                Err((if after_r { nls } else { NLS { newline: newline + 1, space: 0 } }, false))
            },
            Some(SpaceKind::NL(NLKind::Other)) => Err((NLS { newline: newline + 1, space: 0 }, false)),
            Some(_) => Err((NLS { newline, space: space + 1 }, false)),
        })
    }))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}
impl Display for LineColumn {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone)]
/// A position with rows and columns.
pub struct LineColumnCounter {
    lc: LineColumn,
    after_r: bool,
}
impl Counter<char> for LineColumnCounter {
    type Pos = LineColumn;
    type Error = Nil;
    #[inline]
    fn new() -> LineColumnCounter {
        Self { lc: LineColumn { line: 1, column: 1 }, after_r: false }
    }
    #[inline]
    fn pos(&self) -> LineColumn {
        self.lc
    }
    #[inline]
    fn next(&mut self, c: &char) -> Result<(), Nil> {
        match get_nl_kind(c) {
            None => {
                self.lc.column += 1;
                self.after_r = false;
            },
            Some(NLKind::Other) => {
                self.lc.column = 1;
                self.lc.line += 1;
                self.after_r = false
            },
            Some(NLKind::CarriageReturn) => {
                self.lc.column = 1;
                self.lc.line += 1;
                self.after_r = true
            },
            Some(NLKind::LineFeed) => {
                if !self.after_r {
                    self.lc.column = 1;
                    self.lc.line += 1;
                }
                self.after_r = false
            },
        }
        Ok(())
    }
}
