//! A convenience prelude for `chasa`.

pub use crate::{
    input::{
        In,
        error::{
            MergeErrors, Merger,
            std::{StdErr, StdSummary},
        },
        inner::{Back, Counter, Input, Offset, SeqInput, WithCounter},
    },
    parse::{ParseOkError, ParseResult, parse_ok_once, parse_once},
    parser::{Parser as _, ParserMut as _, ParserOnce as _},
};

// Function-level constructors (subset).
pub use crate::char::{
    ASCII, ASCII_ALPHA, ASCII_ALPHANUM, ASCII_DIGIT, SPACE, ascii, ascii_alpha, ascii_alphanumeric, ascii_digit, space, ws, ws1,
};
pub use crate::input::error::std::{ConvertErr, Expected};
pub use crate::parser::choice::choice;
pub use crate::parser::item::{any, eoi, item, none_of, one_of, satisfy, set::ItemSet as _};
pub use crate::parser::many::{
    CountRange as _, count, many, many_map, many_map_mut, many_map_once, many_skip, many1, many1_map, many1_map_mut, many1_map_once,
    many1_skip,
};
pub use crate::parser::prim::{convert_err, label, label_with, with_range, with_seq};
pub use crate::parser::prim::{cut, lookahead, maybe, no_cut, not};
pub use crate::parser::sep::{sep, sep_map, sep_map_mut, sep_map_once, sep_reduce, sep1, sep1_map};
pub use crate::parser::then::{between, bind, left, map, right, then, to};
pub use crate::parser::token::tag;
