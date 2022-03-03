//! A parser combinator with additional operations on procedures.
//!
//! A parser combinator is a mechanism that allows you to combine small syntactic elements to define a larger syntax, which can then be parse directly.
//!
//! ```
//! use chasa::{Parser, EasyParser, prim, prim::{one_of, char}};
//! // It reads a number of letters (numbers) from 0 to 9,
//! let num = one_of('0'..='9').many::<String>()
//! // Interpreted as a number, errors are reported as a message.
//!     .and_then(|str| str.parse::<u32>().map_err(prim::Error::Message));
//!
//! // Multiply by something separated by '*' and
//! let prod = num.sep_fold1(char('*'), |a,_,b| a * b);
//! // Then add the '+' separator to it.
//! let sum = prod.sep_fold1(char('+'), |a,_,b| a + b);
//! // Can parse simple addition and multiplication expressions.
//! assert_eq!(sum.parse_ok("1+2*3+4"), Some(11));
//! ```
//!
//! Like the relationship between `Fn` and `FnOnce`, we have `Parser` and `ParserOnce` and write a parser to manipulate the iterator.
//!
//! To define and re-use the syntax recursively, use a function (which implements the [`Parser`] trait) that returns `impl `[`ParserOnce`].
//!
//! In the following example, `EasyParser` is a special case alias for `ParserOnce`.
//! ```
//! use chasa::*;
//! #[derive(Debug, PartialEq, Eq)]
//! enum SExp {
//!     Term(String),
//!     List(Vec<SExp>),
//! }
//! fn sexp_like<I: Input<Item = char> + Clone>() -> impl EasyParser<I, Output = SExp> {
//!     // The `parser` is to prevent recursion of existential types (removing it will crash the current compiler).
//!     parser(|k| {
//!         let term = satisfy(|c: &char| !char::is_space(c) && c != &'(' && c != &')').many1();
//!         k.then(term.map(SExp::Term).or(sexp_like.sep(ws1).between(char('('), char(')')).map(SExp::List)))
//!     })
//! }
//! assert_eq!(
//!     sexp_like.parse_easy("(defun fact (x) (if (zerop x) 1 (* x (fact (- x 1)))))"),
//!     Ok(SExp::List(vec![
//!         SExp::Term("defun".to_string()),
//!         SExp::Term("fact".to_string()),
//!         SExp::List(vec![SExp::Term("x".to_string())]),
//!         SExp::List(vec![
//!             SExp::Term("if".to_string()),
//!             SExp::List(vec![SExp::Term("zerop".to_string()), SExp::Term("x".to_string())]),
//!             SExp::Term("1".to_string()),
//!             SExp::List(vec![
//!                 SExp::Term("*".to_string()),
//!                 SExp::Term("x".to_string()),
//!                 SExp::List(vec![
//!                     SExp::Term("fact".to_string()),
//!                     SExp::List(vec![SExp::Term("-".to_string()), SExp::Term("x".to_string()), SExp::Term("1".to_string())]),
//!                 ]),
//!             ]),
//!         ]),
//!     ])),
//! );
//! ```
//!
//! Rust doesn't allow you to branch different functions, which prevents you from writing procedural parsers. This hampers the writing of procedural parsers, which can be replaced by a procedural chain for better visibility.
//!
//! For example, the JSON parser is procedural, but you can write it in procedural form:
//! ```
//! use chasa::*;
//! use chasa::char::*;
//!
//! #[derive(Debug,PartialEq)]
//! enum JSON {
//!     Object(Vec<(String, JSON)>),
//!     Array(Vec<JSON>),
//!     String(String),
//!     Number(f64),
//!     True,
//!     False,
//!     Null,
//! }
//!
//! fn json_parser<I: Input<Item = char> + Clone>() -> impl EasyParser<I, Output = JSON> {
//!     any.case(|c, k| match c {
//!         '{' => k
//!             .then(
//!                 char('"')
//!                     .right(string_char.many_with(|iter| iter.map_while(|x| x).collect()))
//!                     .between(whitespace, whitespace)
//!                     .bind(|key| char(':').right(json_parser).map_once(move |value| (key, value)))
//!                     .sep(char(',')),
//!             ).left(char('}'))
//!             .map(JSON::Object),
//!         '[' => k.then(json_parser.sep(char(','))).left(char(']')).map(JSON::Array),
//!         '"' => k.then(string_char.many_with(|iter| iter.map_while(|x| x).collect())).map(JSON::String),
//!         '-' => k.then(any).bind(num_parser).map(|n| JSON::Number(-n)),
//!         c @ '0'..='9' => k.then(num_parser(c)).map(JSON::Number),
//!         't' => k.then(str("rue", JSON::True)),
//!         'f' => k.then(str("alse", JSON::False)),
//!         'n' => k.then(str("ull", JSON::Null)),
//!         c => k.fail(prim::Error::Unexpect(c)),
//!     })
//!     .between(whitespace, whitespace)
//! }
//!
//! fn whitespace<I: Input<Item = char> + Clone>() -> impl EasyParser<I, Output = ()> {
//!     one_of("\t\r\n ").skip_many()
//! }
//!
//! fn string_char<I: Input<Item = char> + Clone>() -> impl EasyParser<I, Output = Option<char>> {
//!     let hex = satisfy(|c: &char| matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F'));
//!     any.case(move |c, k| match c {
//!         '\\' => k.then(any.case(|c, k| {
//!             match c {
//!                 '"' => k.to(Some('\"')),
//!                 '\\' => k.to(Some('\\')),
//!                 '/' => k.to(Some('/')),
//!                 'b' => k.to(Some('\x08')),
//!                 'f' => k.to(Some('\x0C')),
//!                 'n' => k.to(Some('\n')),
//!                 'r' => k.to(Some('\r')),
//!                 't' => k.to(Some('\t')),
//!                 'u' => k
//!                     .then(
//!                         hex.many_with(|iter| iter.take(4).collect::<String>())
//!                             .and_then(|str| if str.len() < 4 { Err(prim::Error::Unexpect("4 hex digits")) } else { Ok(str) })
//!                             .and_then(|str| u32::from_str_radix(&str, 16).map_err(prim::Error::Message))
//!                             .and_then(|u| char::from_u32(u).ok_or(prim::Error::Unexpect("invalid unicode char"))),
//!                     )
//!                     .map(Some),
//!                 c => k.fail(prim::Error::Unexpect(c)),
//!             }
//!         })),
//!         '"' => k.to(None),
//!         c => k.to(Some(c)),
//!     })
//! }
//!
//! fn num_parser<I: Input<Item = char> + Clone>(c: char) -> impl EasyParser<I, Output = f64> {
//!     let digit = satisfy(|c: &char| ('0'..='9').contains(c));
//!     parser_once(move |k| match c {
//!         '0' => k.then(char('.').or_not().case(|c,k| if c.is_some() {
//!             k.then(digit.extend("0.".to_string()))
//!         } else {
//!             k.to("0".to_string())
//!         })),
//!         c @ '1'..='9' => k.then(digit.extend(c.to_string())).bind(|mut str| char('.').or_not().case_once(move |c,k|
//!             if c.is_some() {
//!                 str.push('.');
//!                 k.then(digit.extend(str))
//!             }
//!             else {
//!                 k.to(str)
//!             }
//!         )),
//!         c => k.fail(prim::Error::Unexpect(c)),
//!     })
//!     .bind_once(move |mut str| {
//!         one_of("eE").or_not().case_once(move |e,k| match e {
//!             Some(_) => k.then(one_of("+-").or_not()).bind(move |pm| {
//!                 str.push('e');
//!                 str.extend(pm);
//!                 digit.extend(str)
//!             }),
//!             None => k.to(str)
//!         })
//!     })
//!     .and_then_once(|str| str.parse::<f64>().map_err(prim::Error::Message))
//! }
//!
//! assert_eq!(
//!     json_parser.parse_ok("{\"key1\": \"value1\", \"key2\": [ true, \"value3\" ], \"key3\": { \"key4\": 15e1 }}"),
//!     Some(JSON::Object(vec![
//!         ("key1".to_string(), JSON::String("value1".to_string())),
//!         ("key2".to_string(), JSON::Array(vec![
//!             JSON::True,
//!             JSON::String("value3".to_string())
//!         ])),
//!         ("key3".to_string(), JSON::Object(vec![("key4".to_string(), JSON::Number(150.0))]))
//!     ]))
//! );
//! ```

pub mod traits;
#[doc(inline)]
pub use traits::*;
pub mod char;
pub use crate::char::{newline, no_break, no_break_ws, no_break_ws1, space, ws, ws1};
pub mod combi;
pub use combi::{before, fold, fold1, many, not_followed_by, pure_or, tail_rec};
pub mod cont;
pub mod error;
pub use error::{Error, LazyError, Nil};
pub mod input;
pub use input::Input;
pub mod prim;
pub use prim::{
    any, char, config, eoi, fail, fail_with, get_state, local_state, no_state, none_of, one_of, parser, parser_once,
    pos, pure, satisfy, satisfy_map, set_config, set_state, state, str,
};
mod util;
