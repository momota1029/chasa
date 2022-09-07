# chasa
[![Crates.io](https://img.shields.io/crates/v/chasa.svg)](https://crates.io/crates/chasa)
[![Docs](https://docs.rs/chasa/badge.svg)](https://docs.rs/chasa)

A parser combinator with `many` taking iterator, conditional branching, and method chain.

A parser combinator is a mechanism that allows you to combine small syntactic elements to define a larger syntax, which can then be parse directly.

```rust
use chasa::char::prelude::*;
// It reads a number of letters (numbers) from 0 to 9,
let num = one_of('0'..='9').many1::<String>()
// Interpreted as a number, errors are reported as a message.
    .and_then(|str| str.parse::<u32>().map_err(from_error));

// Multiply by something separated by '*' and
let prod = num.sep_reduce(char('*'), |a,_,b| a * b);
// Then add the '+' separator to it.
let sum = prod.sep_reduce(char('+'), |a,_,b| a + b);
// Can parse simple addition and multiplication expressions.
assert_eq!(sum.parse_ok("10*10*10+9*9*9"), Some(1729));
```

The base is Parsec, but with some Rust essence added. For example, not only do you get `Vec` with `many`, but you can also manipulate iterators.
```rust
let string = char('"').right(
    any.many_with(|iter| iter.take_while(|c| c != &'"').collect())
);
assert_eq!(
    string.parse_ok("\"Lorem ipsum\" dolor sit amet,"),
    Some("Lorem ipsum".to_string())
)
```

Like the relationship between `Fn` and `FnOnce`, we have `Parser` and `ParserOnce` and write a parser to manipulate the iterator.

To define and re-use the syntax recursively, use a function (which implements the [`Parser`] trait) that returns `impl `[`ParserOnce`].

In the following example, `EasyParser` is a special case alias for `ParserOnce`.
```rust
use chasa::{char, char::prelude::*};
#[derive(Debug, PartialEq, Eq)]
enum SExp {
    Term(String),
    List(Vec<SExp>),
}
fn sexp_like<I: Seq>() -> impl Pat<I, SExp> {
    // `run` prevents type recursion, but does not Box
    let term = satisfy(|c| !char::is_space(c) && c != &'(' && c != &')').many1();
    term.map(SExp::Term).or(run(sexp_like).sep(ws1).between(char('('), char(')')).map(SExp::List))
}
assert_eq!(
    sexp_like.parse_easy("(defun fact (x) (if (zerop x) 1 (* x (fact (- x 1)))))"),
    Ok(SExp::List(vec![
        SExp::Term("defun".to_string()),
        SExp::Term("fact".to_string()),
        SExp::List(vec![SExp::Term("x".to_string())]),
        SExp::List(vec![
            SExp::Term("if".to_string()),
            SExp::List(vec![SExp::Term("zerop".to_string()), SExp::Term("x".to_string())]),
            SExp::Term("1".to_string()),
            SExp::List(vec![
                SExp::Term("*".to_string()),
                SExp::Term("x".to_string()),
                SExp::List(vec![
                    SExp::Term("fact".to_string()),
                    SExp::List(vec![SExp::Term("-".to_string()), SExp::Term("x".to_string()), SExp::Term("1".to_string())]),
                ]),
            ]),
        ]),
    ])),
);
```

Rust doesn't allow you to branch different functions, which prevents you from writing procedural parsers. This hampers the writing of procedural parsers, which can be replaced by a procedural chain for better visibility.

For example, the JSON parser is procedural, but you can write it in procedural form:
```rust
use chasa::char::prelude::*;

#[derive(Debug, PartialEq)]
enum JSON {
    Object(Vec<(String, JSON)>),
    Array(Vec<JSON>),
    String(String),
    Number(f64),
    True,
    False,
    Null,
}

fn json_parser<I: Seq>() -> impl Pat<I, JSON> {
    any.case(|c, k| match c {
        '{' => k
            .then(
                char('"')
                    .right(string_char.many_map(|iter| iter.map_while(|x| x).collect::<String>()))
                    .between(whitespace, whitespace)
                    .bind(|key| char(':').right(run(json_parser)).map_once(move |value: JSON| (key, value)))
                    .sep(char(',')),
            )
            .left(char('}'))
            .map(JSON::Object),
        '[' => k.then(json_parser.sep(char(','))).left(char(']')).map(JSON::Array),
        '"' => k.then(string_char.many_map(|iter| iter.map_while(|x| x).collect())).map(JSON::String),
        '-' => k.then(any).bind(num_parser).map(|n| JSON::Number(-n)),
        c @ '0'..='9' => k.then(num_parser(c)).map(JSON::Number),
        't' => k.then(str("rue").to(JSON::True)),
        'f' => k.then(str("alse").to(JSON::False)),
        'n' => k.then(str("ull").to(JSON::Null)),
        c => k.fail(unexpected(token(c))),
    })
    .between(whitespace, whitespace)
}

fn whitespace<I: Seq>() -> impl Pat<I, ()> {
    one_of("\t\r\n ").skip_many()
}

fn string_char<I: Seq>() -> impl Pat<I, Option<char>> {
    any.case(|c, k| match c {
        '\\' => k.then(any.case(|c, k| {
            match c {
                '"' => k.to(Some('\"')),
                '\\' => k.to(Some('\\')),
                '/' => k.to(Some('/')),
                'b' => k.to(Some('\x08')),
                'f' => k.to(Some('\x0C')),
                'n' => k.to(Some('\n')),
                'r' => k.to(Some('\r')),
                't' => k.to(Some('\t')),
                'u' => k
                    .then(
                        satisfy(|c| matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F'))
                            .repeat::<String, _>(4)
                            .and_then(|str| u32::from_str_radix(&str, 16).map_err(from_error))
                            .and_then(|int| char::from_u32(int).ok_or(unexpected(format("invalid unicode char")))),
                    )
                    .map(Some),
                c => k.fail(unexpected(token(c))),
            }
        })),
        '"' => k.to(None),
        c => k.to(Some(c)),
    })
}

fn num_parser<I: Seq>(c: char) -> impl Pat<I, f64> {
    let digit = one_of('0'..='9');
    extend_with_str(c.to_string(), {
        skip_chain((
            parser_once(move |k| match c {
                '0' => k.done(),
                '1'..='9' => k.then(digit.skip_many()),
                c => k.fail(unexpected(token(c))),
            }),
            char('.').right(digit.skip_many1()).or_not(),
            one_of("eE").right(one_of("+-").or_not()).right(digit.skip_many1()).or_not(),
        ))
    })
    .and_then_once(|(_, str)| str.parse::<f64>().map_err(from_error))
}

assert_eq!(
    json_parser.parse_ok("{\"key1\": \"value1\", \"key2\": [ true, \"value3\" ], \"key3\": { \"key4\": 15e1 }}"),
    Some(JSON::Object(vec![
        ("key1".to_string(), JSON::String("value1".to_string())),
        ("key2".to_string(), JSON::Array(vec![JSON::True, JSON::String("value3".to_string())])),
        ("key3".to_string(), JSON::Object(vec![("key4".to_string(), JSON::Number(150.0))]))
    ]))
);
```
