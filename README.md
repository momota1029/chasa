A parser combinator with additional operations on procedures.

A parser combinator is a mechanism that allows you to combine small syntactic elements to define a larger syntax, which can then be parse directly.

```rust
use chasa::{Parser, EasyParser, prim, prim::{one_of, char}, message};
// It reads a number of letters (numbers) from 0 to 9,
let num = one_of('0'..='9').many::<String>()
// Interpreted as a number, errors are reported as a message.
    .and_then(|str| str.parse::<u32>().map_err(message));

// Multiply by something separated by '*' and
let prod = num.sep_fold1(char('*'), |a,_,b| a * b);
// Then add the '+' separator to it.
let sum = prod.sep_fold1(char('+'), |a,_,b| a + b);
// Can parse simple addition and multiplication expressions.
assert_eq!(sum.parse_ok("1+2*3+4"), Some(11));
```

Like the relationship between `Fn` and `FnOnce`, we have `Parser` and `ParserOnce` and write a parser to manipulate the iterator.

To define and re-use the syntax recursively, use a function (which implements the [`Parser`] trait) that returns `impl `[`ParserOnce`].

In the following example, `EasyParser` is a special case alias for `ParserOnce`.
```rust
use chasa::*;
#[derive(Debug, PartialEq, Eq)]
enum SExp {
    Term(String),
    List(Vec<SExp>),
}
fn sexp_like<I: Input<Item = char>>() -> impl EasyParser<I, Output = SExp> {
    // The `parser` is to prevent recursion of existential types (removing it will crash the current compiler).
    parser(|k| {
        let term = satisfy(|c: &char| !char::is_space(c) && c != &'(' && c != &')').many1();
        k.then(term.map(SExp::Term).or(sexp_like.sep(ws1).between(char('('), char(')')).map(SExp::List)))
    })
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
use chasa::*;
use chasa::char::*;

#[derive(Debug,PartialEq)]
enum JSON {
    Object(Vec<(String, JSON)>),
    Array(Vec<JSON>),
    String(String),
    Number(f64),
    True,
    False,
    Null,
}

fn json_parser<I: Input<Item = char>>() -> impl EasyParser<I, Output = JSON> {
    any.case(|c, k| match c {
        '{' => k
            .then(
                char('"')
                    .right(string_char.many_with(|iter| iter.map_while(|x| x).collect()))
                    .between(whitespace, whitespace)
                    .bind(|key| char(':').right(json_parser).map_once(move |value| (key, value)))
                    .sep(char(',')),
            ).left(char('}'))
            .map(JSON::Object),
        '[' => k.then(json_parser.sep(char(','))).left(char(']')).map(JSON::Array),
        '"' => k.then(string_char.many_with(|iter| iter.map_while(|x| x).collect())).map(JSON::String),
        '-' => k.then(any).bind(num_parser).map(|n| JSON::Number(-n)),
        c @ '0'..='9' => k.then(num_parser(c)).map(JSON::Number),
        't' => k.then(str("rue").to(JSON::True)),
        'f' => k.then(str("alse").to(JSON::False)),
        'n' => k.then(str("ull").to(JSON::Null)),
        c => k.fail(error::unexpect(c)),
    })
    .between(whitespace, whitespace)
}

fn whitespace<I: Input<Item = char>>() -> impl EasyParser<I, Output = ()> {
    one_of("\t\r\n ").skip_many()
}

fn string_char<I: Input<Item = char>>() -> impl EasyParser<I, Output = Option<char>> {
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
                        .repeat::<String,_>(4)
                        .and_then(|str| {
                            char::from_u32(u32::from_str_radix(&str, 16).map_err(message)?)
                            .ok_or(unexpect("invalid unicode char"))
                        })
                    )
                    .map(Some),
                c => k.fail(unexpect(c)),
            }
        })),
        '"' => k.to(None),
        c => k.to(Some(c)),
    })
}

fn num_parser<I: Input<Item = char>>(c: char) -> impl EasyParser<I, Output = f64> {
    let digit = satisfy(|c| ('0'..='9').contains(c));
    extend_with_str(c.to_string(),
        parser_once(move |k| match c {
            '0' => k.done(),
            c @ '1'..='9' => k.then(digit.skip_many()),
            c => k.fail(unexpect(c)),
        })
        .left(char('.').right(digit.skip_many1()).or_not())
        .left(one_of("eE").right(one_of("+-").or_not()).right(digit.skip_many1()).or_not())
    ).and_then_once(|(_,str)| str.parse::<f64>().map_err(message))
}

assert_eq!(
    json_parser.parse_ok("{\"key1\": \"value1\", \"key2\": [ true, \"value3\" ], \"key3\": { \"key4\": 15e1 }}"),
    Some(JSON::Object(vec![
        ("key1".to_string(), JSON::String("value1".to_string())),
        ("key2".to_string(), JSON::Array(vec![
            JSON::True,
            JSON::String("value3".to_string())
        ])),
        ("key3".to_string(), JSON::Object(vec![("key4".to_string(), JSON::Number(150.0))]))
    ]))
);
```
