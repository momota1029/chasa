# chasa, procedural invariant parser combinator

A parser combinator with additional operations on procedures.

A parser combinator is a mechanism that allows you to combine small syntactic elements to define a larger syntax, which can then be parse directly.

```
use chasa::{Parser, EasyParser, prim, prim::{one_of, char}};
// It reads a number of letters (numbers) from 0 to 9,
let num = one_of('0'..='9').many::<String>()
// Interpreted as a number, errors are reported as a message.
    .and_then(|str| str.parse::<u32>().map_err(prim::Error::Message));

// Multiply by something separated by '*' and
let prod = num.sep1(1, |a, b| a * b, char('*'));
// Then add the '+' separator to it.
let sum = prod.sep1(0, |a, b| a + b, char('+'));
// Can parse simple addition and multiplication expressions.
assert_eq!(sum.parse_easy("1+2*3+4".chars()).ok(), Some(11));
```

Rust doesn't allow you to branch different functions, which prevents you from writing procedural parsers. This hampers the writing of procedural parsers, which can be replaced by a procedural chain for better visibility.

For example, the JSON parser is procedural, but you can write it in procedural form:
```
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

fn json_parser<I: Input<Item = char> + Clone>() -> impl EasyParser<I, Output = JSON> {
    any.case(|c, k| match c {
        '{' => k
            .then(
                char('"')
                    .right(string_char.many_with(|iter| iter.map_while(|x| x).collect()))
                    .between(whitespace, whitespace)
                    .bind(|key| char(':').right(json_parser).map_once(move |value| (key, value)))
                    .extend_sep(vec![], char(',')),
            ).left(char('}'))
            .map(JSON::Object),
        '[' => k.then(json_parser.extend_sep(vec![], char(','))).left(char(']')).map(JSON::Array),
        '"' => k.then(string_char.many_with(|iter| iter.map_while(|x| x).collect())).map(JSON::String),
        '-' => k.then(any).bind(num_parser).map(|n| JSON::Number(-n)),
        c @ '0'..='9' => k.then(num_parser(c)).map(JSON::Number),
        't' => k.then(string("rue".chars(), JSON::True)),
        'f' => k.then(string("alse".chars(), JSON::False)),
        'n' => k.then(string("ull".chars(), JSON::Null)),
        c => k.fail(prim::Error::Unexpect(c)),
    })
    .between(whitespace, whitespace)
}

fn whitespace<I: Input<Item = char> + Clone>() -> impl EasyParser<I, Output = ()> {
    one_of("\t\r\n ".chars()).skip_many()
}

fn string_char<I: Input<Item = char> + Clone>() -> impl EasyParser<I, Output = Option<char>> {
    let hex = satisfy(|c: &char| matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F'));
    any.case(move |c, k| match c {
        '\\' => k.then(any.case(|c, k| {
            match c {
                '"' => k.pure(Some('\"')),
                '\\' => k.pure(Some('\\')),
                '/' => k.pure(Some('/')),
                'b' => k.pure(Some('\x08')),
                'f' => k.pure(Some('\x0C')),
                'n' => k.pure(Some('\n')),
                'r' => k.pure(Some('\r')),
                't' => k.pure(Some('\t')),
                'u' => k
                    .then(
                        hex.many_with(|iter| iter.take(4).collect::<String>())
                            .and_then(|str| if str.len() < 4 { Err(prim::Error::Unexpect("4 hex digits")) } else { Ok(str) })
                            .and_then(|str| u32::from_str_radix(&str, 16).map_err(prim::Error::Message))
                            .and_then(|u| char::from_u32(u).ok_or(prim::Error::Unexpect("invalid unicode char"))),
                    )
                    .map(Some),
                c => k.fail(prim::Error::Unexpect(c)),
            }
        })),
        '"' => k.pure(None),
        c => k.pure(Some(c)),
    })
}

fn num_parser<I: Input<Item = char> + Clone>(c: char) -> impl EasyParser<I, Output = f64> {
    let digit = satisfy(|c: &char| ('0'..='9').contains(c));
    parser_once(move |k| match c {
        '0' => k.then(char('.').or_not().case(|c,k| if c.is_some() {
            k.then(digit.extend("0.".to_string()))
        } else {
            k.pure("0".to_string())
        })),
        c @ '1'..='9' => k.then(digit.extend(c.to_string())).bind(|mut str| char('.').or_not().case_once(move |c,k|
            if c.is_some() {
                str.push('.');
                k.then(digit.extend(str))
            }
            else {
                k.pure(str)
            }
        )),
        c => k.fail(prim::Error::Unexpect(c)),
    })
    .bind_once(move |mut str| {
        one_of("eE".chars()).right(one_of("+-".chars()).or_not()).bind_once(move |pm| {
            str.push('e');
            str.extend(pm);
            digit.extend(str)
        })
    })
    .and_then_once(|str| str.parse::<f64>().map_err(prim::Error::Message))
}

assert_eq!(
    json_parser.parse_easy("{\"key1\": \"value1\", \"key2\": [ true, \"value3\" ], \"key3\": { \"key4\": 15e1 }}".chars()).ok(),
    Some(JSON::Object(vec![
        ("key1".to_string(), JSON::String("value1".to_string())),
        ("key2".to_string(), JSON::Array(vec![JSON::True, JSON::String("value3".to_string())])),
        ("key3".to_string(), JSON::Object(vec![("key4".to_string(), JSON::Number(150.0))]))
    ]))
);
```