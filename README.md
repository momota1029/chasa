# chasa

Parser combinators for the YuLang workspace.

## TL;DR

```rust
use chasa::prelude::*;

// Parse "let x" and extract the identifier
let mut input = "let x";
let name = parse_ok_once(&mut input, tag("let").right(ws1).right(any)).unwrap();
assert_eq!(name, 'x');
```

Key combinators:
- `tag("...")` – match an exact string
- `ws1` / `ws` – match whitespace (one-or-more / zero-or-more)
- `any` – match any single character
- `right(q)` – parse both, return right result
- `many()` – repeat zero or more times
- `sep(s)` – parse separated list

Most combinators are available as methods (via `ParserOnce` / `ParserMut` / `Parser`), and the
`prelude` imports those traits so you can write `p.right(q)` / `p.many()` / `p.sep(...)`.

## Showcase

This crate supports both combinator style and imperative style via `In`.

### Example 1: S-expressions (combinator style)

```rust
use chasa::prelude::*;

#[derive(Debug, PartialEq, Eq)]
enum SExp {
    Atom(String),
    List(Vec<SExp>),
}

fn sexp(mut i: In<&str>) -> Option<SExp> {
    let atom_char = none_of(SPACE.and("()"));
    let atom = atom_char.many1().map(SExp::Atom);
    let list = sexp
        .sep(ws1)
        .map(SExp::List)
        .between(ws, ws)
        .between(item('('), item(')'));
    i.choice((list, atom))
}

let mut input = "(a (b c) d)";
let out = parse_ok_once(&mut input, sexp).unwrap();
assert_eq!(
    out,
    SExp::List(vec![
        SExp::Atom("a".into()),
        SExp::List(vec![SExp::Atom("b".into()), SExp::Atom("c".into())]),
        SExp::Atom("d".into()),
    ])
);
```

### Example 2: `key = value` (imperative style)

```rust
use chasa::prelude::*;

#[derive(Debug, PartialEq, Eq)]
enum Value {
    Bool(bool),
    Number(i64),
    Str(String),
}

fn kv(mut i: In<&str>) -> Option<(String, Value)> {
    let ident = one_of(ASCII_ALPHA.and("_")).bind(|h| {
        one_of(ASCII_ALPHANUM.and("_"))
            .many_map(move |it| std::iter::once(h).chain(it).collect::<String>())
    });

    let eq = item('=').between(ws, ws);

    let digit = one_of(ASCII_DIGIT);
    let number = choice((
        item('-').right(digit.many1::<String>()).map_once(|s: String| {
            let n = s.parse::<i64>().unwrap();
            Value::Number(-n)
        }),
        digit.many1::<String>().map_once(|s: String| {
            let n = s.parse::<i64>().unwrap();
            Value::Number(n)
        }),
    ));

    let str_body = none_of("\"\\").many::<String>();
    let string = str_body.between(item('\"'), item('\"')).map(Value::Str);

    let key = i.run(ident)?;
    i.run(eq)?;
    let value = i.choice((
        tag("true").to(Value::Bool(true)),
        tag("false").to(Value::Bool(false)),
        number,
        string,
    ))?;
    Some((key, value))
}

let mut input = "port = 8080";
assert_eq!(
    parse_ok_once(&mut input, kv).unwrap(),
    ("port".into(), Value::Number(8080))
);
let mut input = "name = \"alice\"";
assert_eq!(
    parse_ok_once(&mut input, kv).unwrap(),
    ("name".into(), Value::Str("alice".into()))
);
```

## Quick API tour

Entry points:
- `parse_once(&mut input, parser)` – run a `ParserOnce` with a fresh `Merger`
- `parse_ok_once(&mut input, parser)` – run a `ParserOnce` and return `Result<T, Error>`

Building blocks:
- Items: `any`, `item(c)`, `one_of("abc")`, `none_of("xyz")`
- Tags: `tag("keyword")`
- Whitespace: `ws`, `ws1`
- Sequencing: `then`, `right`, `left`, `between`
- Choice: `or`, `choice`
- Repetition: `many`, `many1`, `many_map`
- Separated lists: `sep`, `sep1`, `sep_map`, `sep_reduce`
- Lookahead: `lookahead`, `not`
- Control: `cut`, `maybe`, `label`

## Where to look next

- `prelude`: start here for imports
- `parser`: combinators and traits
- `input`: input abstractions and streaming inputs
- `parse`: helpers like `parse_ok_once`
