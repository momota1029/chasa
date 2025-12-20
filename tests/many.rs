use chasa::prelude::*;

#[test]
fn many_map_err_returns_summary_on_hard_failure() {
    let mut input: &'static str = "aaX!";
    let mut did_cut = false;
    let out = many_map::<_, _, (), _, _, _, usize>(
        |mut i: In<&'static str, (), (), ()>| match i.input.next() {
            Some('a') => Ok('a'),
            Some('X') => {
                i.cut();
                Err("X")
            }
            Some(_) => Err("other"),
            None => Err("eoi"),
        },
        |it| it.count(),
    )
    .run_once(In::new(&mut input, (), chasa::input::is_cut::IsCut::new(&mut did_cut)));

    assert_eq!(out, Err(chasa::parser::many::ManyErr { summary: 2, err: "X" }));
    assert_eq!(input, "!");
    assert_eq!(did_cut, true);
}

#[test]
fn many_rolls_back_none_terminator_attempt() {
    let mut input: &'static str = "aa!";
    let r = chasa::parse::parse_once::<_, chasa::parser::many::ManyErr<String, ()>, _, _>(
        &mut input,
        many::<_, String>(|i: In<&'static str, Merger<&'static str, chasa::parser::many::ManyErr<String, ()>>, (), ()>| match i
            .input
            .next()
        {
            Some('a') => Some('a'),
            Some('!') => None, // consumed, but must be rolled back
            Some(_) => None,
            None => None,
        }),
    );
    assert_eq!(r.out, Some("aa".to_string()));
    assert_eq!(input, "!");
    assert_eq!(r.did_cut, false);
    assert_eq!(r.errors.errors().len(), 0);
}

#[test]
fn many_returns_none_on_cut_failure_and_keeps_consumption() {
    let mut input: &'static str = "aaX!";
    let r = chasa::parse::parse_once::<_, chasa::parser::many::ManyErr<String, ()>, _, _>(
        &mut input,
        many::<_, String>(|mut i: In<&'static str, Merger<&'static str, chasa::parser::many::ManyErr<String, ()>>, (), ()>| {
            match i.input.next() {
                Some('a') => Some('a'),
                Some('X') => {
                    i.cut();
                    None
                }
                Some('!') => None,
                Some(_) => None,
                None => None,
            }
        }),
    );
    assert_eq!(r.out, None);
    assert_eq!(input, "!");
    assert_eq!(r.did_cut, true);
}

#[test]
fn many1_parses_one_or_more() {
    let mut input: &'static str = "aa!";
    let r = chasa::parse::parse_once::<_, chasa::parser::many::ManyErr<String, ()>, _, _>(
        &mut input,
        many1::<_, String>(
            |i: In<&'static str, Merger<&'static str, chasa::parser::many::ManyErr<String, ()>>, (), ()>| match i.input.next() {
                Some('a') => Some('a'),
                Some('!') => None, // consumed, but must be rolled back
                Some(_) => None,
                None => None,
            },
        ),
    );
    assert_eq!(r.out, Some("aa".to_string()));
    assert_eq!(input, "!");
    assert_eq!(r.did_cut, false);
    assert_eq!(r.errors.errors().len(), 0);
}

#[test]
fn many1_returns_none_on_soft_failure_first() {
    let mut input: &'static str = "!";
    let r = chasa::parse::parse_once::<_, chasa::parser::many::ManyErr<String, ()>, _, _>(
        &mut input,
        many1::<_, String>(
            |i: In<&'static str, Merger<&'static str, chasa::parser::many::ManyErr<String, ()>>, (), ()>| match i.input.next() {
                Some('a') => Some('a'),
                Some('!') => None, // consumed, but must be rolled back
                Some(_) => None,
                None => None,
            },
        ),
    );
    assert_eq!(r.out, None);
    assert_eq!(input, "");
    assert_eq!(r.did_cut, false);
    assert_eq!(r.errors.errors().len(), 1);
}

#[test]
fn many1_returns_none_on_cut_failure_and_keeps_consumption() {
    let mut input: &'static str = "X!";
    let r = chasa::parse::parse_once::<_, chasa::parser::many::ManyErr<String, ()>, _, _>(
        &mut input,
        many1::<_, String>(|mut i: In<&'static str, Merger<&'static str, chasa::parser::many::ManyErr<String, ()>>, (), ()>| {
            match i.input.next() {
                Some('a') => Some('a'),
                Some('X') => {
                    i.cut();
                    None
                }
                Some('!') => None,
                Some(_) => None,
                None => None,
            }
        }),
    );
    assert_eq!(r.out, None);
    assert_eq!(input, "!");
    assert_eq!(r.did_cut, true);
}

#[test]
fn many1_map_counts_one_or_more() {
    let mut input: &'static str = "aa!";
    let r = chasa::parse::parse_once::<_, chasa::parser::many::ManyErr<Option<usize>, ()>, _, _>(
        &mut input,
        many1_map(
            |i: In<&'static str, Merger<&'static str, chasa::parser::many::ManyErr<Option<usize>, ()>>, (), ()>| match i
                .input
                .next()
            {
                Some('a') => Some('a'),
                Some('!') => None,
                Some(_) => None,
                None => None,
            },
            |it| it.count(),
        ),
    );
    assert_eq!(r.out, Some(2));
    assert_eq!(input, "!");
    assert_eq!(r.did_cut, false);
}
