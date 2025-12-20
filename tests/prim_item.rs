use chasa::input::error::std::Unexpected;
use chasa::parse::parse_once;
use chasa::parser::choice::Choice;
use chasa::prelude::*;

#[test]
fn item_any() {
    let mut input = "ab";
    let out = parse_ok_once(&mut input, any).unwrap();
    assert_eq!(out, 'a');
    assert_eq!(input, "b");
}

#[test]
fn item_eoi() {
    let mut input = "";
    let out = parse_ok_once(&mut input, eoi).unwrap();
    assert_eq!(out, ());
    assert_eq!(input, "");
}

#[test]
fn item_satisfy() {
    let mut input = "ab";
    let out = parse_ok_once(&mut input, satisfy(|c| c == 'a')).unwrap();
    assert_eq!(out, 'a');
    assert_eq!(input, "b");
}

#[test]
fn item_item() {
    let mut input = "ab";
    let out = parse_ok_once(&mut input, item('a')).unwrap();
    assert_eq!(out, 'a');
    assert_eq!(input, "b");
}

#[test]
fn item_one_of() {
    let mut input = "b";
    let out = parse_ok_once(&mut input, one_of("ab")).unwrap();
    assert_eq!(out, 'b');
    assert_eq!(input, "");
}

#[test]
fn item_none_of() {
    let mut input = "b";
    let out = parse_ok_once(&mut input, none_of("a")).unwrap();
    assert_eq!(out, 'b');
    assert_eq!(input, "");
}

#[test]
fn item_one_of_range() {
    let mut input = "d";
    let out = parse_ok_once(&mut input, one_of('a'..='z')).unwrap();
    assert_eq!(out, 'd');
    assert_eq!(input, "");
}

#[test]
fn item_one_of_union_ranges() {
    let mut input = "Q";
    let out = parse_ok_once(&mut input, one_of(('a'..='z').and('A'..='Z'))).unwrap();
    assert_eq!(out, 'Q');
    assert_eq!(input, "");
}

#[test]
fn choice_rolls_back_errors_on_soft_failure() {
    let mut input = "a";
    let r = parse_once::<_, Unexpected<char>, _, _>(&mut input, Choice::new((item('b'), item('a'))));
    assert_eq!(r.out, Some('a'));
    assert!(r.errors.errors().is_empty());
}
