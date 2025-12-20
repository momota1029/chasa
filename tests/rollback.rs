use chasa::input::{In, is_cut::IsCut};
use chasa::parser::prim::{self, convert_err};
use chasa::prelude::*;

#[test]
fn maybe_rolls_back_in_merger_but_not_in_direct() {
    // Merger mode: rollback on soft failure.
    let mut input = "b";
    let out = parse_ok_once(&mut input, prim::maybe(item('a'))).unwrap();
    assert_eq!(out, None);
    assert_eq!(input, "b");

    // Direct mode: keep input position on soft failure.
    let mut input = "b";
    let mut did_cut = false;
    let out = prim::maybe(item('a')).run_once(In::<&str, ()>::new(&mut input, (), IsCut::new(&mut did_cut)));
    assert_eq!(out, Ok(None));
    assert_eq!(input, "");
}

#[test]
fn tag_rolls_back_one_item_on_mismatch() {
    let mut input = "ab";
    let r = parse_once::<_, StdErr<char>, _, _>(&mut input, tag("ac"));
    assert!(r.out.is_none());
    // First char consumed, mismatch char rolled back.
    assert_eq!(input, "b");
}

#[test]
fn convert_err_rolls_back_on_convert_failure() {
    let mut input = "aX";
    let p = convert_err(item('a'), |_| Err::<char, &str>("bad"));
    let r = parse_once::<_, StdErr<char>, _, _>(&mut input, p);
    assert!(r.out.is_none());
    assert_eq!(input, "aX");
}
