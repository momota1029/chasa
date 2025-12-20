use chasa::prelude::*;

#[test]
fn tag_str_matches() {
    let mut input = "hello!";
    let out = parse_ok_once(&mut input, tag("hello")).unwrap();
    assert_eq!(out, ());
    assert_eq!(input, "!");
}

#[test]
fn tag_slice_matches() {
    let mut input: &[u8] = &[1, 2, 3, 4];
    let out = parse_ok_once(&mut input, tag(&[1u8, 2, 3])).unwrap();
    assert_eq!(out, ());
    assert_eq!(input, &[4]);
}
