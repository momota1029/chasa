//! Sequence-like predicates for tag parsers.
//!
//! [`ItemSeq`] is the "sequence" counterpart to [`crate::parser::item::set::ItemSet`]:
//! it describes something that can yield an exact sequence of "expected items" to be matched
//! against the input.
//!
//! It exists to make a single `tag(...)` API work for both:
//!
//! - string tags: `tag("let")` (for `char` inputs)
//! - token tags: `tag(&[Tok::Let, Tok::Ident])`
//!
//! The actual parser constructor lives in [`crate::parser::token::tag`].

/// A value that can be compared to an input item by reference.
///
/// This is used by exact-sequence tag parsers to avoid cloning input items during comparisons.
pub trait ExpectedItem<Item> {
    fn eq_item(&self, item: &Item) -> bool;
}

impl ExpectedItem<char> for char {
    fn eq_item(&self, item: &char) -> bool {
        self == item
    }
}

impl<Item, T> ExpectedItem<Item> for &T
where
    T: PartialEq<Item>,
{
    fn eq_item(&self, item: &Item) -> bool {
        (*self).eq(item)
    }
}

/// An "exact sequence" of expected items.
///
/// This is intentionally iterator-like: it can be turned into an iterator that yields `Expected`
/// values, each comparable to an input `Item`.
pub trait ItemSeq<Item>: Clone {
    type Expected: ExpectedItem<Item>;
    type Iter: Iterator<Item = Self::Expected>;

    fn iter(self) -> Self::Iter;
}

impl<'a> ItemSeq<char> for &'a str {
    type Expected = char;
    type Iter = std::str::Chars<'a>;

    fn iter(self) -> Self::Iter {
        self.chars()
    }
}

impl<'a, Item, T> ItemSeq<Item> for &'a [T]
where
    T: PartialEq<Item>,
{
    type Expected = &'a T;
    type Iter = std::slice::Iter<'a, T>;

    fn iter(self) -> Self::Iter {
        self.iter()
    }
}

impl<'a, Item, T, const N: usize> ItemSeq<Item> for &'a [T; N]
where
    T: PartialEq<Item>,
{
    type Expected = &'a T;
    type Iter = std::slice::Iter<'a, T>;

    fn iter(self) -> Self::Iter {
        self.as_slice().iter()
    }
}
