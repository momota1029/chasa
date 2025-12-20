//! Standard error types for `chasa`.
//!
//! This module provides a "good enough" default error representation (`StdErr`) that can be used
//! as the crate's `E` parameter when you don't want to design a custom error type yet.
//!
//! It is designed to work well with:
//!
//! - item-level mismatches (`Unexpected`)
//! - Parsec-like labelling (`Expected`, injected by `.label(...)`)

use ::std::borrow::Cow;
use ::std::{cmp::Ordering, fmt};

use crate::input::error::MergeErrors;
use crate::parser::many::ManyErr;
use crate::parser::prim::{ConvertErr as PrimConvertErr, NotErr};

/// A standard error enum intended for quick start.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StdErr<Item> {
    /// Parsec-like expected label.
    Expected(Expected<()>),
    /// Unexpected input (mismatch / end-of-input).
    Unexpected(Unexpected<Item>),
    /// Value-conversion failure (e.g. `convert_err`).
    Convert(ConvertErr),
    Many,
}
/// Parsec-style label with the start index.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Expected<E> {
    /// Start index of the labelled parser.
    pub start: u64,
    /// Human-friendly label.
    pub label: Cow<'static, str>,
    pub err: E,
}
impl<E> Expected<E> {
    /// Create a new labelled error.
    pub fn new(start: u64, label: impl Into<Cow<'static, str>>, err: E) -> Self {
        Self { start, label: label.into(), err }
    }
}
impl<E> fmt::Display for Expected<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.label.as_ref())
    }
}

/// Unexpected input (item or end-of-input).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Unexpected<Item> {
    EndOfInput,
    Item(Item),
}
/// Marker for end-of-input.
pub struct UnexpectedEndOfInput;
/// Marker for a concrete unexpected item.
pub struct UnexpectedItem<Item>(pub Item);
impl<Item> From<UnexpectedEndOfInput> for Unexpected<Item> {
    fn from(_: UnexpectedEndOfInput) -> Self {
        Unexpected::EndOfInput
    }
}
impl<Item> From<UnexpectedItem<Item>> for Unexpected<Item> {
    fn from(value: UnexpectedItem<Item>) -> Self {
        Unexpected::Item(value.0)
    }
}
impl<Item: fmt::Debug> fmt::Display for Unexpected<Item> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unexpected::EndOfInput => f.write_str("end of input"),
            Unexpected::Item(item) => write!(f, "{item:?}"),
        }
    }
}

pub type ConvertErr = PrimConvertErr<(), Cow<'static, str>>;

impl<Item, E> From<Expected<E>> for StdErr<Item> {
    fn from(value: Expected<E>) -> Self {
        StdErr::Expected(Expected { start: value.start, label: value.label, err: () })
    }
}

impl<Item> From<Unexpected<Item>> for StdErr<Item> {
    fn from(value: Unexpected<Item>) -> Self {
        StdErr::Unexpected(value)
    }
}

impl<Item> From<UnexpectedItem<Item>> for StdErr<Item> {
    fn from(value: UnexpectedItem<Item>) -> Self {
        StdErr::Unexpected(Unexpected::from(value))
    }
}

impl<Item> From<UnexpectedEndOfInput> for StdErr<Item> {
    fn from(value: UnexpectedEndOfInput) -> Self {
        StdErr::Unexpected(Unexpected::from(value))
    }
}

impl<Item, CE> From<PrimConvertErr<(), CE>> for StdErr<Item>
where
    CE: Into<Cow<'static, str>>,
{
    fn from(value: PrimConvertErr<(), CE>) -> Self {
        let mapped = match value {
            PrimConvertErr::Parse(()) => PrimConvertErr::Parse(()),
            PrimConvertErr::Convert(err) => PrimConvertErr::Convert(err.into()),
        };
        StdErr::Convert(mapped)
    }
}

impl<O, E, Item> From<ManyErr<O, E>> for StdErr<Item> {
    fn from(_value: ManyErr<O, E>) -> Self {
        StdErr::Many
    }
}

impl<Item, E> From<NotErr<E>> for StdErr<Item> {
    fn from(value: NotErr<E>) -> Self {
        match value {
            NotErr::Exists => StdErr::Convert(ConvertErr::from("negative lookahead failed")),
            NotErr::CutErr(_) => StdErr::Convert(ConvertErr::from("negative lookahead failed (cut)")),
        }
    }
}

/// Merged summary for [`StdErr`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StdSummary<Item> {
    pub unexpected: Option<Unexpected<Item>>,
    pub convert: Option<ConvertErr>,
    /// Expected labels selected at the "latest" label start index (typically innermost).
    ///
    /// This list contains only labels whose `start` equals the maximum `start` observed among
    /// recorded labels for the chosen span.
    ///
    /// Duplicates by `label` are removed.
    pub expected: Vec<Expected<()>>,
}

impl<Item> StdSummary<Item> {
    /// Return true if no error components are present.
    pub fn is_empty(&self) -> bool {
        self.unexpected.is_none() && self.convert.is_none() && self.expected.is_empty()
    }
}

/// Merge errors by extracting one `unexpected` (if present) and selecting the "latest" expected
/// label by its start index.
///
/// When multiple labels have the same latest `start`, all of them are kept (duplicates removed).
impl<Item: Clone> MergeErrors for StdErr<Item> {
    type Output = StdSummary<Item>;

    fn merge(errors: &[Self]) -> Self::Output {
        let mut unexpected = None;
        let mut convert = None;
        let mut expected: Vec<Expected<()>> = Vec::new();
        let mut max_start: Option<u64> = None;

        for e in errors {
            match e {
                StdErr::Unexpected(u) => {
                    if unexpected.is_none() {
                        unexpected = Some(u.clone());
                    }
                }
                StdErr::Convert(c) => match c {
                    PrimConvertErr::Convert(_) => {
                        if convert.is_none() {
                            convert = Some(c.clone());
                        }
                    }
                    PrimConvertErr::Parse(_) => {}
                },
                StdErr::Expected(x) => match max_start {
                    None => {
                        max_start = Some(x.start);
                        expected.push(x.clone());
                    }
                    Some(cur) => match x.start.cmp(&cur) {
                        Ordering::Less => {}
                        Ordering::Equal => expected.push(x.clone()),
                        Ordering::Greater => {
                            max_start = Some(x.start);
                            expected.clear();
                            expected.push(x.clone());
                        }
                    },
                },
                _ => (),
            }
        }

        // De-dup by label while preserving order.
        let mut dedup = Vec::<Expected<()>>::new();
        for x in expected {
            if dedup.iter().any(|y| y.label == x.label) {
                continue;
            }
            dedup.push(x);
        }

        StdSummary { unexpected, convert, expected: dedup }
    }
}

impl<Item: fmt::Debug> fmt::Display for StdSummary<Item> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            return f.write_str("<no error>");
        }

        if let Some(u) = &self.unexpected {
            write!(f, "unexpected {u}")?;
        }

        if let Some(c) = &self.convert {
            if self.unexpected.is_some() {
                f.write_str(", ")?;
            }
            write!(f, "error {c}")?;
        }

        if !self.expected.is_empty() {
            if self.unexpected.is_some() || self.convert.is_some() {
                f.write_str(", ")?;
            }
            f.write_str("expecting ")?;
            for (i, e) in self.expected.iter().enumerate() {
                if i > 0 {
                    f.write_str(" or ")?;
                }
                write!(f, "{e}")?;
            }
        }

        Ok(())
    }
}
