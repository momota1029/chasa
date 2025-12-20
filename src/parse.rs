//! High-level entry points to run parsers.
//!
//! This is a small compatibility layer inspired by `chasa::parse`, used mainly for examples/tests.

use ::std::fmt;

use crate::{
    input::{
        In,
        error::{
            Merger, OutOf,
            std::{StdErr, StdSummary},
        },
        inner::Input,
        is_cut::IsCut,
    },
    parser::{ParserOnce, ValueOf},
};

/// Result of a parse run.
pub struct ParseResult<I: Input, E, O> {
    pub out: O,
    pub errors: Merger<I, E>,
    pub did_cut: bool,
}

impl<I: Input, E, O> ParseResult<I, E, O> {
    /// Best error range recorded by the merger.
    pub fn range(&self) -> Option<&std::ops::Range<I::Pos>> {
        self.errors.range()
    }

    /// Errors recorded for the best range.
    pub fn errors(&self) -> &[E] {
        self.errors.errors()
    }
}

/// Run a parser once with a fresh error merger.
pub fn parse_once<I, E, P, O>(input: &mut I, parser: P) -> ParseResult<I, E, O>
where
    I: Input,
    P: ParserOnce<I, Merger<I, E>, (), (), Out = O>,
{
    let mut errors = Merger::<I, E>::new();
    let mut did_cut = false;
    let out = parser.run_once(In::new(input, &mut errors, IsCut::new(&mut did_cut)));
    ParseResult { out, errors, did_cut }
}

/// Error returned by [`parse_ok_once`].
pub struct ParseOkError<I: Input> {
    pub errors: Merger<I, StdErr<I::Item>>,
}

impl<I: Input> ParseOkError<I> {
    /// Best error range recorded by the merger.
    pub fn range(&self) -> Option<&std::ops::Range<I::Pos>> {
        self.errors.range()
    }

    /// Errors recorded for the best range.
    pub fn errors(&self) -> &[StdErr<I::Item>] {
        self.errors.errors()
    }

    /// Merge errors into a summary.
    pub fn merged_errors(&self) -> StdSummary<I::Item>
    where
        I::Item: Clone,
    {
        self.errors.merged().unwrap_or(StdSummary { unexpected: None, convert: None, expected: Vec::new() })
    }
}

impl<I> fmt::Debug for ParseOkError<I>
where
    I: Input,
    I::Item: Clone + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParseOkError").field("summary", &self.merged_errors()).finish()
    }
}

impl<I> fmt::Display for ParseOkError<I>
where
    I: Input,
    I::Item: Clone + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.merged_errors())
    }
}

impl<I> ::std::error::Error for ParseOkError<I>
where
    I: Input,
    I::Item: Clone + fmt::Debug,
{
}

/// Run an `Option<T>` parser with `StdErr` errors.
pub fn parse_ok_once<I, P>(input: &mut I, parser: P) -> Result<ValueOf<P, I, Merger<I, StdErr<I::Item>>, (), ()>, ParseOkError<I>>
where
    I: Input,
    P: ParserOnce<I, Merger<I, StdErr<I::Item>>, (), (), Out: OutOf<I, Merger<I, StdErr<I::Item>>>>,
{
    let r = parse_once::<_, StdErr<I::Item>, _, _>(input, parser);
    let Some(out) = <P::Out>::embed_result(r.out.as_result()) else {
        return Err(ParseOkError { errors: r.errors });
    };
    Ok(out)
}
