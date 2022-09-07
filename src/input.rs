use std::{
    fmt::{self, Display},
    hash::Hash,
};

use super::error::{unexpected, EndOfInput, Unexpected};

pub trait Input: Clone {
    type Token: Clone;
    type Message;
    type Position: Position;

    fn uncons(&mut self) -> Result<Self::Token, Self::Message>;
    fn position(&self) -> Self::Position;
}

pub trait Seq: Input<Message = Unexpected<EndOfInput>>
where
    Self::Token: Hash + Eq,
{
}
impl<I: Input<Message = Unexpected<EndOfInput>>> Seq for I where I::Token: Hash + Eq {}

pub trait Save {
    type Savepoint;
    fn save(&mut self) -> Self::Savepoint;
    fn load(&mut self, savepoint: Self::Savepoint);
}
impl<T: Clone> Save for T {
    type Savepoint = Self;
    #[inline(always)]
    fn save(&mut self) -> Self::Savepoint {
        self.clone()
    }
    #[inline(always)]
    fn load(&mut self, savepoint: Self::Savepoint) {
        *self = savepoint
    }
}

pub trait Position {
    type Offset: Ord;
    fn offset(&self) -> Self::Offset;
}
impl<P: Position> Position for &P {
    type Offset = P::Offset;
    #[inline(always)]
    fn offset(&self) -> Self::Offset {
        P::offset(self)
    }
}

pub struct Ranged<Position, T> {
    pub start: Option<Position>,
    pub end: Position,
    pub item: T,
}

pub trait PositionPrinter: Position + Sized {
    fn print<F: Display>(ranged: &Ranged<&Self, F>, f: &mut fmt::Formatter) -> fmt::Result;
}
impl<P: PositionPrinter> PositionPrinter for &P {
    fn print<F: Display>(ranged: &Ranged<&Self, F>, f: &mut fmt::Formatter) -> fmt::Result {
        let Ranged { start, end, item } = ranged;
        P::print(&Ranged { start: start.as_ref().map(|s| **s), end, item }, f)
    }
}
impl<P: PositionPrinter, T: Display> Display for Ranged<P, T> {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Ranged { start, end, item } = self;
        P::print(&Ranged { start: start.as_ref(), end, item }, f)
    }
}

impl Input for &str {
    type Token = char;
    type Message = Unexpected<EndOfInput>;
    type Position = Self;

    #[inline(always)]
    fn uncons(&mut self) -> Result<char, Unexpected<EndOfInput>> {
        let mut chars = self.chars();
        match chars.next() {
            Some(c) => {
                *self = chars.as_str();
                Ok(c)
            },
            None => Err(unexpected(EndOfInput)),
        }
    }

    #[inline(always)]
    fn position(&self) -> Self::Position {
        self
    }
}
impl Position for &str {
    type Offset = usize;

    #[inline(always)]
    fn offset(&self) -> Self::Offset {
        self.as_ptr() as usize
    }
}
impl PositionPrinter for &str {
    #[inline(always)]
    fn print<F: Display>(Ranged { start: _, end: _, item }: &Ranged<&Self, F>, f: &mut fmt::Formatter) -> fmt::Result {
        item.fmt(f)
    }
}
#[derive(Clone, Copy)]
pub struct PositionString<'a> {
    pos: usize,
    str: &'a str,
}
impl<'a> PositionString<'a> {
    pub fn new(str: &'a str) -> Self {
        Self { pos: 1, str }
    }
}
pub fn pos_str<'a>(str: &'a str) -> PositionString<'a> {
    PositionString { pos: 1, str }
}

impl<'a> Input for PositionString<'a> {
    type Token = char;
    type Message = Unexpected<EndOfInput>;
    type Position = usize;

    #[inline(always)]
    fn uncons(&mut self) -> Result<char, Unexpected<EndOfInput>> {
        let mut chars = self.str.chars();
        match chars.next() {
            Some(c) => {
                self.str = chars.as_str();
                self.pos += 1;
                Ok(c)
            },
            None => Err(unexpected(EndOfInput)),
        }
    }
    #[inline(always)]
    fn position(&self) -> Self::Position {
        self.pos
    }
}
impl<'a> Position for usize {
    type Offset = usize;

    fn offset(&self) -> Self::Offset {
        *self
    }
}
impl<'a> PositionPrinter for usize {
    #[inline(always)]
    fn print<F: Display>(Ranged { start, end, item }: &Ranged<&Self, F>, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(start) = start {
            if start != end {
                return write!(f, "{} at {}-{}", item, start, end);
            }
        }
        write!(f, "{} at {}", item, end)
    }
}

#[derive(Clone, Copy)]
pub struct LineString<'a> {
    line_start: &'a str,
    line: usize,
    str: &'a str,
}
impl<'a> LineString<'a> {
    pub fn new(str: &'a str) -> Self {
        LineString { line_start: str, line: 0, str }
    }
}
impl<'a> Into<usize> for LineString<'a> {
    fn into(self) -> usize {
        self.line_start[0..self.str.offset() - self.line_start.offset()].chars().count() + 1
    }
}

impl<'a> Input for LineString<'a> {
    type Token = char;
    type Message = Unexpected<EndOfInput>;
    type Position = Self;

    #[inline(always)]
    fn uncons(&mut self) -> Result<char, Unexpected<EndOfInput>> {
        let mut chars = self.str.chars();
        match chars.next() {
            Some(c) => {
                self.str = chars.as_str();
                if c == '\n' {
                    self.line += 1;
                    self.line_start = self.str;
                }
                Ok(c)
            },
            None => Err(unexpected(EndOfInput)),
        }
    }
    #[inline(always)]
    fn position(&self) -> Self::Position {
        *self
    }
}
impl<'a> Position for LineString<'a> {
    type Offset = usize;

    #[inline(always)]
    fn offset(&self) -> Self::Offset {
        self.str.offset()
    }
}
impl<'a> PositionPrinter for LineString<'a> {
    #[inline(always)]
    fn print<F: Display>(Ranged { start, end, item }: &Ranged<&Self, F>, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(start) = start {
            if start.line != end.line {
                return write!(
                    f,
                    "{} at {}:{}-{}:{}",
                    item,
                    start.line + 1,
                    start.line_start[0..start.str.offset() - start.line_start.offset()].chars().count() + 1,
                    end.line + 1,
                    end.line_start[0..end.str.offset() - end.line_start.offset()].chars().count() + 1
                );
            }
            if start.str.offset() != end.str.offset() {
                return write!(
                    f,
                    "{} at {}:{}-{}",
                    item,
                    start.line + 1,
                    start.line_start[0..start.str.offset() - start.line_start.offset()].chars().count() + 1,
                    end.line_start[0..end.str.offset() - end.line_start.offset()].chars().count() + 1
                );
            }
        }
        return write!(
            f,
            "{} at {}:{}",
            item,
            end.line + 1,
            end.line_start[0..end.str.offset() - end.line_start.offset()].chars().count() + 1,
        );
    }
}
