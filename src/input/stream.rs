//! Streaming input implementations.
//!
//! This module provides `Input` implementations that do **not** require the whole input to be
//! available up-front. They keep a rollback buffer and support `commit()` to discard already
//! accepted prefixes (triggered by a root `cut`).
//!
//! The key guarantee is:
//! - `checkpoint()` / `rollback()` only need to work for data **since the last commit**.
//! - A root-level `cut` triggers `Input::commit()` (see [`crate::input::In::cut`]), meaning older
//!   checkpoints become unreachable by design.

use std::{collections::VecDeque, io::BufRead};

use crate::input::inner::{Back, Input};

/// A simple index-based position for streaming inputs.
///
/// This is primarily used for span ordering in [`crate::input::error::Merger`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IndexPos(pub u64);

/// Streaming input backed by an iterator.
///
/// Items are pulled lazily from the iterator and stored in an internal buffer so the input can
/// roll back. Calling `commit()` discards the already-consumed prefix of that buffer.
///
/// Notes:
/// - This type does **not** implement [`crate::input::inner::SeqInput`], because its checkpoints
///   do not carry enough information to reconstruct a consumed slice without access to the live
///   buffer.
pub struct StreamInput<It>
where
    It: Iterator,
    It::Item: Clone,
{
    iter: It,
    buf: VecDeque<It::Item>,
    cursor: usize,
    base_index: u64,
}

impl<It> StreamInput<It>
where
    It: Iterator,
    It::Item: Clone,
{
    /// Create a new streaming input from an iterator.
    pub fn new(iter: It) -> Self {
        Self { iter, buf: VecDeque::new(), cursor: 0, base_index: 0 }
    }

    /// Number of items currently buffered (since the last commit).
    pub fn buffered_len(&self) -> usize {
        self.buf.len()
    }

    /// Current committed base index.
    pub fn committed_index(&self) -> u64 {
        self.base_index
    }

    /// Current cursor within the buffer.
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    /// Take back the underlying iterator.
    pub fn into_inner(self) -> It {
        self.iter
    }
}

impl<It> Back for StreamInput<It>
where
    It: Iterator,
    It::Item: Clone,
{
    /// Absolute index (monotonic, even across commits).
    type Checkpoint = u64;

    fn checkpoint(&mut self) -> Self::Checkpoint {
        self.base_index + (self.cursor as u64)
    }

    fn rollback(&mut self, checkpoint: Self::Checkpoint) {
        let checkpoint = checkpoint.max(self.base_index);
        let rel = (checkpoint - self.base_index) as usize;
        self.cursor = rel.min(self.buf.len());
    }
}

impl<It> Input for StreamInput<It>
where
    It: Iterator,
    It::Item: Clone,
{
    type Pos = IndexPos;
    type Item = It::Item;

    fn index(&self) -> u64 {
        self.base_index + (self.cursor as u64)
    }

    fn pos(&self) -> Self::Pos {
        IndexPos(self.index())
    }

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.buf.len() {
            let item = self.iter.next()?;
            self.buf.push_back(item);
        }
        let out = self.buf.get(self.cursor).cloned();
        self.cursor += 1;
        out
    }

    fn commit(&mut self) {
        if self.cursor == 0 {
            return;
        }
        self.base_index += self.cursor as u64;
        for _ in 0..self.cursor {
            self.buf.pop_front();
        }
        self.cursor = 0;
    }
}

/// Streaming input backed by a [`BufRead`].
///
/// This input yields bytes (`u8`). IO errors are treated as end-of-input.
pub struct BufReadInput<R: BufRead> {
    reader: R,
    buf: VecDeque<u8>,
    cursor: usize,
    base_index: u64,
}

impl<R: BufRead> BufReadInput<R> {
    /// Create a streaming input from a buffered reader.
    pub fn new(reader: R) -> Self {
        Self { reader, buf: VecDeque::new(), cursor: 0, base_index: 0 }
    }

    /// Number of bytes currently buffered (since the last commit).
    pub fn buffered_len(&self) -> usize {
        self.buf.len()
    }

    /// Current committed base index.
    pub fn committed_index(&self) -> u64 {
        self.base_index
    }

    /// Take back the underlying reader.
    pub fn into_inner(self) -> R {
        self.reader
    }

    fn read_one(&mut self) -> Option<u8> {
        let slice = self.reader.fill_buf().ok()?;
        if slice.is_empty() {
            return None;
        }
        let b = slice[0];
        self.reader.consume(1);
        Some(b)
    }
}

impl<R: BufRead> Back for BufReadInput<R> {
    type Checkpoint = u64;

    fn checkpoint(&mut self) -> Self::Checkpoint {
        self.base_index + (self.cursor as u64)
    }

    fn rollback(&mut self, checkpoint: Self::Checkpoint) {
        let checkpoint = checkpoint.max(self.base_index);
        let rel = (checkpoint - self.base_index) as usize;
        self.cursor = rel.min(self.buf.len());
    }
}

impl<R: BufRead> Input for BufReadInput<R> {
    type Pos = IndexPos;
    type Item = u8;

    fn index(&self) -> u64 {
        self.base_index + (self.cursor as u64)
    }

    fn pos(&self) -> Self::Pos {
        IndexPos(self.index())
    }

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.buf.len() {
            let b = self.read_one()?;
            self.buf.push_back(b);
        }
        let out = self.buf.get(self.cursor).copied();
        self.cursor += 1;
        out
    }

    fn commit(&mut self) {
        if self.cursor == 0 {
            return;
        }
        self.base_index += self.cursor as u64;
        for _ in 0..self.cursor {
            self.buf.pop_front();
        }
        self.cursor = 0;
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::prelude::*;

//     #[test]
//     fn stream_input_commit_drops_prefix_on_root_cut() {
//         let mut input = super::StreamInput::new("abc".chars());
//         let p = item('a').cut().right(item('b'));

//         let out = parse_ok_once(&mut input, p).unwrap();
//         assert_eq!(out, 'b');
//         assert_eq!(input.committed_index(), 1);
//         assert_eq!(input.buffered_len(), 1); // only 'b' remains buffered
//         assert_eq!(input.cursor(), 1);
//     }

//     #[test]
//     fn stream_input_rollback_restores_cursor_within_buffer() {
//         let mut input = super::StreamInput::new("abc".chars());
//         let cp0 = input.checkpoint();
//         assert_eq!(input.next(), Some('a'));
//         assert_eq!(input.next(), Some('b'));
//         let cp2 = input.checkpoint();
//         assert_eq!(cp2, cp0 + 2);

//         input.rollback(cp0);
//         assert_eq!(input.next(), Some('a'));
//         assert_eq!(input.next(), Some('b'));
//     }
// }
