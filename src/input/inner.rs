//! Low-level input traits.
//!
//! This module defines the abstraction that parsers consume.
//!
//! - [`Input`] provides `next()` and a position type `Pos`.
//! - [`Back`] provides `checkpoint()` / `rollback()` for local backtracking.
//! - [`Input::commit`] allows streaming inputs to discard already-accepted prefixes.
//!
//! Important: `commit` is triggered by `cut` only in a **root** cut scope
//! (see [`crate::input::In::cut`]).

use std::marker::PhantomData;

use reborrow_generic::Reborrow as Rb;

/// Core input trait consumed by parsers.
pub trait Input: Back {
    /// Position type used for error spans and ordering.
    type Pos: Ord;
    /// Item type returned by [`Input::next`].
    type Item: Clone;

    /// A monotonic index used for internal ordering.
    ///
    /// This is used by label-related errors (e.g. [`crate::input::error::std::Expected`]) to select the most
    /// specific label. It does not have to be human-readable.
    fn index(&self) -> u64;
    /// Current position.
    fn pos(&self) -> Self::Pos;
    /// Consume one item from the input.
    fn next(&mut self) -> Option<Self::Item>;
    /// Commit already-accepted input.
    ///
    /// Inputs that can drop prefixes (e.g. streaming readers) can implement this to free memory.
    /// For in-memory inputs like `&str`, this is typically a no-op.
    fn commit(&mut self);

    /// Wrap this input with a counter to track position.
    ///
    /// This is useful for tracking byte/character positions in inputs like `&str`.
    ///
    /// ## Examples
    ///
    /// ```
    /// use chasa::prelude::*;
    ///
    /// let mut input = "hello".with_counter(0usize);
    /// assert_eq!(input.pos(), 0);
    /// input.next();
    /// assert_eq!(input.pos(), 1);
    /// ```
    fn with_counter<C: Counter<Self::Item>>(self, counter: C) -> WithCounter<Self, C>
    where
        Self: Sized,
    {
        WithCounter { input: self, counter }
    }
}

/// Rollback support for local backtracking.
pub trait Back {
    /// A clonable checkpoint that can restore the input to an earlier state.
    type Checkpoint: Clone;
    /// Create a rollback checkpoint.
    fn checkpoint(&mut self) -> Self::Checkpoint;
    /// Roll back the input to a previously created checkpoint.
    fn rollback(&mut self, checkpoint: Self::Checkpoint);
}
impl<T: Clone> Back for T {
    type Checkpoint = T;
    fn checkpoint(&mut self) -> Self::Checkpoint {
        self.clone()
    }
    fn rollback(&mut self, checkpoint: Self::Checkpoint) {
        *self = checkpoint;
    }
}

/// Reborrow-friendly variant of [`Back`].
pub trait RbBack: Rb {
    /// A clonable checkpoint that can restore the input to an earlier state.
    type Checkpoint: Clone;
    /// Create a rollback checkpoint.
    fn checkpoint<'a>(this: Self::Target<'a>) -> Self::Checkpoint;
    /// Roll back the input to a previously created checkpoint.
    fn rollback<'a>(this: Self::Target<'a>, checkpoint: Self::Checkpoint);
}
impl<'a, T: Back> RbBack for &'a mut T {
    type Checkpoint = T::Checkpoint;
    fn checkpoint<'b>(this: &'b mut T) -> Self::Checkpoint {
        this.checkpoint()
    }
    fn rollback<'b>(this: &'b mut T, checkpoint: Self::Checkpoint) {
        this.rollback(checkpoint);
    }
}
impl RbBack for () {
    type Checkpoint = ();
    fn checkpoint<'a>(_: ()) -> Self::Checkpoint {}
    fn rollback<'a>(_: (), _: Self::Checkpoint) {}
}

/// Slice-like access for inputs with stable checkpoints.
///
/// This is used to retrieve "the chunk between two checkpoints" as a value `T`.
pub trait SeqInput<T>: Input {
    fn seq(start: Self::Checkpoint, end: Self::Checkpoint) -> T;
}

#[derive(Debug, Hash)]
/// An address-based offset used as `Pos` for `&str` input.
pub struct Offset<S: ?Sized>(pub usize, PhantomData<fn() -> S>);
impl<S: ?Sized> PartialOrd for Offset<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<S: ?Sized> Ord for Offset<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
impl<S: ?Sized> PartialEq<Offset<S>> for Offset<S> {
    fn eq(&self, other: &Offset<S>) -> bool {
        self.0 == other.0
    }
}
impl<S: ?Sized> Eq for Offset<S> {}
impl<S: ?Sized> Clone for Offset<S> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}
impl<S: ?Sized> Copy for Offset<S> {}

impl<'a> Input for &'a str {
    type Item = char;
    type Pos = Offset<char>;

    fn index(&self) -> u64 {
        // Use remaining length to derive a stable, monotonic (increasing) index.
        // This does not require storing the initial base pointer/length.
        u64::MAX - (self.len() as u64)
    }

    /// Consume one `char` from the front of the string.
    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.chars();
        let c = chars.next();
        *self = chars.as_str();
        c
    }
    /// Current position represented by the string's pointer value.
    fn pos(&self) -> Self::Pos {
        Offset(self.as_ptr() as usize, PhantomData)
    }
    /// No-op for `&str`.
    fn commit(&mut self) {}
}
impl<'a> SeqInput<&'a str> for &'a str {
    /// Return the prefix of `start` that was consumed to reach `end`.
    fn seq(start: Self::Checkpoint, end: Self::Checkpoint) -> &'a str {
        &start[0..(start.len() - end.len())]
    }
}

impl<'a, T> Input for &'a [T]
where
    T: Clone,
{
    type Item = T;
    type Pos = Offset<T>;

    fn index(&self) -> u64 {
        // Use remaining length to derive a stable, monotonic (increasing) index.
        u64::MAX - (self.len() as u64)
    }

    fn pos(&self) -> Self::Pos {
        Offset(self.as_ptr() as usize, PhantomData)
    }

    fn next(&mut self) -> Option<Self::Item> {
        let first = self.first()?.clone();
        *self = &self[1..];
        Some(first)
    }

    fn commit(&mut self) {}
}

impl<'a, T> SeqInput<&'a [T]> for &'a [T]
where
    T: Clone,
{
    fn seq(start: Self::Checkpoint, end: Self::Checkpoint) -> &'a [T] {
        &start[0..(start.len() - end.len())]
    }
}

/// Position counter fed with each consumed item.
pub trait Counter<Item>: Back {
    type Pos: Ord;
    /// Feed one consumed item into the counter.
    fn feed(&mut self, item: Item);
    /// Current position derived from the counter state.
    fn pos(&self) -> Self::Pos;
}

/// Wrap an input with a user-defined counter that tracks position/state.
///
/// The counter is fed each consumed item and provides `pos()` for the wrapper.
pub struct WithCounter<I, C> {
    /// The underlying input.
    pub input: I,
    /// The counter state.
    pub counter: C,
}

impl<I, C> WithCounter<I, C> {
    /// Get a reference to the underlying input.
    pub fn inner(&self) -> &I {
        &self.input
    }

    /// Get a mutable reference to the underlying input.
    pub fn inner_mut(&mut self) -> &mut I {
        &mut self.input
    }

    /// Get a reference to the counter.
    pub fn counter(&self) -> &C {
        &self.counter
    }
}

impl<I, C> Back for WithCounter<I, C>
where
    I: Input,
    C: Counter<I::Item>,
{
    type Checkpoint = (I::Checkpoint, C::Checkpoint);
    fn checkpoint(&mut self) -> Self::Checkpoint {
        (self.input.checkpoint(), self.counter.checkpoint())
    }
    fn rollback(&mut self, point: Self::Checkpoint) {
        self.input.rollback(point.0);
        self.counter.rollback(point.1);
    }
}

impl<I, C> Input for WithCounter<I, C>
where
    I: Input,
    C: Counter<I::Item>,
{
    type Item = I::Item;
    type Pos = C::Pos;

    fn index(&self) -> u64 {
        self.input.index()
    }

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.input.next()?;
        self.counter.feed(item.clone());
        Some(item)
    }

    fn pos(&self) -> Self::Pos {
        self.counter.pos()
    }

    /// Delegate `commit` to the underlying input.
    fn commit(&mut self) {
        self.input.commit();
    }
}
impl<T, I, C> SeqInput<T> for WithCounter<I, C>
where
    I: SeqInput<T>,
    C: Counter<I::Item>,
{
    fn seq(start: Self::Checkpoint, end: Self::Checkpoint) -> T {
        I::seq(start.0, end.0)
    }
}

/// Simple counter implementation for `usize` that counts consumed items.
///
/// This is useful for tracking byte/character positions without additional state.
impl<T> Counter<T> for usize {
    type Pos = usize;

    fn feed(&mut self, _item: T) {
        *self += 1;
    }

    fn pos(&self) -> Self::Pos {
        *self
    }
}
