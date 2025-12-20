pub mod std;

use ::std::{collections::VecDeque, ops::Range};

use crate::input::inner::Input;
use reborrow_generic::short::Rb;

/// Merge multiple errors into a single summary value.
pub trait MergeErrors: Sized {
    type Output;
    fn merge(errors: &[Self]) -> Self::Output;
}

/// Error-mode abstraction that defines the parser output type and rollback handling.
pub trait ErrMode<I: Input>: Rb + Sized {
    type Out<O, E>: OutOf<I, Self, Value = O>;
    type Checkpoint: Clone;
    /// Lift a value into this error mode.
    fn value<T, E>(value: T) -> Self::Out<T, E>;
    /// Map over the output value.
    fn map<S, T, E>(this: Self::Out<S, E>, f: impl FnOnce(S) -> T) -> Self::Out<T, E>;
    /// Monadic bind for the output value.
    fn and_then<S, T, E>(this: Self::Out<S, E>, f: impl FnOnce(S) -> Self::Out<T, E>) -> Self::Out<T, E>;
    /// Take a checkpoint for this error mode.
    fn checkpoint<'a>(this: Self::Target<'a>) -> Self::Checkpoint;
    /// Roll back to a checkpoint.
    fn rollback<'a>(this: Self::Target<'a>, checkpoint: Self::Checkpoint);
    /// Commit accumulated errors (default no-op).
    fn commit<'a>(_this: Self::Target<'a>) {}
    /// Retag an error payload into a different output type.
    fn retag<O1, O2, E>(e: Result<O1, <Self::Out<O2, E> as OutOf<I, Self>>::Error>) -> Self::Out<O1, E>;
    /// Whether soft failure should roll back input to the checkpoint.
    fn rollback_on_soft_failure() -> bool {
        true
    }
}
impl<'a, I: Input, EO> ErrMode<I> for Merger<I, EO> {
    type Out<O, E> = Option<O>;
    type Checkpoint = usize;
    fn value<T, E>(value: T) -> Self::Out<T, E> {
        Some(value)
    }
    fn map<S, T, E>(this: Option<S>, f: impl FnOnce(S) -> T) -> Option<T> {
        this.map(f)
    }
    fn and_then<S, T, E>(this: Self::Out<S, E>, f: impl FnOnce(S) -> Self::Out<T, E>) -> Self::Out<T, E> {
        this.and_then(f)
    }
    fn checkpoint<'b>(this: &'b mut Merger<I, EO>) -> Self::Checkpoint {
        this.checkpoint()
    }
    fn rollback<'b>(this: &'b mut Merger<I, EO>, checkpoint: Self::Checkpoint) {
        this.rollback(checkpoint);
    }
    fn commit<'b>(this: &'b mut Merger<I, EO>) {
        this.commit();
    }
    fn retag<O1, O2, E>(e: Result<O1, ()>) -> Self::Out<O1, E> {
        e.ok()
    }
    fn rollback_on_soft_failure() -> bool {
        true
    }
}
impl<I: Input> ErrMode<I> for () {
    type Out<O, E> = Result<O, E>;
    type Checkpoint = ();
    fn value<T, E>(value: T) -> Self::Out<T, E> {
        Ok(value)
    }
    fn map<S, T, E>(this: Result<S, E>, f: impl FnOnce(S) -> T) -> Result<T, E> {
        this.map(f)
    }
    fn and_then<S, T, E>(this: Self::Out<S, E>, f: impl FnOnce(S) -> Self::Out<T, E>) -> Self::Out<T, E> {
        this.and_then(f)
    }
    fn checkpoint(_this: ()) {}
    fn rollback(_this: (), _checkpoint: ()) {}

    fn retag<O1, O2, E>(e: Result<O1, E>) -> Result<O1, E> {
        e
    }
    fn rollback_on_soft_failure() -> bool {
        false
    }
}

/// Error sink for pushing errors into the current mode.
pub trait ErrSink<I: Input, EI>: ErrMode<I> {
    /// Push an error with an explicit range.
    fn push<'a, O>(this: Self::Target<'a>, range: Range<I::Pos>, error: EI) -> Self::Out<O, EI>;
    /// Push an error at the current position.
    fn push_at_current<'a, O>(this: Self::Target<'a>, error: EI) -> Result<Self::Out<O, EI>, EI>;
}
impl<I: Input, EI: Into<EO>, EO> ErrSink<I, EI> for Merger<I, EO> {
    fn push<'b, O>(this: &'b mut Merger<I, EO>, range: Range<I::Pos>, error: EI) -> Option<O> {
        this.push(range, error.into());
        None
    }
    fn push_at_current<'a, O>(this: Self::Target<'a>, error: EI) -> Result<Self::Out<O, EI>, EI> {
        this.push_at_current(error).map(|_| None)
    }
}
impl<I: Input, E> ErrSink<I, E> for () {
    fn push<'a, O>(_this: (), _range: Range<I::Pos>, error: E) -> Result<O, E> {
        Err(error)
    }
    fn push_at_current<'a, O>(_this: Self::Target<'a>, error: E) -> Result<Self::Out<O, E>, E> {
        Err(error)
    }
}

/// Bridge between `ErrMode::Out` and a concrete output type.
pub trait OutOf<I: Input, M: ErrMode<I>>: Into<M::Out<Self::Value, Self::Error>> + From<M::Out<Self::Value, Self::Error>> {
    type Value;
    type Error;

    /// Whether this output represents success.
    fn is_ok(&self) -> bool;
    /// Whether this output represents failure.
    fn is_err(&self) -> bool;
    /// Attempt to recover from an error.
    fn recover(self, f: impl FnOnce(Self::Error) -> Result<Self::Value, Self::Error>) -> Self;
    /// Convert into a plain `Result`.
    fn as_result(self) -> Result<Self::Value, Self::Error>;
    /// Construct from a plain `Result`.
    fn from_result(res: Result<Self::Value, Self::Error>) -> Self;
    /// Project `ErrMode::Out` into a plain `Result`.
    fn project_result<O>(this: M::Out<O, Self::Error>) -> Result<O, Self::Error>;
    /// Embed a plain `Result` back into `ErrMode::Out`.
    fn embed_result<O>(this: Result<O, Self::Error>) -> M::Out<O, Self::Error>;
}
impl<I: Input, O, EO> OutOf<I, Merger<I, EO>> for Option<O> {
    type Value = O;
    type Error = ();

    fn is_ok(&self) -> bool {
        self.is_some()
    }

    fn is_err(&self) -> bool {
        self.is_none()
    }

    fn recover(self, f: impl FnOnce(()) -> Result<O, ()>) -> Self {
        match self {
            Some(o) => Some(o),
            None => f(()).ok(),
        }
    }
    fn as_result(self) -> Result<Self::Value, Self::Error> {
        self.ok_or(())
    }
    fn from_result(res: Result<Self::Value, Self::Error>) -> Self {
        res.ok()
    }

    fn project_result<T>(this: Option<T>) -> Result<T, Self::Error> {
        this.ok_or(())
    }
    fn embed_result<T>(this: Result<T, Self::Error>) -> Option<T> {
        this.ok()
    }
}
impl<I: Input, O, EO> OutOf<I, ()> for Result<O, EO> {
    type Value = O;
    type Error = EO;

    fn is_ok(&self) -> bool {
        self.is_ok()
    }

    fn is_err(&self) -> bool {
        self.is_err()
    }

    fn recover(self, f: impl FnOnce(EO) -> Result<O, EO>) -> Self {
        match self {
            Ok(o) => Ok(o),
            Err(e) => f(e),
        }
    }
    fn as_result(self) -> Result<Self::Value, Self::Error> {
        self
    }
    fn from_result(res: Result<Self::Value, Self::Error>) -> Self {
        res
    }

    fn project_result<T>(this: Result<T, Self::Error>) -> Result<T, Self::Error> {
        this
    }
    fn embed_result<T>(this: Result<T, Self::Error>) -> Result<T, Self::Error> {
        this
    }
}

/// Aggregate and merge errors by span.
pub struct Merger<I: Input, E> {
    range: Option<Range<I::Pos>>,
    errors: Vec<E>,
    group_start: usize,
    undo: VecDeque<Undo<I::Pos>>,
    base_index: usize,
}
#[derive(Debug)]
enum Undo<Pos> {
    SetNone { old_len: usize },
    PopError,
    Replace { old_range: Option<Range<Pos>>, old_group_start: usize, old_len: usize },
}
impl<I: Input, E> Rb for Merger<I, E> {
    type Target<'short>
        = &'short mut Merger<I, E>
    where
        Self: 'short;
    fn rb<'short>(&'short mut self) -> &'short mut Merger<I, E> {
        self
    }
    fn shorten<'short, 'long: 'short>(this: &'long mut Merger<I, E>) -> &'short mut Merger<I, E> {
        this
    }
    fn shorten_mut<'short, 'long>(this: &'short mut &'long mut Merger<I, E>) -> &'short mut Merger<I, E> {
        this
    }
}

impl<I: Input, E> Merger<I, E> {
    /// Create an empty merger.
    pub fn new() -> Self {
        Self { range: None, errors: Vec::new(), group_start: 0, undo: VecDeque::new(), base_index: 0 }
    }
    /// Current best error range (if any).
    pub fn range(&self) -> Option<&Range<I::Pos>> {
        self.range.as_ref()
    }
    /// Errors for the current best range.
    pub fn errors(&self) -> &[E] {
        match self.range {
            None => &[],
            Some(_) => &self.errors[self.group_start..],
        }
    }
    /// Push an error at the current best range.
    pub fn push_at_current<EI: Into<E>>(&mut self, error: EI) -> Result<(), EI> {
        if self.range.is_none() {
            return Err(error);
        }
        self.errors.push(error.into());
        self.undo.push_back(Undo::PopError);
        Ok(())
    }
    /// Push an error with an explicit range.
    pub fn push(&mut self, range: Range<I::Pos>, error: E) {
        match &self.range {
            None => {
                let old_len = self.errors.len();
                self.range = Some(range);
                // 新しいグループを開始する（末尾に積む）
                self.group_start = self.errors.len();
                self.errors.push(error);
                self.undo.push_back(Undo::SetNone { old_len });
            }
            Some(current) => {
                let cur_key = (&current.start, &current.end);
                let new_key = (&range.start, &range.end);
                match new_key.cmp(&cur_key) {
                    ::std::cmp::Ordering::Less => {}
                    ::std::cmp::Ordering::Equal => {
                        self.errors.push(error);
                        self.undo.push_back(Undo::PopError);
                    }
                    ::std::cmp::Ordering::Greater => {
                        let old_range = self.range.replace(range);
                        let old_group_start = self.group_start;
                        let old_len = self.errors.len();
                        // 新しいグループを開始（古いグループは `errors` 内に残す）
                        self.group_start = self.errors.len();
                        self.errors.push(error);
                        self.undo.push_back(Undo::Replace { old_range, old_group_start, old_len });
                    }
                }
            }
        }
    }

    fn checkpoint(&self) -> usize {
        self.base_index + self.undo.len()
    }
    fn rollback(&mut self, checkpoint: usize) {
        // 日本語メモ:
        // `commit()` 後はそれ以前の checkpoint に戻れません。
        // もし古い checkpoint が来たら、戻せる範囲まで（= base_index）に丸めます。
        let checkpoint = checkpoint.max(self.base_index);
        // 日本語メモ:
        // `checkpoint` は “push index” です。つまり「このindex以降に積まれた undo を全部戻す」です。
        // `VecDeque` なので後ろから pop して逆操作を適用します。
        while self.base_index + self.undo.len() > checkpoint {
            match self.undo.pop_back().expect("checked by loop condition") {
                Undo::SetNone { old_len } => {
                    self.range = None;
                    self.errors.truncate(old_len);
                    self.group_start = self.errors.len();
                }
                Undo::PopError => {
                    self.errors.pop();
                }
                Undo::Replace { old_range, old_group_start, old_len } => {
                    self.range = old_range;
                    self.errors.truncate(old_len);
                    self.group_start = old_group_start;
                }
            }
        }
    }
    /// Commit accumulated errors and discard older groups.
    pub fn commit(&mut self) {
        self.base_index += self.undo.len();
        self.undo.clear();
        if self.range.is_none() {
            self.errors.clear();
            self.group_start = 0;
        } else if self.group_start > 0 {
            self.errors.drain(0..self.group_start);
            self.group_start = 0;
        }
    }

    /// Merge current errors using [`MergeErrors`].
    pub fn merged(&self) -> Option<E::Output>
    where
        E: MergeErrors,
    {
        let errors = self.errors();
        if errors.is_empty() { None } else { Some(E::merge(errors)) }
    }
}
