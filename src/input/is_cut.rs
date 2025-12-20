//! `cut` marker plumbing.
//!
//! `IsCut` is a small helper that stores a mutable `bool` and an `is_root` flag.
//!
//! - `cut()` sets the flag.
//! - `capture_cut(f)` runs `f` and reports whether `f` performed a `cut`.
//!
//! The `is_root` flag is used by [`crate::input::In::cut`] to decide whether to call
//! [`crate::input::inner::Input::commit`].
//!
//! ## Nested observation
//!
//! When the flag is already `true`, `capture_cut` runs `f` with an isolated (non-root) marker so
//! that the caller can still observe whether `f` would cut *inside* the scope, without changing
//! the already-cut outer state.

use reborrow_generic::Reborrow;

/// Marker for cut propagation and root commit behavior.
#[derive(Reborrow)]
pub struct IsCut<'a> {
    pub(crate) is_root: bool,
    pub(crate) is_cut: &'a mut bool,
}
impl<'a> IsCut<'a> {
    /// Create a non-root cut marker (never triggers `commit`).
    pub fn non_root(is_consumed: &'a mut bool) -> Self {
        IsCut { is_root: false, is_cut: is_consumed }
    }

    /// Create a root cut marker (eligible to trigger `commit`).
    pub fn new(is_consumed: &'a mut bool) -> Self {
        IsCut { is_root: true, is_cut: is_consumed }
    }

    /// Mark this scope as cut.
    pub fn cut(&mut self) {
        *self.is_cut = true;
    }

    /// Run `f` while capturing whether it performed a cut.
    ///
    /// Returns `(output, did_cut_inside_scope)`.
    pub fn capture_cut<O>(&mut self, f: impl FnOnce(IsCut) -> O) -> (O, bool) {
        if *self.is_cut {
            let mut is_consumed = false;
            return (f(IsCut::non_root(&mut is_consumed)), is_consumed);
        } else {
            let out = f(IsCut { is_root: self.is_root, is_cut: self.is_cut });
            (out, *self.is_cut)
        }
    }
}
