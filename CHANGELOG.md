# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.0] - 2025-12-20

### Breaking Changes

This release switches the core error/rollback model to **`ErrMode`**, matching the experimental
design from `chasa-experiment`. Parsers are now parameterized by an error mode that controls the
output type and rollback policy.

#### ErrMode-based output and rollback
- **Parser traits now depend on `ErrMode`** (`ParserOnce/Mut/Parser<I, E, N, L>`), not just
  `Option<T>`-style soft failure.
- **Output is no longer fixed to `Option<T>`**:
  - `Merger<I, E>` mode => `Out = Option<T>` (soft failure rolls back).
  - `()` mode => `Out = Result<T, E>` (soft failure does **not** roll back).
- **Soft-failure rollback is now controlled by `ErrMode::rollback_on_soft_failure()`** instead of
  being uniform across the library.
- Errors and rollback are explicitly tied: only combinators that roll back input also roll back
  errors.

#### Error API re-organization
- Error core moved under `input::error` and is now split into:
  - `ErrMode` / `ErrSink` / `OutOf` (mode + output bridge)
  - `Merger` (error accumulation with rollback + commit)
  - `std` error types (`StdErr`, `StdSummary`)
- Any code that directly used `chasa::error::*` must update imports.

#### Behavior differences vs 0.2.x
- `lookahead(p)` now **rolls back only on success**; on failure it may consume input and keep errors.
- `not(p)` now **succeeds only on soft failure**, but may consume input on inner success or cut-failure.
- These changes make lookahead more permissive and closer to the new rollback model.

#### Removed or redesigned APIs
- Removed: `parse`, `parse_mut`, `parse_ok`, `parse_ok_mut`, `TestableParser::test_ok`.
  - Use `parse_once` / `parse_ok_once` or call `ParserOnce::run_once` on `In` directly.
- Removed: `many_ok*` family (Result-based repetition).
  - Use `ErrMode=()` (Result output) with `many`/`many_map`/`many1` instead.
- Removed: `map_out*` / `map_out_mut` / `map_out_once`.
  - Use `map`/`bind` and mode-aware output via `ErrMode`.
- Renamed: `cut_on_success` => `cut_if_ok` (and `ParserOnce::cut()` uses it internally).
- Removed: `then::Out` wrapper and `out` helper; tuples now short-circuit directly.
- Removed: `flow_opt` / `flow_ok`.
  - Use `flow_many` / `flow_many_map` or mode-aware loops with `ErrMode`.
- Removed: `In::maybe_ok` and `In::uncut`.
  - Use `ErrMode=()` with `maybe` / `no_cut`.
- Removed: `parser::set` module.
  - Use `parser::item::set` (`ItemSet`) instead.

### Added

- **`ErrMode` / `ErrSink` / `OutOf`** for mode-driven outputs and error handling.
- **Mode-aware parsing helpers** in `input::error`, including `OutOf::as_result` and `embed_result`.

### Changed

- `parse_ok_once` now accepts any parser whose output implements `OutOf` under `Merger` mode.
  The returned error type no longer includes `did_cut`.
- `ParseOkError` no longer stores `did_cut` (use `parse_once` if you need cut tracking).
- Documentation updated to emphasize explicit rollback behavior and `ErrMode` semantics.
- `parser::prim::Choice` moved to `parser::choice` (and `choice` module is now public).
- `error` module is reorganized under `input::error` (`ErrMode`, `ErrSink`, `OutOf`, `Merger`, `std`).
- Prelude no longer re-exports streaming input types (`StreamInput`, `BufReadInput`, `IndexPos`).

## [0.2.0] - 2024-12-18

### Breaking Changes

This release represents a complete redesign of the chasa parser combinator library. The core design philosophy has shifted from continuation-passing style to explicit `In` wrapper-based parsing with streaming support.

#### Removed Features
- **`.case()` pattern matching combinator** - Use `choice` or standard Rust `match` instead
- **Continuation-style API** (`Args`, `Cont` types) - Replaced by `In` wrapper methods
- **`fold` / `fold1`** - Replaced by `flow`, `flow_opt`, `flow_ok` combinators
- **`extend` / `extend1`** - Use `many` / `many_map` instead
- **`sep_fold` / `sep_fold1`** - Use `sep_map` instead
- **`tail_rec`** - No longer needed; functions directly implement parser traits
- **`run()` combinator** - Functions are parsers by default
- **`Ranged` combinator** - Replaced by `with_range` (requires `with_counter`)
- **`GetString` / `GetStringExtend`** - Use `with_seq` instead
- **`OrWith`** - Removed
- **`pure_or`** - Removed
- **`repeat(n)`** - Replaced by `count(n)` with more flexible range support

#### Renamed/Redesigned
- Parser output conventions now prefer `Option<T>` for soft failure
- Error handling redesigned around `Merger` with rollback support
- Input abstraction redesigned with `Input` trait and streaming support

### Added

#### Core Features
- **Streaming input support** via `StreamInput` - parse large files without loading everything into memory
- **Explicit `cut` control** - `cut` is now a branch-pruning signal with root-level commit triggers
- **Combinator-driven rollback** - explicit control over when input is rolled back
- **Error rollback** - errors are rolled back together with input during backtracking
- **`In` wrapper** - bundles input, error accumulator, and cut flag for clean parser composition

#### New Combinators
- **`count(range, parser)`** - repeat a parser a specified number of times
  - Supports exact counts: `count(4, p)` 
  - Range counts: `count(2..5, p)`, `count(2..=5, p)`, `count(2.., p)`
  - `CountRange` trait for flexible range specifications
- **`with_range(parser)`** - attach position range to parse results (requires `with_counter`)
  - Useful for error reporting, source maps, and syntax highlighting
- **`label_with(parser, f)`** - lazily-evaluated error labels
  - Only generates labels on failure, improving performance when label generation is expensive
- **`flow` / `flow_opt` / `flow_ok`** - stateful loop abstractions using `ControlFlow`
- **`many_ok` family** - repetition for `Result<T, E>` parsers with error accumulation
- **`maybe` / `maybe_ok`** - explicit try-with-rollback wrappers

#### Input System
- **`Counter` trait** - track position/state during parsing
- **`usize` Counter implementation** - simple position tracking
- **`with_counter(counter)`** - wrap inputs with position tracking
- **`WithCounter` helper methods** - `inner()`, `inner_mut()`, `counter()`
- **`StreamInput`** - lazy input from iterators with commit support
- **`BufReadInput`** - streaming from `BufRead` sources

#### Parser Traits
- **Three-level trait hierarchy**: `ParserOnce`, `ParserMut`, `Parser`
- **`Reborrow` support** - via `reborrow-generic` for efficient borrowing
- **Environment and local state** - `In<I, E, N, L>` supports user-defined state

### Changed
- **Minimum Rust version**: 1.85 (edition 2024)
- **Dependencies**: 
  - Added `reborrow-generic` 0.1.1
  - Added `seq-macro` 0.3.6
  - Added `copy-range` 0.1.1
  - Removed `either`
- **Error types**: Now use `StdErr` and `StdSummary` for standard error handling
- **Whitespace parsers**: `ws` and `ws1` now built-in for char inputs
- **Method chaining**: All major combinators available as methods via traits

### Documentation
- Completely rewritten README with emphasis on `cut`, streaming, and rollback semantics
- Added TL;DR section for quick start
- Added "What makes chasa different?" section highlighting unique features
- Comprehensive doctests for all public APIs
- Clear migration path from Parsec-style combinators

### Migration Guide from 0.1.x

#### Pattern Matching (`.case()`)
```rust
// 0.1.x
any.case(|c, k| match c {
    '{' => k.then(object),
    '[' => k.then(array),
    _ => k.fail(unexpected(c))
})

// 0.2.x - Use choice or direct match
fn parser(mut i: In<&str>) -> Option<Value> {
    i.choice((
        item('{').right(object),
        item('[').right(array),
    ))
}
```

#### Folding
```rust
// 0.1.x
fold(init, parser, |acc, x| acc + x)

// 0.2.x
parser.flow_opt(init, |acc, x| ControlFlow::Continue(acc + x), |acc| acc)
```

#### Repetition with Count
```rust
// 0.1.x
satisfy(is_hex_digit).repeat(4)

// 0.2.x
one_of("0123456789abcdefABCDEF").count(4)
```

#### Position Tracking
```rust
// 0.1.x
parser.ranged()

// 0.2.x
let mut input = "text".with_counter(0usize);
parser.with_range()
```

## [0.1.13] - Previous Release

Previous version with continuation-passing style API. See [GitHub repository](https://github.com/momota1029/chasa) for 0.1.x documentation.

[0.4.0]: https://github.com/momota1029/chasa/releases/tag/v0.4.0
[0.2.0]: https://github.com/momota1029/chasa/releases/tag/v0.2.0
[0.1.13]: https://github.com/momota1029/chasa/releases/tag/v0.1.13
