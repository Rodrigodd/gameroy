# CLAUDE.md

## Project

GameRoy is a Nintendo Game Boy (DMG) emulator and debugger written in Rust, with an
emphasis on accuracy: it syncs on the clock frequency itself rather than on frames or
sound.

It is a Cargo workspace. The members are the emulation core, an x86_64 recompiler, an
emulation server and a Slint front-end; see `Cargo.toml` for the current list and each
crate's own `Cargo.toml` for what it does. Some older front-ends also live in this tree
but are outside the workspace — the comment at the top of `Cargo.toml` says why.

The front-end has its own guidance in
[`gameroy-slint/CLAUDE.md`](gameroy-slint/CLAUDE.md). Read it before working in that
crate.

## Build and run

```sh
cargo build --workspace
cargo run -p gameroy-slint [path/to/rom.gb]
```

ROMs are not in the repository. `roms/` is gitignored; put test ROMs there.

## Tests

```sh
cargo test --workspace
cargo test -p <crate>          # one crate
```

Tests that need a ROM skip themselves when it is absent rather than failing, so a clean
checkout still passes. Emulator accuracy is covered by test ROMs, which have to be
downloaded separately — see `README.md`.

Prefer tests that assert observable behaviour over tests that mirror the implementation;
the latter have to be rewritten every time the code moves.

## Formatting and lints

```sh
cargo fmt --all
cargo clippy --workspace --all-targets
```

`.rustfmt.toml` sets import options that are nightly-only. `cargo fmt` on stable works
but warns and skips them; `cargo +nightly fmt --all` applies them.

Keep new code clippy-clean. The older crates still carry warnings — don't treat those as
a licence to add more, and don't bulk-fix them in an unrelated change.

## Comments

Comment the *why*, not the *what*. Code already says what it does; a comment earns its
place by explaining a decision, a constraint, or a non-obvious consequence — especially
where the straightforward approach was rejected for a reason that isn't visible locally.

- Don't restate the code (`// increment i`), and don't leave commented-out code behind.
- Use `///` doc comments on public items, and on anything whose contract isn't obvious
  from its signature.
- Prefer making the code self-explanatory to explaining confusing code.
- If a comment describes a specific line number, file, or current implementation detail
  elsewhere, it will rot. Describe the invariant instead.

## Commits

Conventional Commits, and **the subject line must stay under 62 characters**:

```
fix(ui): break binding loop in screen image layout
```

Types in use: `feat`, `fix`, `refactor`, `docs`, `test`, `chore`, `build`. The scope
tracks the tree (a crate, or an area within one). Put the detail in the body, not the
subject — the body is the right place for what changed and why, wrapped at ~76 columns.

Commit only when asked, and keep unrelated changes in separate commits.

End AI-assisted commit messages with:

```
AI-assisted: Claude Code
```
