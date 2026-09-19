# CLAUDE.md

Front-end–specific guidance. The workspace-wide conventions — commits, comments,
formatting, lints — are in [`../CLAUDE.md`](../CLAUDE.md) and apply here too.

## This crate

`gameroy-slint` is the user interface: windowing, rendering, audio output, input, and
all filesystem access. It is one half of a client/server split.

The other half is `gameroy-server`, which owns the emulator. The two communicate only by
passing owned, serializable messages over a transport — they share no mutable state.
That is deliberate: it is what allows the server to run somewhere else later (another
process, or a web worker) and what allows a different front-end to be written against
the same protocol.

Two consequences worth keeping in mind whenever you touch the boundary:

- **The front-end owns the filesystem.** The server deals in bytes. Anything involving a
  path, a read or a write belongs here, not there.
- **The server must stay usable without this crate.** `gameroy-server` has a guard
  script that asserts its protocol still builds free of the emulator; run it after
  changing the protocol.

## Layout

- `src/` — application state and the event loop wiring, plus one module per concern
  (input, audio, storage, config, the ROM library, the server connection).
- `ui/` — the Slint files. The root window is deliberately thin: it handles focus and
  keys and picks a screen. Each screen and reusable widget is its own component in its
  own file.

Prefer adding a new component file over growing an existing one. A component that takes
its data as properties and reports back through callbacks can be previewed and reasoned
about on its own.

## Build, run, test

Run these from the workspace root:

```sh
cargo run -p gameroy-slint [path/to/rom.gb]
cargo test -p gameroy-slint
cargo test -p gameroy-server            # the emulation path is covered here
./gameroy-server/scripts/check-boundary.sh
```

Logging is via `env_logger`; set `RUST_LOG=debug` for more detail.

Most front-end logic worth testing lives in plain modules (formatting, sorting, key
mapping, config) rather than in callbacks, so it can be unit-tested without a window.
Keep it that way — code inside a Slint callback is code that cannot be tested headlessly.

## Working on the UI

After editing a `.slint` file, check it before building:

```sh
slint-viewer --check gameroy-slint/ui/app-window.slint
```

It is much faster than `cargo build` and reports the same diagnostics.

**Never call UI work done without looking at a render.** `slint-viewer --screenshot`
renders a file headlessly, and a component written to take its data as properties can be
previewed by wrapping it in a small throwaway file that supplies sample values. Several
layout mistakes are invisible in the source and obvious in a picture.

`slint-viewer --auto-reload <file>` gives a live preview while editing.

A `slint` skill is available covering the language, layout, and its common pitfalls, and
the Slint documentation is the authority on element and widget signatures. Consult them
rather than guessing at an API.
