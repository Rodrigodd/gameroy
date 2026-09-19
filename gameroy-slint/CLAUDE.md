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
- `ui/` — the Slint files. `app-window.slint` is the window shell and nothing else: it
  sizes the window, routes keys, and picks a screen. Each screen and reusable widget is
  its own component in its own file.
- `ui/state.slint` — the whole contract between the host and the UI, as exported
  globals.

Prefer adding a new component file over growing an existing one.

## Where a component's data comes from

Two kinds of component, and they get their data differently.

**Screens and panels read a global.** Slint exposes a property to Rust only if it is on
the compiled root or on an exported global, so routing everything through the root is
what once forced each screen's data to be declared on the window, again on every
component in between, and once more where it was used. A global reaches any depth with
no help from its ancestors. Add the properties and callbacks to `ui/state.slint`, and
**export the global from `app-window.slint`** — that re-export is what creates
`ui.global::<X>()`, and without it the global silently has no Rust API.

**Reusable widgets take properties and report through callbacks.** `GameScreen`,
`SpriteGrid`, `TileMapView` and the file-private helpers know nothing about the
emulator. Keep them that way: they are the ones worth previewing and reasoning about
alone.

If a value is a component's own business — a splitter position, a fold's state — it is a
property on that component, not part of the contract.

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
renders a file headlessly. Several layout mistakes are invisible in the source and
obvious in a picture.

A widget that takes properties is previewed by wrapping it in a small throwaway file
that supplies sample values. A screen that reads a global is previewed with
`--load-data`:

```sh
slint-viewer --component Library --load-data sample.json ui/library.slint
```

`--load-data` only reaches a global exported from the file being previewed, which is why
each screen file re-exports the global it reads. Note that `--on` cannot set a global
callback, so a button's handler has to be exercised in the real app.

`slint-viewer --auto-reload <file>` gives a live preview while editing.

A `slint` skill is available covering the language, layout, and its common pitfalls, and
the Slint documentation is the authority on element and widget signatures. Consult them
rather than guessing at an API.
