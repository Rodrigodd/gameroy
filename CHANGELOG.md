# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0] - Unreleased

### Added

- Implement a experimental libretro port.
- Add thumbnails to ROM list UI.
- Increase interpreter performance by 4 times.
- Implement a dynamic recompiler, a.k.a. JIT compiler, which is 70% faster than
  the interpreted version. But the recompiler is not very optimized yet. It is
  only targeting x64.
- Delta-compress rewinding's save states.

### Changed

- Renamed packages from using `_` to `-`, like `gameroy_native ->
  gameroy-native`.

### Fixed

- Replace poorly maintained OpenGL backend by glutin.
- Fix resizing in Wayland.
- Fix `bench` command's "times faster" calculation.

## [0.2.0] - 2022-09-20

### Added

- add `gameroy bench` subcommand.
- add a table view for the rom loading UI.
- make UI dpi aware.
- add a WebAssembly port.
- add a Android port.
- add button to change the rom folder (only update config on Android).

### Fixed

- fix STOP instruction ([e2fb38d](https://github.com/Rodrigodd/gameroy/commit/e2fb38d4208164063c53433c513bd4afad5bb6c8))
- also load dropped file when already running a game.


## [0.1.1] - 2022-04-08

### Added

- Log version at startup.

### Fixed

- Don't panic if failed to initialize the audio backend.

## [0.1.0] - 2022-04-06
