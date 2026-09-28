# Changelog

Notable changes to ob-zig.el. When a `v<version>` tag is pushed, the release
workflow publishes the matching section of this file as the release notes.

## 0.2.0 - 2026-09-28

First release of the fork. Requires Zig 0.16 and Emacs 30.1.

### Breaking

- Blocks without `pub fn main` are wrapped in
  `pub fn main(init: std.process.Init) !void`, so `init` and `init.io` are
  available to the body.
- Scalar `:var` values are emitted as `const` and are comptime-known.
- `:using-namespaces` is gone with the `usingnamespace` keyword.
- Emacs 30.1 or later is required.

### Fixed

- `:flags`, `:libs` and `:cmdline` reach the compiler as
  `zig run FLAGS LIBS FILE -- ARGS`. They were silently dropped.
- `:c-includes` and `:c-defines` emit a real `@cImport` block bound to `c`
  and link libc.
- Table column helpers work for any column count, quote numeric header cells,
  import `std` themselves, and panic with "unknown column" on a bad name.
- String variables are escaped, floats keep full precision, and
  single-character symbols stay strings.
- Main detection ignores `pub fn main` inside comments.
- `:session` signals an error instead of being ignored.

### Changed

- zig-mode is no longer required or depended on. Org picks the edit mode
  through `major-mode-remap`.
- The test suite is a self-contained ERT file that runs real blocks through
  Babel. `ob-zig-test.org` and `test-ob-zig-runner.el` are removed.
- README rewritten for the new wrapper and header arguments.
