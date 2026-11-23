# AGENTS.md

Notes for coding agents working on `competitive-companion.el`.

## Layout

- `competitive-companion.el`: the whole package (single file).
- `test/competitive-companion-tests.el`: ERT suite.
- `test/competitive-companion-test-helpers.el`: fixtures (`cc-test-run`, `cc-test-make-program`, `cc-test-capturing-messages`).
- `doc/competitive-companion.org`: the manual. Edit only this file; `doc/competitive-companion.texi` is regenerated from it.
- `guix.scm`: package definition, also used to run the tests.

## Running the tests

The canonical way is a Guix build, which runs `make check` in its check phase:

```sh
guix build -f guix.scm
```

For faster iteration:

```sh
guix shell emacs emacs-magit coreutils bash -- make check
```

A change is not done until `guix build -f guix.scm` succeeds.

## Writing tests

- Work test-first: add a failing ERT test, watch it fail, then implement.
- Test programs are shell scripts written by `cc-test-make-program`; add reusable ones to `cc-test-programs`. Do not depend on a compiler.
- The shebang must come from `(executable-find "sh")`: Guix build containers have no `/bin/sh`.
- Everything a test creates (temp dirs, buffers, processes) must be cleaned up in `unwind-protect`.
- Keep the suite fast (a few seconds). Timing-based tests use small limits; if one is flaky, widen its bounds instead of adding retries.
- Both files use the shorthand `cc-` for `competitive-companion-` (see their `Local Variables`).

## Conventions

- The package keeps no state on disk between Emacs sessions: tasks are re-fetched from the browser extension after a restart. Per-task data lives in buffer-local variables of the task buffer and is copied to its output buffer.
- Record user-visible changes in `CHANGELOG.org`.
