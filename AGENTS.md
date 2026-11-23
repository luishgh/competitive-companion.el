# AGENTS.md

Notes for coding agents working on `competitive-companion.el`.

## Layout

- `competitive-companion.el`: the whole package (single file).
- `test/competitive-companion-tests.el`: ERT suite.
- `test/competitive-companion-test-helpers.el`: fixtures (`cc-test-run`, `cc-test-make-program`, `cc-test-capturing-messages`).
- `doc/competitive-companion.org`: the user manual. Edit only this file; `doc/competitive-companion.texi` is regenerated from it.
- `README.org`: overview, installation, and the "Development" section for contributors.
- `guix.scm`: package definition, also used to run the tests.
- `integration-manifest.scm`: Guix manifest with the integration tests' dependencies.

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

### Integration tests (real browser)

`test/competitive-companion-integration-tests.el` drives Chrome with the
Competitive Companion extension (via Puppeteer, `test/integration/driver.mjs`)
on live Codeforces/AtCoder pages and judges real C++ solutions
(`test/integration/solutions/`). It needs the network, Node.js and g++, so
`make check` and `guix build` do not run it:

```sh
guix shell -m integration-manifest.scm -- make integration
```

- `make integration` runs `npm ci` and `fetch-extension.sh` (pinned release,
  checksum verified) first; downloads go to `test/integration/node_modules/`
  and `test/integration/.cache/`, both gitignored and excluded from
  `guix.scm`'s source.
- Codeforces serves a Cloudflare challenge to headless Chrome: that test is
  skipped unless `CC_INTEGRATION_HEADFUL=1`.
- Run it when touching the server, task processing or the test runner.

## Writing tests

- Work test-first: add a failing ERT test, watch it fail, then implement.
- Test programs are shell scripts written by `cc-test-make-program`; add reusable ones to `cc-test-programs`. Do not depend on a compiler.
- The shebang must come from `(executable-find "sh")`: Guix build containers have no `/bin/sh`.
- Everything a test creates (temp dirs, buffers, processes) must be cleaned up in `unwind-protect`.
- Server tests use `cc-test-with-server` (random port) and send raw HTTP
  with `cc-test-send-request`, which can split a request into chunks.
- Batch Emacs does not ignore SIGPIPE: a test that makes Emacs write to a
  dead program's stdin kills the whole run.
- Keep the suite fast (a few seconds). Timing-based tests use small limits; if one is flaky, widen its bounds instead of adding retries.
- Both files use the shorthand `cc-` for `competitive-companion-` (see their `Local Variables`).

## Conventions

- The package keeps no state on disk between Emacs sessions: tasks are re-fetched from the browser extension after a restart. Per-task data lives in buffer-local variables of the task buffer and is copied to its output buffer.
- Record user-visible changes in `CHANGELOG.org`.
- The manual is for users only: how to install, use and customize the
  package.  Development instructions (tests, tooling, how to contribute)
  go in the "Development" section of `README.org`, never in the manual.
