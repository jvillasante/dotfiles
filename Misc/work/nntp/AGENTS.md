# Agent Guidelines

Guidance for AI coding agents working in this repo.

## What this is

A C++ NNTP server suite: a collection of cooperating binaries (typhoond,
hurricane, cycloned, hw_stormcellard, scbe2, jessica, nimbus, pileus, adapter,
etc.) plus static libraries. Not purely C++ — `cmd/geobalancer/` is a Python
application. Supports a wide range of Rocky Linux targets (from version 7 to 10)
and also builds on Darwin/macOS.

**Daemon vs product naming.** The `cmd/` subdir (and the built binary) usually
carries a trailing `d` (`cycloned`, `hw_stormcellard`, `typhoond`); the
product/package names and `docs/`/`top/` paths drop it (`cyclone`,
`hw_stormcellar`, `typhoon`). Both forms refer to the same thing. `tornado_be`
(Stormchaser backend, built from `cmd/scbe2/`) is another product with its own
CHANGES, README, and config files in `top/`.

**Compiler flags.** `CXXWARN = -Wall -Werror -Wshadow -Wextra` (see
`mtools/master.mk`). The C++ standard is selected per toolchain in
`mtools/master.mk`:

- GCC 4.8.1–7.x → `-std=c++11`
- GCC 8.x → `-std=c++2a` (early C++20 flag)
- GCC 9+ → `-std=c++20`
- Darwin/macOS (clang) → always `-std=c++11`

Code must compile on all supported toolchains, so the lowest common denominator
is C++11; don't reach for C++14/17/20 features without checking that they work
everywhere.

## Layout

- `cmd/<binary>/`: binaries, CLIs, and tools. Typically one subdir per binary
  (e.g. `cmd/adapter/`, `cmd/typhoond/`, `cmd/hurricane/`). Exception:
  `cmd/tools/` is a flat directory of many standalone `.C` tools sharing one
  `Makefile`.
- `lib/<module>/`: C++ libraries (~226 modules). One subdir per module; each is
  on the include path and statically linked into one of two archive libraries:
  `libhws.a` (main) or `libhwsext.a` (extended). Nearly all modules go into
  `libhws.a`; check `lib/Makefile` for the split. Exception: `lib/build/` is not
  a C++ library — it contains CI/build infrastructure scripts (`BUILDSCRIPT`,
  `CIBUILD`, etc.); skip it when searching for code.
- `top/`: example configs (`*-EXAMPLE`), `CHANGES.<product>` release notes,
  protocol/error files, version stamps (`release.txt`, `minor_release.txt`),
  etc.
- `docs/`: per-subsystem example configuration files (cyclone, hurricane,
  hw_stormcellar, scbe2). These are reference configs, not design documentation.
- `misc/`: assorted Perl helpers and standalone `.C` utilities (`fastReader.C`,
  `multiReader.C`, etc.) that don't belong to a specific binary.
- `mtools/`: build system fragments (`master.mk`, `packages.mk`, etc).
- `systemtest/`: **integration tests**, written in PHP. Exercises the system
  end-to-end (multiple binaries cooperating). `systemtest/include/` holds shared
  `*.class.inc` helpers; tests live under `systemtest/tests/nntp/<subsystem>/`
  (subsystem is sometimes a binary name, sometimes a product name).
- `build/`: out-of-tree build dir; `Makefile` is a symlink to `../Makefile`.
  `make` must be run from here, not from the source tree (the root `Makefile`
  enforces this via `CHECK_SOURCE_TREE`). The `build/headers/` subdir (or
  `headers/` in-tree) is a symlink farm of every `*.h`, `*.hpp`, `*.H`, `*.def`
  found under `lib/`, populated by `make headers`. This is how cross-module
  `#include "foo.H"` resolves, so a new header in `lib/<module>/` needs `make
  headers` rerun before other modules can find it.

## Code organization

**File extensions.** C++ source is `.C`, headers are `.H`, test descriptors are
`.t`. Not `.cpp`/`.hpp`/`.cc`; don't rename them or look for the "missing"
extension.

**Unit tests live alongside the code they test**, in the same `cmd/<binary>/` or
`lib/<module>/` directory, with a `t_` prefix; e.g. `lib/article/t_article.C`
+ `lib/article/t_article.t`. Test descriptors (`.t`) are run by `cmd/tester/`
(`tester` and `testjig` scripts); a passing test produces a `.T` marker file.
When changing a module, look for `t_*` files in that directory before adding a
new test elsewhere.

**Adding a source file.** Each module/binary has its own `Makefile`; new `.C`
files must be listed there or they won't be built. Always check the local
`Makefile` when you add a file.

**Adding a header.** New `.H` files in `lib/<module>/` are picked up by `make
headers` and symlinked into `build/headers/`. Until that runs, other modules
won't see the header.

**`local.mk` overrides.** Per-developer or per-machine build overrides go in
`local.mk` (see `mtools/local.mk-EXAMPLE` for available toggles: C++ standard,
gcov, gprof, tcmalloc, SSL, etc.). Don't create or modify this file without
asking.

## Building

**Do not run builds yourself.** A full build is slow and burns a lot of tokens
on output. Ask me to run the build and I'll paste back what you need (errors,
warnings, etc.). The same goes for `clean` and packaging.

**When you ask for a build,** say which target and what output you want (full
log, just errors, warnings count, etc.) so I can keep the paste short.

**Build details.** ccache is used automatically when available. tcmalloc is
enabled by default on Linux. On Darwin/macOS, the toolchain is clang/clang++
(not gcc/g++), `jessica` and `scbe2` are excluded, and C++ is locked to C++11.
Parallel recursive builds are supported via `PARALLEL_SUBDIRS=1`.

## Tests

**Don't run the tests either.** Same reasoning as builds; they take a while and
the output is noisy. Ask me to run whatever is relevant (integration-tests,
unit-tests, etc.) and I'll paste back the failures or summary you need.

**When you ask for a test run,** say which test(s) and whether you want full
output, the failure summary, or just pass/fail.

Reading test code to understand expected behavior is fine and encouraged.

Since you can't build or run tests yourself, if you need to verify syntax or
logic, reason through it carefully by reading the code; trace types, ownership,
and control flow across files rather than guessing. When you genuinely need a
build/test signal to be sure, ask for it instead of shipping a "probably
correct" change.

## Git

**I drive git, not you.** Do not commit, stage, push, pull, merge, rebase,
branch, or tag unless I explicitly ask. Read-only commands (`git status`, `git
log`, `git diff`, `git show`, `git blame`, etc) are fine whenever they help you
understand the code.

When you finish a change, leave the working tree dirty and tell me what you
changed, I'll handle staging, commits, etc.

**Worktree layout.** This repo uses git worktrees. The bare repository lives in
`.bare/` (one level above the current directory) - never touch it. The `master`
branch is a worktree; feature branches are additional worktrees that are
siblings to `master`. Always work in the current directory - it may be `master`
or any branch worktree. Do not assume the repo root is one level up or attempt
to navigate to `.bare/` or sibling worktrees.

## Process

- Make small, focused changes; one logical change at a time, minimal diffs, no
  opportunistic refactoring or modernizing of unrelated code.
- Preserve existing API contracts and interfaces unless I explicitly ask you to
  change them.
- Don't add new dependencies without asking.
- Add a new module under `lib/<name>/` only when it's reused; one-off helpers
  live next to the binary that uses them. Don't forget the tests when adding a
  new module.
- When in doubt, ask - and that includes when something in the existing code
  looks "wrong" to you. Your sense of incorrect may just be unfamiliarity with
  the codebase's idioms.

## Style

- Match the surrounding style of the file you're editing. Modules vary in age
  and idioms; consistency with the existing code matters more than any
  individual preference. No automated formatting is enforced (no `.clang-format`
  or `.editorconfig`), so style is maintained by convention.
- The repo has in-tree predecessors to many STL types (`lib/auto_ptr`,
  `lib/scopedfd`, `lib/scopedbuf`, `lib/refcount`, `lib/simplestring`,
  `lib/simplelist`, etc.). My current preference is **STL** for new code, but
  call it out when you see one of these so we can decide together; some of them
  have semantics that aren't a clean swap for the STL version.
- **Exceptions**: keep using these in-tree types, don't swap them for the STL
  lookalike:
  - `string` (in `lib/string/`): ref-counted, *not* `std::string`. Pervasive
    across the codebase; the ref-counting/lifetime semantics matter.
  - `SUBSTR` (in `lib/miscutil/`): similar to `std::string_view`, but predates
    it and is pervasive across the codebase.

## Quality

- Treat warnings as errors. `-Werror` is on (along with `-Wall -Wextra
  -Wshadow`), so dead-code, unused-variable, and shadowed-variable fixes are
  real fixes, not cosmetic.
- After consistency, **simplicity** is the highest value, prefer the simple
  solution unless it conflicts with an established pattern.

## Don't touch

- Lines tagged `JULIOJULIOJULIO` are scratch debug logging. Leave them alone
  unless explicitly asked.
