# Agent Guidelines

Guidance for AI coding agents working in this repo.

## What this is

A C++ NNTP server suite: a collection of cooperating binaries (typhoond,
hurricane, cyclone, hw_stormcellar, scbe2, jessica, nimbus, pileus, etc.) plus
shared libraries. Built with `-std=c++11 -Wall -Wextra -Werror`.

## Layout

- `cmd/<binary>/` — binaries, CLIs, and tools. One subdir per binary
  (e.g. `typhoond/`, `nntpcli/`, `tools/`).
- `lib/<module>/` — shared C++ libraries. One subdir per module; each is on
  the include path (see `compile_flags.txt`).
- `top/` — example configs (`*-EXAMPLE`), `CHANGES.<binary>` release notes,
  protocol/error files. Read `CHANGES.*` for binary-specific context, and
  append a line there for any user-visible change you make to a binary.
- `docs/` — per-subsystem design docs (cyclone, hurricane, hw_stormcellar,
  scbe2).
- `mtools/` — build system fragments (`master.mk`, `packages.mk`, ...).
- `systemtest/` — **integration tests**, written in PHP. Exercises the
  system end-to-end (multiple binaries cooperating). `systemtest/include/`
  holds shared `*.class.inc` helpers.
- `build/` — out-of-tree build dir; `Makefile` is a symlink to `../Makefile`.

**Unit tests live alongside the code they test**, in the same `cmd/<binary>/`
or `lib/<module>/` directory, with a `t_` prefix — e.g. `lib/article/t_article.C`
+ `lib/article/t_article.t`. When changing a module, look for `t_*` files in
that directory before adding a new test elsewhere.

**File extensions.** C++ source is `.C`, headers are `.H`, test descriptors
are `.t`. Not `.cpp`/`.hpp`/`.cc` — don't rename them or look for the
"missing" extension.

**Adding a source file.** Each module/binary has its own `Makefile`; new
`.C` files must be listed there or they won't be built. Always check the
local Makefile when you add a file.

## Building

**Do not run builds yourself.** A full build is slow and burns a lot of
tokens on output. Ask me to run the build and I'll paste back what you need
(errors, warnings, etc.). The same goes for `clean` and packaging.

Builds run inside a podman container, never on the host. For reference, the
commands you should run are:

```
bash .scripts/build.sh        # full build
bash .scripts/build.sh clean  # clean
```

## Tests

**Don't run the tests either.** Same reasoning as builds — they take a while
and the output is noisy. Ask me to run whatever's relevant (`systemtest/`,
per-module tests, etc.) and I'll paste back the failures or summary you need.

Reading test code to understand expected behavior is fine and encouraged.

Since you can't build or run tests yourself: if you need to verify syntax
or logic, reason through it carefully by reading the code — trace types,
ownership, and control flow across files rather than guessing. When you
genuinely need a build/test signal to be sure, ask for it instead of
shipping a "probably correct" change.

## Git

**I drive git, not you.** Do not commit, stage, push, pull, merge, rebase,
branch, or tag unless I explicitly ask. Read-only commands (`git status`,
`git log`, `git diff`, `git show`, `git blame`, etc) are fine whenever they
help you understand the code.

When you finish a change, leave the working tree dirty and tell me what
you changed - I'll handle staging and the commit message.

## Conventions

- Make small, focused changes — one logical change at a time, minimal diffs,
  no opportunistic refactoring or modernizing of unrelated code.
- Match the surrounding style of the file you're editing. Modules vary in
  age and idiom.
- Preserve existing API contracts and interfaces unless I explicitly ask you
  to change them.
- Don't add new dependencies without asking.
- The repo has in-tree predecessors to many STL types (`lib/auto_ptr`,
  `lib/scopedfd`, `lib/scopedbuf`, `lib/refcount`, `lib/simplestring`,
  `lib/simplelist`, etc.). My current preference is **STL** for new code,
  but call it out when you see one of these so we can decide together —
  some of them have semantics that aren't a clean swap for the STL version.
  - **Exceptions:** `string` (in `lib/string/`, ref-counted, *not*
    `std::string`) and `SUBSTR` (in `lib/miscutil/`, similar to
    `std::string_view` — yes, different module, this codebase is old).
    These are pervasive across the codebase and the ref-counting/lifetime
    semantics matter — keep using them, don't swap to `std::string` /
    `std::string_view`.
- Add a new shared module under `lib/<name>/` only when it's reused; one-off
  helpers live next to the binary that uses them.
- Treat warnings as errors. `-Werror` is on, so dead-code or unused-variable
  fixes are real fixes, not cosmetic.

## When you're unsure

- Ask before making architectural decisions.
- Ask before changing widely-included headers (`lib/<module>/*.H`) — every
  TU that includes them rebuilds, and with `-Werror` any new warning becomes
  a failure across many binaries.
- Prefer a clarifying question over an incorrect assumption.

## Don't touch

- Lines tagged `JULIOJULIOJULIO` are scratch debug logging. Leave them alone
  unless explicitly asked.
- `top/*-EXAMPLE` files are reference configs, not live config - don't edit
  them as part of code changes.
