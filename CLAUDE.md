# tunnelpascal

An FPC fork (reports version 3.3.1) adding Delphi-parity features. See
`docs/delphi-parity/FEATURE-COMPARISON.md` and `docs/delphi-parity/PLAN.md`.

## Building the compiler

Fast iteration (~10s, one stage) — use an existing `compiler/ppc1` as bootstrap:

```sh
cd compiler
rm -f *.wpo                       # stale WPO feedback aborts the build, see below
make compiler FPC=$PWD/ppc1 OS_TARGET=linux CPU_TARGET=x86_64
# produces compiler/ppcx64
```

Full validation (~10 min, 3 stages, rebuilds the RTL):

```sh
cd compiler && rm -f *.wpo && make clean cycle OS_TARGET=linux CPU_TARGET=x86_64
```

A completed cycle is the strongest signal for a codegen change: the compiler
compiles itself twice. Verify the fixpoint with `cmp ppc2 ppc3` — they should
differ only in ~2 bytes of the embedded version-banner date. Any other
difference means nondeterministic codegen.

**Gotchas that will waste your time:**

- `make: 'ppc3' is up to date` — stale artifacts from an old build. It is lying.
- `Unit ... compiled using a different whole program optimization feedback
  input` — leftover `*.wpo`. `rm -f compiler/*.wpo`.
- Switching between modified and pristine compiler sources requires
  `rm -rf compiler/x86_64/units`, otherwise the build dies with
  `Error: Compilation raised exception internally`. The `.ppu` files on disk
  were produced by the *other* version of the source.
- `make clean` in `compiler/` also deletes `ppc1..3` and the built RTL. If you
  need a pristine baseline binary later, copy it aside first.

## Running the test suite

```sh
cd tests
make create_c_objects TEST_FPC=/abs/path/to/compiler/ppcx64   # once; needed by test/cg/*
make allexectests     TEST_FPC=/abs/path/to/compiler/ppcx64
```

`TEST_FPC` is **required** and must be absolute — without it the Makefile aborts
immediately with `*** ERROR: TEST_FPC is missing ***`. Passing `FPC=` is not
enough. Results land in `tests/output/x86_64-linux/log`; count them with
`grep -c '^Successfully compiled'` / `'^Failed to compile'` / `'^Failed to run'`.

### Before investigating any failure, check `.github/known_failures.txt`

That file is the CI gate (`.github/workflows/test.yml`) and lists tests that
**also fail on a pristine upstream FPC build**. Roughly 10 tests fail on a clean
tree; they are all listed there. Diff your failures against it before concluding
you broke something:

```sh
comm -23 <(your_failures|sort -u) <(grep -vE '^\s*#|^\s*$' .github/known_failures.txt|sort -u)
```

Known local-environment extras not on that list (they pass in CI):

- `test/cg/tcalext*`, `test/cg/tcppcl*` — fail unless `create_c_objects` has run.
- `test/units/linux/tepoll1.pp` — environmental (container/kernel epoll).
- `webtbf/*` tests are *expected-to-fail-compilation*; a failure there is a pass
  and won't appear in the raw log's failure lines.

Only build a pristine baseline (stash changes, `rm -rf compiler/x86_64/units`,
rebuild) if a failure is **not** on the known list.

## Testing language features — traps

Several plausible-looking reductions silently test nothing:

- `var s := 'x'` infers **Char**, not string — a 1-char literal never exercises
  the managed-type path. Use a 2+ character literal.
- Without `{$H+}` (or Delphi mode, which implies it) `string` is a
  **ShortString**, which is unmanaged. "It works without `{$mode delphi}`"
  usually means the managed path was skipped, not that mode matters.
- **A passing run does not mean correct codegen.** An uninitialised managed
  local only faults when the stack slot happens to be non-zero. Confirm with
  `-al` and check the generated `.s`: a managed local needs `movq $0,-N(%rbp)`
  before first use and a matching `fpc_*_decr_ref`. Count them rather than
  trusting exit 0.
- Runtime error 216 in a loop of ~1100 frames = SIGSEGV, usually a refcounted
  type being assigned over uninitialised memory.

## Inline variables

Implemented in `pstatmnt.pas` (`inline_var_const_statement`), originally from
the `wfeus` remote (`fpc-with-inline-variables`).

Inline vars have **scope lifetime**: `close_nested_block()` in `pstatmnt.pas`
emits initialisation on block entry and finalisation on block exit, wrapped in
an implicit try/finally so exceptions, `exit`, `break` and `continue` still
finalise. Symbols it handles are flagged `has_scope_lifetime` (transient, not in
the PPU) so the per-procedure passes in `ngenutil.pas` skip them and nothing is
initialised twice.

Every `enter_nested_block` must be paired with `close_nested_block(node)` rather
than a bare `exit_nested_block` — a missed site leaves managed vars
uninitialised, which only crashes intermittently.

Classic `var`-section variables keep **procedure** lifetime; Pascal has no
block-level var section. Tests: `tests/test/tinlinevarscope1.pp` (block scopes,
plus a classic-var contrast check) and `tinlinevarscope2.pp` (branch statements
without begin/end).

## Compiler Explorer

`tp191` is the published build and is a **dotted-units** variant: `System.SysUtils`,
not `SysUtils`. The compiler does not predefine `FPC_DOTTEDUNITS`, so sources
using `{$IFDEF FPC_DOTTEDUNITS}` must be compiled with `-dFPC_DOTTEDUNITS`
explicitly or they fail with a bare `Can't find unit SysUtils`.
