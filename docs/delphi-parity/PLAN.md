# Delphi 12/13 Language-Parity Plan (tunnelpascal)

Status: **items 1–5, 8, 9 landed; 6–7 outstanding.**
All file:line anchors below were captured when this plan was written and should be
re-verified before editing (line numbers drift — several already have).

| # | Item | Status |
|---|---|---|
| 1 | Inline `if` expression | ✅ landed (`tests/test/tinlineif1-3.pp`) |
| 2 | `NameOf` intrinsic | ✅ landed (`tnameof1-3.pp`) — vars/fields/types; routines & enum elements unverified |
| 3 | `interface` / `unmanaged` constraints | ✅ landed (`tgenconstr1-4.pp`) — see deviation note below |
| 4 | Default-mode switch flips | ✅ landed — `m_type_helpers` and (via item 8) `m_implicit_function_specialization` both on by default in Delphi mode |
| 5 | Extended method/field RTTI | ✅ landed (`textrtti1.pp`), on by default fork-wide |
| 6 | Compiler directive polish | ❌ not started |
| 7 | `System.Net.HttpClient` | ❌ not started |
| 8 | Implicit function specialization by default | ✅ landed — overload-probing crash fixed, switch added to `delphimodeswitches` |
| 9 | Inline variable **scope lifetime** | ✅ landed (PR #22, `tinlinevarscope1-2.pp`) — not in the original plan |

**Deviations from what was planned below, worth knowing before trusting the detail:**

- **Item 3** was implemented without adding `gcf_interface`. The enum in
  `compiler/symconst.pas` gained only `gcf_unmanaged`; the `interface` constraint
  is expressed as "any interface" via `IInterface` rather than a new flag. The
  PPU version *was* bumped as anticipated (`CurrentPPUVersion = 209`).
- **Item 4** was split. Type helpers landed first; implicit function
  specialization was held back by a compiler crash in overload probing and
  tracked as item 8. Both are now in `delphimodeswitches` (`compiler/globals.pas`)
  and item 8 has landed.
- **Item 9** was not foreseen at all. It surfaced from a crash report (issue #21)
  and turned out to be a lifetime-model gap rather than the local fix the issue
  implied. See the section at the end.

Two limitations found while validating the landed work, both documented in
`FEATURE-COMPARISON.md` rather than tracked as plan items:

- `TRttiMethod.Invoke` raises `ENotImplemented` unless `ffi.manager` (from the
  shipped `libffi` package) is in `uses`. Delphi needs no such step.
- The dotted-units build does not predefine `FPC_DOTTEDUNITS`, so
  `{$IFDEF FPC_DOTTEDUNITS}` cannot detect the RTL variant without
  `-dFPC_DOTTEDUNITS` being passed by hand.

## Context

The fork already has, in **default Delphi mode** (`compiler/globals.pas:53-59`):
generics + constraints, anonymous functions + `reference to` function references,
advanced records with management operators, custom attributes, multiline string
literals, digit separators, inline `var` declarations with type inference, and
custom managed records. That covers essentially everything from Delphi 2009
through **Delphi 12 Athens**.

Since this plan was written the Florence syntax items (1–3), the two
pre-existing gaps that mattered more in practice (4, 5), the mode-switch flip
for implicit function specialization (8), and the inline-variable scope work (9)
have all landed. What remains is directive polish (6) and a library item (7).

The per-item sections below are kept as originally written, with a status line
added at the top of each. Treat the "Current state" and "Implementation" text in
a landed item as a record of the plan, **not** a description of the tree — item 3
in particular was implemented differently from its plan.

This document orders the work **language features first, library utilities last**,
as requested. Priority (impact) is called out per item separately from ordering.

Legend — Effort: S (≤1 day), M (2–4 days), L (1–2 weeks).

---

## 1. Inline `if` expression (ternary)  — D13

**Status: ✅ landed.** Tests: `tests/test/tinlineif1.pp`–`tinlineif3.pp`.

**Priority: High (flagship D13 syntax).  Effort: M.  Risk: Medium (grammar).**

### Goal
Allow `if`/`then`/`else` in expression position, yielding a value:

```pascal
x := if Cond then AValue else BValue;
Caption := 'Items: ' + (if N = 1 then 'one' else IntToStr(N));
```

The `else` branch is **mandatory** in expression form (unlike the statement),
and the result type is the unified type of both branches.

### Current state
- No support, no partial work. FPC only offers the `IfThen` RTL function.
- Tokens `_IF`/`_THEN`/`_ELSE` already exist (`compiler/tokens.pas:109,150,138`).
- `tifnode` is **statement-only**: `nflw.pas:1843` hardcodes `resultdef:=voidtype`.

### Implementation
1. **Parser hook** — `compiler/pexpr.pas`, `factor()` (~`:3332`), token dispatch
   `case current_scanner.token of` (~`:3819`). Add an `_IF:` case that parses an
   inline-if expression. Model the condition/branch parsing on `if_statement`
   (`compiler/pstatmnt.pas:371-396`) but call `comp_expr` for the branches instead
   of `statement`, and **require** the `else`.
2. **Node** — prefer a **new `tifexprnode`** over mutating `tifnode` (which many
   passes assume is void/statement). It carries condition + two value subnodes.
   - `pass_typecheck`: compute the unified result type of the two branches using
     the `compare_defs` (`defcmp.pas:123`) + `is_in_limit` (`defutil.pas:122`)
     pattern that array constructors use (`nld.pas:1304-1336`). Handle
     integer-width promotion, string/char unification, and `nil`/pointer cases.
   - `pass_1` / codegen: lower to existing conditional evaluation. Simplest path is
     to reuse the secondary-pass machinery of `tifnode` by generating a temp and
     two assignments, or lower in `pass_1` to a temp + `tifnode`. Evaluate the
     condition once; only the taken branch is evaluated (short-circuit semantics).
3. **Constant folding** — in `simplify`, if the condition is a constant, collapse
   to the taken branch (lets it appear in const contexts where both branches fold).

### Tests
- Basic int/string/float branch unification; nested inline-if; in const expr;
  in default-parameter and array-constructor contexts; verify only one branch
  has side effects (counter test); type-mismatch error path.

### Open questions
- Delphi binds `if`-expr at the precedence of a full expression; confirm parenthesization
  rules around `+`/`and`. Recommend requiring parens when embedded in a larger
  expression initially, then relax.
- Decide whether to gate behind a modeswitch (e.g. only in `m_delphi`) — recommend
  yes, Delphi-mode only, to avoid surprising `fpc`/`objfpc` code.

---

## 2. `NameOf` intrinsic — D13

**Status: ✅ landed.** Tests: `tnameof1.pp`–`tnameof3.pp`. Verified for vars, fields and
type names; routines and enum elements are not covered by those tests.

**Priority: High (small, visible).  Effort: S–M.  Risk: Low.**

### Goal
`NameOf(X)` returns the **source identifier name** of X as a string constant at
compile time (variable, field, type, routine, etc.):

```pascal
WriteLn(NameOf(SomeVar));        // 'SomeVar'
RegisterProp(NameOf(TFoo.Bar));  // 'Bar'
```

### Current state
Clean slate — zero matches. Closest model is the string-returning ObjC intrinsic
`in_objc_encode_x` + `handle_objc_encode` (`compiler/ninl.pas:3361`).

### Implementation
1. `compiler/compinnr.pas` — add `in_nameof_x` to the `tinlinenumber` enum.
2. `compiler/psystem.pas` — register the builtin in `create_intern_symbols`
   (~`:65-115`): `systemunit.insertsym(csyssym.create('NameOf', in_nameof_x))`.
3. `compiler/pexpr.pas` — handle the call in `statement_syssym()` (parse one
   argument without evaluating side effects; accept a symbol/designator).
4. `compiler/ninl.pas`:
   - `pass_typecheck`: set `resultdef := cunicodestringtype` (Delphi `NameOf`
     yields a `string`/UnicodeString); validate the arg resolves to a named entity.
   - `simplify`: extract the final identifier from the `tloadnode`/designator
     (last component only — `NameOf(TFoo.Bar)` → `'Bar'`) and return
     `cstringconstnode.createstr(name)` (`ncon.pas:836`).

### Tests
- Local var, global var, field, property, type, routine, enum element; dotted
  designator returns last component; error on a non-named expression like
  `NameOf(1+2)`; usable in a const declaration.

### Open questions
- Confirm Delphi's exact rule for qualified names (last identifier vs full path).
  Embarcadero docs: `NameOf` returns the simple (unqualified) name. Match that.

---

## 3. `interface` and `unmanaged` generic constraints — D13

**Status: ✅ landed**, but not as designed below — no `gcf_interface` flag was added;
the `interface` constraint is "any interface" via `IInterface`. Only `gcf_unmanaged`
exists in the enum. PPU bumped to 209 as planned. Tests: `tgenconstr1.pp`–`tgenconstr4.pp`.

**Priority: Medium.  Effort: M.  Risk: Low–Medium (PPU bump).**

### Goal
```pascal
type
  TFoo<T: interface> = class ... end;     // T must be an interface
  TBar<T: unmanaged> = record ... end;     // T has no managed fields (refines `record`)
```

### Current state
`tgenericconstraintflag` = `(gcf_none, gcf_constructor, gcf_class, gcf_record)`
(`compiler/symconst.pas:272-277`). Parsing and enforcement handle class/record/
constructor only.

### Implementation
1. `compiler/symconst.pas:272` — add `gcf_interface`, `gcf_unmanaged` to the enum.
2. `compiler/pgenutil.pas` — `parse_generic_parameters` (~`:2409-2475`):
   - Add `_INTERFACE:` case alongside `_CLASS:` (`:2417`), setting `gcf_interface`.
   - `unmanaged` is **not a reserved word** — handle it as a contextual identifier
     in the `else` branch before the `single_type()` call (~`:2447`): test
     `(token=_ID) and (upcase(orgpattern)='UNMANAGED')`, set `gcf_unmanaged`,
     consume. Reject combination with `gcf_record`/`gcf_class`.
3. `compiler/pgenutil.pas` — `check_generic_constraints` (~`:392-431`):
   - `gcf_interface`: require the supplied arg be an interface objectdef
     (`odt_interfacecom`/`corba`/`dispinterface`/`java`); else
     `type_e_interface_type_expected`.
   - `gcf_unmanaged`: require `not is_managed_type(paradef)`
     (`defutil.pas:828`, i.e. `not paradef.needs_inittable`); else a new error
     message (add to `msg/errore.msg`).
4. **PPU** — flags serialize automatically via `tppuset1`
   (`symdef.pas:1993-2017`), but adding enum values changes the set layout. Bump
   `CurrentPPUVersion` (`compiler/ppu.pas:48`, currently 208 → 209). Coordinate
   with the bootstrap/PPU-version gotcha noted in prior dotted-units work.

### Tests
- Interface constraint accepts interface, rejects class/record; unmanaged accepts
  `Integer`/POD record, rejects `string`/`IInterface`/managed record; PPU round-trip
  (compile a unit exporting a constrained generic, consume from another unit).

---

## 4. Enable type helpers + implicit function specialization in default Delphi mode

**Status: 🟡 half landed.** `m_type_helpers` is in `delphimodeswitches`; test
`ttypehelperdef1.pp`. `m_implicit_function_specialization` is **not** — see item 8.

**Priority: High (cheap, daily ergonomics).  Effort: S (+regression).  Risk: Medium (semantics).**

### Goal
Make `m_type_helpers` and `m_implicit_function_specialization` active by default in
Delphi mode, as in real Delphi.

### Current state
Both modeswitches exist but are **absent** from `delphimodeswitches`
(`compiler/globals.pas:53-59`). Today users must `{$modeswitch typehelpers}` /
`{$modeswitch implicitfunctionspecialization}` manually.

### Implementation
1. Add `m_type_helpers` and `m_implicit_function_specialization` to the
   `delphimodeswitches` set (`globals.pas:53`). `delphiunicodemodeswitches` inherits.
2. **Critical semantics check before flipping `m_type_helpers`:** FPC also has
   `m_multi_helpers` (multiple active helpers per type). Delphi semantics are
   **single helper, nearest-in-scope wins**. Do **not** add `m_multi_helpers` to
   Delphi mode — verify that with only `m_type_helpers` the resolution matches
   Delphi (last/nearest helper in scope shadows earlier ones).
3. Run the full compiler test suite (`tests/`) under Delphi mode; type helpers
   becoming default can change overload/type-helper resolution in existing code.

### Tests
- Existing `tests/test/` helper and generics cases still pass; add Delphi-mode
  cases: record/type helper without explicit modeswitch; implicit `Foo(x)` for
  `generic Foo<T>`; helper shadowing precedence.

### Open questions
- Some real-world code relies on helpers being *off* in Delphi mode to avoid
  clashes. Low risk, but note it in release notes as a behavior change.

---

## 5. Extended method (and field) RTTI for classes

**Status: ✅ landed** and enabled fork-wide by default (not gated on `{$RTTI EXPLICIT}`).
Test: `textrtti1.pp`. Caveat: `TRttiMethod.Invoke` still needs `ffi.manager` — reading
metadata works, calling through it does not without that unit.

**Priority: Highest impact (ecosystem unlock).  Effort: M–L.  Risk: Medium.**

### Goal
`TRttiType.GetMethods` / `GetFields` return **non-published** members (with
parameter info) when a class enables extended RTTI via `{$RTTI EXPLICIT METHODS ...}`.
This unblocks Spring4D, DI containers, ORMs, and JSON/REST serializers.

### Current state — smaller gap than it looks
- The **VMT extended method table is already emitted** —
  `ncgvmt.pas:537,573` calls `RTTIWriter.write_extended_method_table` (params,
  visibility, code address, attributes all present).
- `write_extended_method_table` / `write_extended_field_table` already exist
  (`ncgrtti.pas:791-822`, `:831-892`).
- **Records** already emit extended method+field tables in `recorddef_rtti`
  (`ncgrtti.pas:1795-1798`, under `rt=fullrtti`).
- **The actual gap:** `objectdef_extended_rtti_class()` (`ncgrtti.pas:1968-1979`)
  writes extended **properties only** — it never writes the extended **method**
  or **field** tables for classes. So `System.Rtti` (`rtl-objpas/.../rtti.pp:6857`,
  `ResolveDeclaredMethods` → `GetMethodList(..., False)`) finds nothing for
  non-published methods.
- `pdecl.pas:506` TODO ("reject a constructor that lacks extended RTTI") is a
  downstream consequence — it can be properly enforced once classes expose method RTTI.

### Implementation
1. `compiler/ncgrtti.pas` — in `objectdef_extended_rtti_class()` (~`:1968`), mirror
   the record path (`:1795-1798`): emit the extended **method** table using
   `def.rtti.options[ro_methods]` visibilities, and the extended **field** table
   using `def.rtti.options[ro_fields]`. Reuse `write_extended_method_table` /
   `write_extended_field_table`.
2. Ensure the class RTTI data layout / offsets that `System.Rtti` reads match what
   records produce (the runtime side already expects `PVmtMethodExEntry` arrays).
3. `compiler/pdecl.pas:506` — once methods are exposed, implement the deferred
   check: reject an attribute constructor whose class lacks method extended RTTI.
4. Verify/extend the runtime reader in `rtl-objpas/src/inc/rtti.pp` (`GetMethods`,
   `GetFields`, `ResolveDeclaredMethods`) handles the now-populated tables for
   classes, not just records.

### Tests
- Class with `{$RTTI EXPLICIT METHODS ([vcPublic..]) FIELDS (...)}`: `GetMethods`
  returns public/private methods with correct param lists and visibility; `GetFields`
  returns non-published fields; attributes attached to methods are readable; a
  serializer round-trip (use the existing `System.Json.Serializers` in vcl-compat as
  a real consumer). Confirm published-only default behavior is unchanged when
  extended RTTI is not requested.

### Notes
- This is the **highest-value** item even though it's listed below the smaller
  syntax features. Because the writer helpers and VMT emission already exist, the
  core change is localized to `objectdef_extended_rtti_class`, but verifying the
  runtime/layout contract is where the L-sized risk lives.

---

## 6. Compiler directive polish — D13

**Status: ❌ not started.** `compiler/scandir.pas` still has no `IFOPT` handling.

**Priority: Low.  Effort: S.  Risk: Low.**

### Goal
Match D13 directive refinements:
- Long-form `{$IFOPT}` (multi-character option directives).
- Warnings for **unbalanced** conditional/option directives.
- Confirm `{$PUSH}`/`{$POP}` save & restore both options **and** the warning
  configuration set (D13 emphasizes warning-set save/restore).

### Current state
- `{$PUSH}`/`{$POP}` already exist in FPC. RTTI/option directive handling lives in
  `compiler/scandir.pas` (e.g. `{$RTTI}` parsing at `:1510-1568`).
- Need to verify `{$IFOPT}` long-form and add unbalanced-directive diagnostics.

### Implementation
1. `compiler/scandir.pas` — extend `{$IFOPT}` handling to accept long option names.
2. Add an unbalanced-directive warning: track push/pop and conditional nesting
   depth at end of unit; emit a warning if non-zero. New message in `msg/errore.msg`.
3. Audit `{$PUSH}`/`{$POP}` to confirm the saved state includes warning config.

### Tests
- `{$IFOPT}` long-form accepted; mismatched `{$PUSH}` without `{$POP}` warns;
  warning state restored across push/pop.

---

## 7. `System.Net.HttpClient` — library (lowest in ordering)

**Status: ❌ not started.** Only `fcl-web`'s `fphttpclient` exists; no `System.Net.HttpClient`.

**Priority: Medium (most-felt library gap).  Effort: L.  Risk: Medium.**

### Goal
High-level Delphi-compatible HTTP client: `THTTPClient`, `IHTTPResponse`,
`IHTTPRequest`, `TNetHeaders`, sync + async `Get/Post/Put/Delete`, redirect
handling, TLS, streaming.

### Current state
Only low-level sockets exist: `fcl-net` provides `System.Net.FPSockets`,
`System.Net.Sslsockets`, `System.Net.Sslbase`, `System.Net.Resolve`. No high-level
client (`System.Net.HttpClient` not found). vcl-compat already hosts many
`System.*` units, so it's the natural home.

### Implementation
1. New unit `packages/vcl-compat/src/system.net.httpclient.pp` (+ namespaced alias
   in `rtl/namespaces.lst`).
2. Build `THTTPClient` over `fcl-net` sockets + `Sslsockets`/`Sslbase` for TLS.
   Implement request/response objects, header collections, status handling,
   chunked transfer, redirects, timeouts.
3. Optionally back async ops with the existing `System.Threading` / `Fcl.ThreadPool`.
4. Provide platform-native backends later if needed (WinHTTP on Windows) — start
   with the portable socket-based implementation.

### Tests
- GET/POST against a local test server; HTTPS; redirect follow; header round-trip;
  large/streamed body; timeout/error paths.

### Note
This is the only library item in scope here. **FMX, FireDAC, Skia, LiveBindings**
are explicitly **out of scope** — they are framework-scale, partly proprietary, and
not language parity. Track separately if ever pursued.

---

---

## 8. Implicit function specialization in default Delphi mode

**Priority: Medium.  Effort: M.  Risk: Medium.**

**Status: ✅ landed.** Split out of item 4.

`m_implicit_function_specialization` is now in `delphimodeswitches`
(`compiler/globals.pas`), so implicit specialization is active by default in
Delphi mode, matching Delphi.

The blocking crash was a dangling generic-**parameter** def, not an overload-logic
bug. A generic function whose parameter is an inline specialization
(`function Test<T>(const A: TArray<T>)`) stores that partial specialization in the
generic procdef's **local** symtable. `free_localsymtables` (`compiler/pmodules.pas`)
freed every non-inline procdef's local symtable at end of unit — including generic
ones — leaving the parameter def dangling while the exported procdef survived. Any
consumer compiled later in the same invocation then dereferenced freed memory;
implicit-spec probing (`is_generic_param_used`) does exactly that, crashing with
Internal error 2021020905. Disk-PPU reload rebuilt the def in a valid scope, so it
only surfaced when the defining unit and its user were compiled together.

Fix: exempt generic procdefs from local-symtable release, as inline routines
already are (their local symtable holds specializations referenced by the
signature and must outlive it). A `gs_para` **placement** change was tried first
and rejected — it split one specialization across the para and local symtables,
breaking specialization reuse and the RTL build (`objpas.inc` `TMarshal.FixArray<T>`,
duplicate identifier). Verified with a full `make clean cycle` (ppc2/ppc3 identical)
and CI. Regression test: `tests/test/timpfuncspez38.pp` (+ `uimpfuncspez38a/b`).

---

## 9. Inline variable scope lifetime

**Priority: High (was a crash).  Effort: M.  Risk: Medium (touches every block).**

**Status: ✅ landed** — PR #22, issue #21. Not part of the original plan.

Inline variables had **procedure** lifetime rather than scope lifetime:
`exit_nested_block` (`compiler/pstatmnt.pas`) only set `scope_lvl := -1` and
renamed the symbol, which controls name visibility, not storage or lifetime.

Two consequences:

- **Crash.** Initialisation and finalisation came from the implicit
  per-procedure passes in `ngenutil.pas`, which dispatch on `proctypeoption`.
  `potype_proginit` (a program's main block) and `potype_unitfinalize` (a unit's
  `finalization`) have empty arms there, so a managed inline var in either was
  never initialised; assigning to it made `fpc_ansistr_assign` decrement the
  refcount of stack garbage — `Runtime error 216`. It only appeared to work when
  the slot happened to be zero, which is why it survived so long.
- **Wrong lifetime.** Where the passes did run, a managed inline var in a nested
  block lived until the enclosing routine returned, so two "scoped" guards in
  sibling blocks overlapped. Delphi releases at block exit.

`close_nested_block()` now emits initialisation on entry to the declaring block
and finalisation on leaving it, in reverse declaration order, wrapped in an
implicit `try/finally` (`ctryfinallynode.create_implicit`) so exceptions, `exit`,
`break` and `continue` all finalise. All 13 `enter_nested_block` sites pair with
it — a missed site leaves variables uninitialised and only crashes
intermittently. Handled symbols are flagged `has_scope_lifetime` (transient, not
in the PPU, so no CRC change or version bump) and skipped by the per-procedure
passes.

Fixing the lifetime subsumed the crash fix: initialisation now comes from the
statement stream rather than the `proctypeoption` dispatch.

Tests: `tinlinevarscope1.pp` (block scopes, plus a classic-var contrast) and
`tinlinevarscope2.pp` (branch statements with no begin/end). The pre-existing
`tinlinevars.pp` never declared an inline var in its own main block.

### Follow-on work not done
- **Stack slot reuse.** Sibling scopes still burn a distinct slot each for the
  whole procedure; lifetime is now correct but storage is not shared. A size
  inefficiency, not a correctness bug.
- **`goto` into or out of a scoped block** is untested against the implicit
  try/finally; `exit`/`break`/`continue` are covered.

---

## Suggested execution order

Remaining work, by impact:

1. **#7 System.Net.HttpClient** — largest remaining gap for porting real code;
   library-level and independent of the compiler.
2. **#6 Directive polish** — minor.

Non-plan items worth folding in at some point: predefining `FPC_DOTTEDUNITS` in
the dotted build, and either shipping a default function-call manager or
documenting `ffi.manager` prominently for `TRttiMethod.Invoke`.

For the record, the landed items were executed in roughly this order:
#5 extended RTTI, #4 type helpers, #2 NameOf, #1 inline `if`,
#3 constraints, then #9 scope lifetime (unplanned, from a bug report).

## Cross-cutting notes
- Any change touching PPU layout (#3, possibly #5) requires a `CurrentPPUVersion`
  bump (`compiler/ppu.pas:48`) and a clean bootstrap rebuild — see prior
  dotted-units/PPU bootstrap gotchas before releasing.
- Gate new **syntax** (#1, and arguably #2) behind Delphi mode to avoid changing
  `fpc`/`objfpc` behavior.
- Add regression tests under `tests/test/` for each item; run the full suite after
  the mode-switch flip (#4), which can shift resolution in existing code.
