# Delphi vs tunnelpascal — feature comparison

A living comparison of Embarcadero Delphi (through 13 "Florence") against this
FPC fork (tunnelpascal). Scope is **language + RTL/library**, not the IDE.

**Legend**
- ✅ Present and on by default in `{$mode delphi}`
- 🟡 Present but partial, or needs an explicit opt-in (`{$modeswitch …}` / unit)
- 🆕 Added or made-default by this fork's Delphi-parity work (see `PLAN.md`)
- ❌ Absent
- 🚫 Out of scope (large proprietary framework)

**Confidence:** language rows are verified against the compiler mode-switch table
and this fork's own test suite. RTL rows are from the shipped unit inventory —
presence of a unit does not guarantee 100 % API completeness. Framework rows are
structural (absent). Treat this as a snapshot, not a certified checklist.

---

## 1. Object Pascal language

### Core (pre-2009, foundational)
| Feature | Delphi | tunnelpascal |
|---|---|---|
| Classes, single inheritance, virtual/abstract/sealed | ✅ | ✅ |
| Interfaces (COM/`IInterface`, ref-counted) | ✅ | ✅ |
| CORBA/"raw" interfaces | ✅ | ✅ |
| Exceptions (`try/except/finally`, `raise`) | ✅ | ✅ |
| Properties (incl. indexed, default, class properties) | ✅ | ✅ |
| Operator overloading | ✅ | ✅ |
| Sets, subranges, enums, `{$scopedenums}` | ✅ | ✅ |
| Dynamic arrays (+ intrinsics) | ✅ | ✅ |
| `strict private` / `strict protected` | ✅ | ✅ |
| Nested types / nested constants | ✅ | ✅ |
| Class vars, class constructors/destructors | ✅ | ✅ |

### Delphi 2009–XE era
| Feature | Delphi | tunnelpascal |
|---|---|---|
| Generics (classes, records, methods) | ✅ | ✅ |
| Generic constraints (`class`/`constructor`/interface/base type) | ✅ | ✅ |
| Anonymous methods / closures | ✅ | ✅ (`m_anonymous_functions`) |
| Unicode `string` (UTF-16) | ✅ | ✅ |
| Class & record helpers | ✅ | 🆕 default in Delphi mode (was opt-in) |
| Attributes (custom, prefixed `[…]`) | ✅ | ✅ |
| Extended RTTI (`System.Rtti`, `TRttiContext`) | ✅ | 🆕 default-on fork-wide (see §2) |

### Delphi 10.x–12 era
| Feature | Delphi | tunnelpascal |
|---|---|---|
| Inline variable declarations + type inference (`var x := …`) | ✅ (10.3) | ✅ |
| Inline variables have **scope** lifetime (managed types released at block exit) | ✅ | 🆕 (PR #22) — was procedure lifetime; see note below |
| Custom managed records (`Initialize`/`Finalize`/`AddRef`/`Copy`) | ✅ (10.4) | ✅ (management operators) |
| Function references (`reference to`) | ✅ | ✅ (`m_function_references`) |
| Multiline string literals (`'''`) | ✅ (12) | ✅ (`m_multiline_strings`) |
| Digit separators (`1_000_000`) | ✅ (12) | ✅ (`m_underscoreisseparator`) |
| `{$PUSH}` / `{$POP}` option save/restore | ✅ | ✅ |

> **Inline variable lifetime.** Until PR #22 an inline `var` had *procedure*
> lifetime: `exit_nested_block` only hid the name, so an interface declared in a
> nested block stayed alive until the enclosing routine returned and two
> "scoped" guards in sibling blocks overlapped. Worse, the blocks that are not
> ordinary procedures — a program's main `begin`/`end.` and a unit's
> `finalization` — got no initialisation at all and faulted with `Runtime error
> 216` (issue #21). Initialisation/finalisation is now emitted around the
> declaring block inside an implicit `try/finally`, so exceptions, `exit`,
> `break` and `continue` all finalise. Classic `var`-section variables keep
> procedure lifetime, which is correct: Pascal has no block-level var section.

### Delphi 13 "Florence"
| Feature | Delphi | tunnelpascal |
|---|---|---|
| Inline `if` expression (ternary): `x := if c then a else b` | ✅ | 🆕 (item #1) |
| `NameOf` intrinsic | ✅ | 🆕 (item #2) — vars/fields/types; routines & enum elements TBD |
| `interface` generic constraint | ✅ | 🆕 (item #3) — "any interface" via `IInterface` |
| `unmanaged` generic constraint | ✅ | 🆕 (item #3) — stricter than Delphi for non-record types (see PLAN.md) |
| `{$IFOPT}` long-form + unbalanced-directive warnings | ✅ | 🟡 planned (item #6) |
| Refined record/class `Initialize`/`Finalize` operators | ✅ | ✅ (audit pending) |

### Known gaps / not implemented
| Feature | Delphi | tunnelpascal |
|---|---|---|
| Implicit function specialization by default | ✅ | ✅ on by default in Delphi mode (item #8); the blocking overload-probing crash — a dangling generic-parameter def for inline-specialization params — was fixed by keeping generic procdefs' local symtables alive |
| Nullable value **types** (language-level) | ❌ (not in 13 either) | ❌ (RTL `System.Nullable<T>` record exists) |
| ARC / weak refs (`[weak]`, `[unsafe]`) | mobile-era only, removed | ❌ (FPC is not ARC; N/A) |

---

## 2. RTTI & metadata

| Capability | Delphi | tunnelpascal |
|---|---|---|
| Published-member RTTI (classic) | ✅ | ✅ |
| Extended RTTI for public/private methods, fields, properties | ✅ | 🆕 default-on (item #5) |
| `TRttiContext.Create` exposes full RTTI by default | ✅ | 🆕 (item #5) |
| Attributes readable via RTTI | ✅ | ✅ |
| `{$RTTI EXPLICIT/INHERIT …}` directive | ✅ | ✅ |
| Invoke methods via RTTI (`TRttiMethod.Invoke`) | ✅ | 🟡 needs `uses ffi.manager` (see below) |

> Note: enabling Delphi-style RTTI fork-wide increases binary size (every type
> carries RTTI) and makes FPC's `TObject._MonitorData` field RTTI-visible — an
> internal Delphi's TObject does not have.

> **`TRttiMethod.Invoke` needs a function-call manager.** The default is
> `NoInvoke` (`packages/rtl-objpas/src/inc/rtti.pp`), which raises
> `ENotImplemented: Invoke functionality is not implemented on this platform.
> Use external managers, e.g. ffi.manager.` Add `ffi.manager` from the shipped
> `libffi` package to your `uses` and it works. Delphi requires no such step, so
> RTTI-driven code ported from Delphi will compile and then fail at run time
> until the manager unit is pulled in. Everything else in this table — reading
> types, fields, methods, attributes — works without it.

---

## 3. RTL / standard library (`System.*`)

The fork ships the RTL under **both** classic names (`sysutils`, `classes`, …)
and Delphi-style dotted names (`System.SysUtils`, …) via namespace aliases; a
"dotted-units" release variant builds the `System.*` units directly.

| Unit / area | Delphi | tunnelpascal |
|---|---|---|
| `System.SysUtils`, `System.Classes` | ✅ | ✅ |
| `System.Generics.Collections` / `.Defaults` | ✅ | ✅ |
| `System.Rtti` | ✅ | ✅ |
| `System.TypInfo`, `System.Variants`, `System.Types` | ✅ | ✅ |
| `System.Threading` (TTask, TParallel) | ✅ | ✅ (vcl-compat) |
| `System.SyncObjs`, monitors | ✅ | ✅ |
| `System.IOUtils` (TFile/TDirectory/TPath) | ✅ | ✅ |
| `System.Diagnostics` (TStopwatch) | ✅ | ✅ |
| `System.JSON` (+ readers/writers/serializers) | ✅ | ✅ (vcl-compat) |
| `System.Hash` | ✅ | ✅ |
| `System.NetEncoding` | ✅ | ✅ |
| `System.RegularExpressions` | ✅ | ✅ |
| `System.DateUtils`, `System.StrUtils`, `System.Character` | ✅ | ✅ |
| `System.Messaging` | ✅ | 🟡 (present; completeness unverified) |
| `System.Net.*` sockets / SSL (low level) | ✅ | ✅ (fcl-net) |
| `System.Net.HttpClient` (high-level HTTP) | ✅ | ❌ planned (item #7) |
| `System.Bindings.*` (LiveBindings) | ✅ | ❌ |
| Notifications / permissions / devices (mobile) | ✅ | 🟡 (stubs in vcl-compat) |

Beyond Delphi, the fork inherits FPC's `fcl-*` packages (fcl-db, fcl-xml,
fcl-web, etc.) and much wider CPU/OS target coverage.

---

## 4. Databases

| Feature | Delphi | tunnelpascal |
|---|---|---|
| `TDataSet` abstraction | ✅ (`Data.DB`) | ✅ (FPC `DB`, dotted `Data.DB`) |
| FireDAC (unified multi-DB access) | ✅ | 🚫 — FPC alternative is **SQLDB** (different API) |
| dbExpress / IBX / ADO | ✅ | ❌ (SQLDB connectors instead) |
| `Data.FMTBcd` | ✅ | ✅ |

---

## 5. GUI & graphics frameworks

| Framework | Delphi | tunnelpascal |
|---|---|---|
| VCL (Windows) | ✅ | 🚫 — FPC alternative is **LCL** (Lazarus), not VCL-identical |
| FMX / FireMonkey (cross-platform GUI) | ✅ | 🚫 |
| Skia integration | ✅ (13) | 🚫 |
| VCL styles / FMX styles | ✅ | 🚫 |

These are large, partly proprietary frameworks and are **not a language-parity
goal**. Cross-platform GUI on FPC is served by LCL/Lazarus.

---

## 6. Cloud / mobile / enterprise

| Feature | Delphi | tunnelpascal |
|---|---|---|
| DataSnap / RAD Server | ✅ | 🚫 (FPC: fcl-web, fpWeb, Brook, etc.) |
| Mobile targets (iOS/Android native) | ✅ | 🟡 (FPC has ARM/Android targets; not the FMX mobile stack) |
| Push notifications / analytics APIs | ✅ | 🟡 (stubs) |

---

## 7. Toolchain & targets (where the fork is *ahead*)

| Capability | Delphi | tunnelpascal |
|---|---|---|
| Native Windows x64 | ✅ | ✅ (release artifacts) |
| Native Windows on ARM | ✅ (13.1) | ✅ (release artifacts) |
| Dotted-unit (`System.*`) naming | ✅ | ✅ (but `FPC_DOTTEDUNITS` is not predefined — see below) |
| Linux / macOS / *BSD / bare-metal / many CPUs | limited | ✅ (FPC's broad target matrix) |
| Open source | ❌ | ✅ |

> **`FPC_DOTTEDUNITS` is not predefined by the dotted-units build.** A source
> file cannot detect which RTL variant it is being compiled against, so the
> usual `{$IFDEF FPC_DOTTEDUNITS} System.SysUtils {$ELSE} SysUtils {$ENDIF}`
> pattern silently takes the wrong branch unless `-dFPC_DOTTEDUNITS` is passed
> explicitly. On a dotted-only build the failure is a bare
> `Fatal: Can't find unit SysUtils`, which does not hint at the cause. This
> affects the Compiler Explorer build (`tp191`), which is dotted-only. Having
> the dotted build predefine the symbol would make the idiom work unassisted.

---

## Summary

For **language and RTL**, tunnelpascal is at or near Delphi 12 parity across the
board, with most Delphi 13 language additions landed (inline `if`, `NameOf`,
`interface`/`unmanaged` constraints), Delphi-style helpers and extended RTTI now
on by default, and inline variables now carrying Delphi's scope lifetime.
Remaining language work is small (directive polish, the implicit-specialization
compiler fix). The genuine, deliberate gaps are the big **frameworks** — FMX,
FireDAC, LiveBindings, Skia — which are out of scope; FPC's own ecosystem (LCL,
SQLDB, fcl-web) covers those needs with different APIs.

Two rough edges worth knowing before porting Delphi code, both detailed above:
`TRttiMethod.Invoke` needs `ffi.manager` in `uses`, and the dotted-units build
does not predefine `FPC_DOTTEDUNITS`.

See `docs/delphi-parity/PLAN.md` for per-item implementation detail, status, and
limitations.
