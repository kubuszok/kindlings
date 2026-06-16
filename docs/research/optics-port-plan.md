# Optics module — quicklens reimplementation port plan

**Status:** plan only (no implementation yet). Approved 2026-06-15.

**Goal.** Prove that a lens/optics-style library (SoftwareMill's
[quicklens](https://github.com/softwaremill/quicklens)) can be implemented with Hearth's
macro-agnostic API — cross-compiled (Scala 2.13 + 3, JVM/JS/Native), no platform-specific
macro *logic* beyond thin bridges, no upstream quicklens dependency.

**Naming (per [[feedback-reimpl-naming-not-dependency]]).** This is a from-scratch reimpl
with no upstream dep, so it must NOT be titled bare `quicklens`. Decided:

- module dir: `optics/`
- moduleName: `kindlings-optics`
- package: `hearth.kindlings.optics`
- docs/nav title: "Optics (quicklens-style lenses)"

This is a **non-derivation** macro module (like `di`/`mock`). The type-class-derivation
requirements (REQ-1 dual entry point, rule-chain / `ValDefsCache` / `parTuple` /
factory-instance, std-extension-once) do **not** all apply; the relevant scaffolding is the
3-layer non-derivation recipe ([[project-di-module-pattern]]).

---

## 1. Quicklens API inventory

Operations the port must support (source of truth: quicklens `package.scala` +
`QuicklensMacros.scala`).

### Entry points
- **Extension form**: `obj.modify(_.a.b.c)` → `PathModify[S, A]`.
- **Function form**: `modify(obj)(_.a.b.c)` (in S3 the curried `modify(_: T)(...)` is the
  reusable-lens idiom).
- **Multi-field**: `obj.modifyAll(_.a, _.b.each, _.c)` → one `PathModify` whose function
  chains each path's modification.

### Terminal ops on `PathModify[S, A]`
`using(mod: A => A): S`, `apply(mod)` (alias of `using`), `setTo(v: A): S`,
`setToIfDefined(v: Option[A])`, `setToIf(cond)(v)`, `usingIf(cond)(mod)`.

### Field access
`_.field.subfield…` — chain of `Select`s → nested `.copy(field = …)`. Works on case
classes, sealed traits (per-child `isInstanceOf`/match dispatch), and product-like classes
with a `copy` (deferred — see open questions).

### Collections / `.each`
`.each` (Seq/Option/Map values/Array/user `QuicklensFunctor[F]`), `.eachWhere(cond)`.

### Index / `.at`
`.at(i)` (throws if absent), `.index(i)` (no-op if absent), `.atOrElse(i, default)`;
`Option`-targeted single-`at`: `.at` / `.atOrElse(default)` / `.index`.

### Either
`.eachLeft` / `.eachRight`.

### Subtype prism
`.when[Subtype]` — `value match { case b: Subtype => f(b); case other => other }` (a
**non-exhaustive 2-case** match).

### Composition / reusable lenses
`modifyLens[T](_.a.b)` → `PathLazyModify[T, U]`; `modifyAllLens[T](_.a, _.b)`; curried
`modify(_: T)(_.a.b)`; `andThenModify` (compose two `PathLazyModify`); `PathLazyModify`'s
own `using`/`setTo`/`setToIfDefined`/`setToIf`/`usingIf` returning `T => T`.

### Runtime typeclasses (hand-written, not derived)
`QuicklensFunctor[F]`, `QuicklensIndexedFunctor[F, I]`, `QuicklensSingleAtFunctor[F]`,
`QuicklensEitherFunctor[T, L, R]`, `QuicklensWhen[A]` + givens for Seq/Option/Map/Array/
Either. (No `+|` operators in current quicklens — ignore.)

---

## 2. Quicklens macro mechanics

The macro never derives a type class — it **parses a path lambda and emits a copy-with-
modification expression** of type `(A => A) => S`. The terminal (`using`/`setTo`) supplies
the `A => A`.

**Scala 3** (`modifyImpl`): strip `Inlined`/`Block(DefDef…)` wrappers → pattern-match
`quotes.reflect` trees into `PathSymbol`s (`Select` → `Field`; the `at/each/eachWhere/
eachRight/eachLeft/atOrElse/index/when` applies → `FunctionDelegate(name, givn, lastTypeTree,
args)` carrying the compiler-resolved implicit functor `givn`; extension applies →
`Extension`; `Ident` starting `_` → root). Merge into a `PathTree` (shared prefixes), then
emit nested `.copy` / sum `if isInstanceOf … else throw` / `givn.method[T](obj, x => …, args)`.

**Scala 2** (`modificationForPath`): same via untyped quasiquotes; sealed dispatch
precomputed from `knownDirectSubclasses`.

Generated shape for `modify(p)(_.a.b.each.c).using(f)`:
```scala
PathModify(p, mod =>
  p.copy(a = p.a.copy(b = functor.each(p.a.b, x => x.copy(c = mod(x.c))))))
```

---

## 3. Hearth mapping

| Quicklens mechanism | Hearth API | Reference file | Risk |
|---|---|---|---|
| Strip lambda wrappers + body | `DestructuredExpr.parse` / `extractLambda` (auto-strips `Inlined`) | hearth `Exprs.scala` | Low — confirm `Block(DefDef…)` (modifyAll element) returns inner body |
| Path `_.a.b.c` → field list | `DestructuredExpr.extractFieldPath` → `FieldPath`/`FieldPathSegment(name, sourceType, resultType, method)` | hearth `Exprs.scala` | Low — purpose-built; verify trailing `.each`/`.at` tolerated |
| Path with `.each`/`.at`/`.when` interleaved | `DestructuredExpr.parse` → walk `MethodCall` chain, classify by `mc.method.name` | mock `MockMacrosImpl.extractMethodRef` | **Medium** — hand-roll classifier; `.when[B]` type arg from `AppliedTypes` |
| case class `.copy(field = …)` | `CaseClass.parse[A]` → `caseFieldValuesAt` + `construct`/`primaryConstructor` (rebuild with one field replaced) | hearth `Classes.scala`, di ctor logic | Low-Med — emulate `copy`; product-like w/ user `copy` deferred |
| sealed `.when[Sub]` / sum field dispatch | `Type.matchOn` / `MatchCase.typeMatch[Sub]` + wildcard `case other => other`; `Enum.parse`/`matchOn` for exhaustive | hearth `Classes.scala`, circe/jsoniter enum rules | Medium — `.when` is a **2-case partial** match, use `MatchCase` not exhaustive `Enum.matchOn` |
| `.each` over Seq/Option/Map | runtime `QuicklensFunctor` typeclass summoned by type; element lambda via `LambdaBuilder` | mock `summonDefault`, cats-derivation `FunctorMacrosImpl` | Medium — runtime-typeclass route avoids `IsCollection` machinery |
| `.at`/`.index`/`.atOrElse`/Either/Option.each | emit calls into hand-written runtime typeclasses summoned via `Expr.summonImplicit` | mock `Expr.summonImplicit`, di `summonOrAbort` | Low — mirrors quicklens `FunctionDelegate` |
| `modifyAll` shared-prefix tree | build `PathTree` ourselves; emit nested copies | quicklens `PathTree` | Medium — pure macro-side, platform-agnostic |
| Reusable lens / `andThenModify` | pure runtime (`PathLazyModify` case class); macro builds `(t, mod) => …` body | quicklens `package.scala` | Low |

**Architectural decision surfaced:** quicklens delegates `.each/.at/.when/.eachLeft` to
*runtime typeclasses* (`FunctionDelegate` emits `givn.method(obj, lambda, args)`). Porting
that verbatim is the **lowest-risk path** — the macro only (a) parses the path, (b)
generates nested `.copy`, (c) for non-field steps summons the typeclass and emits a call
with an element lambda. This avoids `IsCollection`/`LambdaBuilder` for everything except the
element lambda itself (which *is* collection iteration → permitted use of `LambdaBuilder`).

---

## 4. Anticipated Hearth gaps (→ `docs/research/` reproducers)

1. **`extractFieldPath` rejects non-field steps** — `Left`s as soon as a `MethodCall` has
   args/type-args (`.each`/`.at`/`.when`). Port needs a richer walker over the `MethodCall`
   chain. Likely kindlings-side, but if `MethodCall` can't expose the implicit `givn`
   receiver of an extension-method call, that's a real gap.
2. **Extension-method receiver recovery** — `.each` is
   `extension (fa: F[A])(using QuicklensFunctor[F])`. After parse, can we recover both
   `F[A]` (prefix expr) and a handle to summon `QuicklensFunctor[F]`? If `AppliedInstance`
   only carries the desugared first arg, we re-summon by type. Reproducer: parse
   `(_.xs.each.n)`, assert `xs` sub-expr + element type recoverable.
3. **Nested copy of deeply path-dependent field types** — `caseFieldValuesAt` yields
   `Expr_??`; rebuilding via `primaryConstructor` with one field swapped must keep types
   aligned across the S2 boundary (path-dependent `Underlying` leaking into `Expr.quote` —
   the di/mock pitfall). Reproducer: 3-level nested copy on S2.
4. **`.when[B]` non-exhaustive match** — need `MatchCase.typeMatch[B]` + catch-all default
   returning the original at type `A`. Confirm `MatchCase` supports a cross-platform
   wildcard default; if not → reproducer.
5. **Sum-typed field auto-dispatch** (`_.animals.each.name`, element sealed) — confirm
   `Enum.parse`/`exhaustiveChildren` yields the same child set on both platforms.

---

## 5. Proposed module structure

```
optics/src/main/
├── scala/hearth/kindlings/optics/
│   ├── PathModify.scala            # runtime: PathModify[S,A], PathLazyModify[T,U]
│   ├── QuicklensFunctors.scala     # runtime typeclasses + givens
│   ├── debug/package.scala         # LogDerivation import
│   └── internal/compiletime/
│       └── ModifyMacrosImpl.scala  # SHARED macro core: path parse, PathTree, copy gen
├── scala-2/hearth/kindlings/optics/
│   ├── syntax.scala                # implicit class ModifyOps + modify/modifyLens (macro defs)
│   ├── package.scala               # bare-import companion
│   └── internal/compiletime/ModifyMacros.scala   # S2 bridge
└── scala-3/hearth/kindlings/optics/
    ├── syntax.scala                # extension (obj) { inline def modify } + modifyLens
    ├── package.scala
    └── internal/compiletime/ModifyMacros.scala   # S3 bridge
```

`ModifyMacrosImpl` mirrors `WiringMacrosImpl`/`MockMacrosImpl`:
`private[optics] trait … { this: MacroCommons => }` with `toPathModify[S, A]`, `modifyAll`,
`modifyLensBody`. Bridges mirror `WiringMacros`/`MockMacros`. Entry-point shapes copied from
mock `syntax.scala` (S3 `extension`/`inline def = ${…}`, S2 `implicit class … = macro`).

**Test layout** (mirror quicklens `test/`): `ModifyFieldSpec`, `ModifyAllSpec`, `EachSpec`,
`AtIndexSpec`, `EitherSpec`, `WhenSpec`, `LensCompositionSpec`, `ErrorMessagesSpec`
(`compileErrors().check()`), plus `src/test/scala-3/` for S3-only syntax.

---

## 6. Phased implementation plan

**Phase 1 — MVP: single + nested field `modify`/`setTo`.** `PathModify` runtime; S2/S3
extension + function forms; `using`/`apply`/`setTo`/`usingIf`/`setToIf`/`setToIfDefined`;
macro parses path via `DestructuredExpr`, builds nested copy via `caseFieldValuesAt`/
`construct`. Tests: `ModifyFieldSpec` (1–3 level) + `ErrorMessagesSpec`. Gate:
`test-jvm-2_13 ; test-jvm-3` green.

**Phase 2 — `modifyAll` + `.each`.** `PathTree` merge; runtime `QuicklensFunctor` + givens
(Seq/Option/Map/Array); macro recognizes functor node, summons by type, emits
`functor.each(prefix, elem => copy…)`; `.eachWhere`. Tests: `ModifyAllSpec`, `EachSpec`
(incl. custom user functor), sealed `_.each.name`.

**Phase 3 — `.at`/`.index`/`.atOrElse`, Option single-at, Either, `.when`.**
`QuicklensIndexedFunctor`/`SingleAtFunctor`/`EitherFunctor`/`When` runtime + givens; macro
handles value args (`AppliedValues`); `.when[B]` via `MatchCase.typeMatch[B]` + wildcard.
Tests: `AtIndexSpec`, `EitherSpec`, `WhenSpec`; cross-platform `test-js-3 ; test-native-3`
smoke.

**Phase 4 — composition / reusable lenses + full parity.** `PathLazyModify` +
`modifyLens`/`modifyAllLens`/`andThenModify`; curried `modify(_: T)(…)`; product-like
(user `copy`) if feasible; port quicklens's full suite for parity; `docs/user-guide/`
snippet. Tests: `LensCompositionSpec` + mirrored quicklens suite.

Per phase: **clean before test** (incremental compilation does not re-expand macros); any
Hearth gap → `docs/research/` reproducer + keep the failing test in shared dirs.

---

## 7. Open decisions (resolved + remaining)

- ✅ **Module name** → `optics` (decided).
- **`.each` strategy** — recommend runtime `QuicklensFunctor` typeclasses (quicklens
  fidelity, lower macro risk, user-extensible); `LambdaBuilder` only for the element lambda.
- **Product-like (non-case-class `copy`) sources** — propose deferring to Phase 4 /
  out-of-scope for v1.
- **`@compileTimeOnly` markers** on `.each`/`.at`/`.when` (so they only typecheck inside
  `modify`) — recommend replicating quicklens (better errors; `???` bodies + annotation).
- **Full `usingIf`/`apply`-alias / lazy-lens by-name `setToIf`** — recommend full parity
  (runtime-only, cheap).

**Validated:** Hearth 0.3.1-47 already ships `DestructuredExpr`
(`extractFieldPath`/`FieldPath`/`MethodCall`), `CaseClass.caseFieldValuesAt`/`construct`/
`primaryConstructor`, `Enum`/`MatchCase.typeMatch`, `Expr.summonImplicit`, `LambdaBuilder`
— covering every quicklens mechanism. Riskiest unknowns: extension-method receiver recovery
(gap #2) and `.when` non-exhaustive matching (gap #4) — first reproducer candidates.
