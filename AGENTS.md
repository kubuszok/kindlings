# AGENTS

## Project Overview

**Kindlings** provides type classes (re)implemented with [Hearth](https://github.com/kubuszok/hearth/) to kick-off the ecosystem — cross-compiled (Scala 2.13 + 3) type class derivation using Hearth's macro-agnostic APIs.

- **Scala Versions**: 2.13.18, 3.8.2 | **Platforms**: JVM, Scala.js, Scala Native | **Build Tool**: SBT

## MCP — Always verify APIs via MCP before using them

Metals MCP at `.metals/mcp.json` (`kindlings-metals`). Query definitions/types/symbols, check compilation errors before running sbt. Fix MCP issues before running sbt to avoid long feedback loops.

## Global Rules

 - **Always use `sbt --client`** — never bare `sbt`. Connects to running server instead of launching new JVM.
 - **Redirect sbt output**: `sbt --client "module/clean ; test-jvm-2_13" 2>&1 | tee /tmp/sbt-output.txt`
   Then inspect with `grep`/`tail`/`head`. Only re-run sbt if code was modified.
 - Only run `sbt compile`/`sbt test` after MCP shows no compilation issues.
 - **Do not modify `dev.properties`** — used by the developer for IDE platform focus.
 - **Do not add yourself as co-author** — no `Co-Authored-By: Claude ...` in commits.

### Project matrix structure

Uses **sbt-projectmatrix** (not sbt-crossproject). **Do NOT use** `++` version switching or `scalaCrossVersion`.

**Naming**: no suffix = Scala 2.13 JVM, `3` = Scala 3 JVM, `JS`/`JS3` = Scala.js, `Native`/`Native3` = Scala Native.
Example: `fastShowPretty` (2.13 JVM), `fastShowPretty3` (3 JVM), `fastShowPrettyJS3` (3 JS).

### Test aliases

| `test-jvm-2_13` | `test-jvm-3` | `test-js-2_13` | `test-js-3` | `test-native-2_13` | `test-native-3` |
|---|---|---|---|---|---|

**JVM-only by default** — JS/Native are slow (Native can OOM). CI covers all platforms.

## Incremental Compilation Gotchas

 - **Always clean after macro changes** — incremental compilation does NOT re-expand macros.
 - Clean specific module: `sbt --client "module/clean ; module3/clean ; test-jvm-2_13 ; test-jvm-3"`
 - Nuclear option: `sbt --client clean` then `sbt --client "test-jvm-2_13 ; test-jvm-3"`

## Cross-Quotes and Macro-Agnostic APIs

Code uses `Expr`, `Type`, etc. from **Hearth's API**, NOT `scala.quoted.Expr` or `c.Expr`:
- `Expr.quote { ... Expr.splice { ... } }` — cross-platform quotes/splices
- `Type.of[A]`, `Type.Ctor1.of[F]` — cross-platform type representations
- Traits mix in `MacroCommons` and `StdExtensions` for the full API
- Scala 3: `inline def` + `${ ... }` with `MacroCommonsScala3`; Scala 2: `def ... = macro` with `MacroCommonsScala2`

## Cross-Compilation Pitfalls

See `docs/contributing/cross-compilation-pitfalls-skill.md` for the full live list.
The most load-bearing entries to keep in mind for any macro work:

- **Sibling `Expr.splice` isolation (Scala 3)** — each splice gets its own `Quotes`; for
  multi-method type-class instances **derive everything at `Q0` using `ValDefsCache`,
  then call cached `def`s by name from inside each splice**. See
  `docs/contributing/def-caching-skill.md`. Do **not** use `LambdaBuilder` as a
  workaround — that was a misdiagnosis; the real fix is `cache.toValDefs.use` placement.
- **`summonExprIgnoring` vs OOM** — always pass the library auto-derivation methods to
  ignore, otherwise summoning recurses into them and OOMs the compiler.
- **`IsMap` before `IsCollection`** — `Map <: Iterable`, so check `IsMap` first.
- **`Type.Ctor2.of[Function1].unapply` wrong on Scala 3** — wrap with
  `Type.Ctor2.fromUntyped[Function1](impl.asUntyped)`.
- **`Type.of[A]` bootstrap cycle in extensions** — see the pitfalls skill.

## Standard extensions, `LambdaBuilder`, and def-caching rules

- **Load standard extensions exactly once per macro bundle.** See
  `docs/contributing/load-standard-extensions-skill.md`. The shared implementation
  lives in `derivation-commons` (`hearth.kindlings.derivation.compiletime.LoadStandardExtensionsOnce`);
  per-module thin delegates re-export it in each module's `internal.compiletime` package.
  Use `ensureStandardExtensionsLoaded()` instead of calling
  `Environment.loadStandardExtensions()` directly. Issue `kubuszok/kindlings#65`.
- **`LambdaBuilder` only for collection/Optional iteration lambdas.** Strict rule, no
  exceptions. See `docs/contributing/lambda-builder-when-to-use-skill.md`. For
  multi-method type-class instances or any other shape that needs pre-derived helper
  functions, use the def-caching pattern instead.
- **Def-caching is the answer to multi-method instances and sibling-splice isolation.**
  See `docs/contributing/def-caching-skill.md`. The recipe: shared `ValDefsCache`,
  derivation in `Q0` via `forwardDeclare` + `buildCachedWith`, helper-call functions
  retrieved via `cache.getNAry`, outer `cacheState.toValDefs.use` wrapping the entire
  `Expr.quote { new T[A] { … } }`. Reference implementations: `cats-derivation`
  `HashMacrosImpl` (two-method) and `jsoniter-derivation`
  `deriveCombinedCodecTypeClass` (five-method, parallel derivation via `parTuple`).
- **Multi-method derivation must use `parTuple` for error aggregation.** Sequential
  `for`-comprehensions short-circuit on the first failure, hiding errors from
  later-derived method bodies. `parTuple` (on `MIO`) runs both derivations and
  aggregates errors. The exception is when two derivations share `MLocal` state via the
  rule chain — parallel branches fork `MLocal`, so a branch that reads state populated
  by another branch will silently see a stale fork. See
  `def-caching-skill.md` § "Deriving multiple method bodies in PARALLEL via parTuple".
- **User-provided implicits must override built-in derivation rules.** Order rules so
  that `*UseImplicitWhenAvailableRule` runs before any built-in / value-type / collection
  rules; always go through `summonExprIgnoring` with the library's auto-derivation
  methods passed in.
- **Cache summoned implicits as `lazy val`s, intermediate recursive derivations as
  `def`s.** Use `ValDefBuilder.ofLazy` for summoned/standalone instances and
  `ValDefBuilder.ofDef*` (with `cache.forwardDeclare` + `cache.buildCachedWith`) for
  recursive helpers.

## Configurable derivation timeout

All derivation modules use `DerivationTimeout` from `derivation-commons`
(`hearth.kindlings.derivation.compiletime.DerivationTimeout`). Default: 120 seconds.
Override per-module via `-Xmacro-settings`:

```
-Xmacro-settings:jsoniterDerivation.timeout=300s
-Xmacro-settings:catsDerivation.timeout=5m
-Xmacro-settings:circeDerivation.timeout=5000ms
```

Supported formats: plain integer (seconds), `Ns`/`Nseconds`, `Nms`/`Nmilliseconds`, `Nm`/`Nminutes`.
Each module reads from its own settings namespace (e.g. `jsoniterDerivation`, `catsDerivation`).
Invalid values emit a compiler warning and fall back to default.

Hearth source is at `../hearth/` when documentation is insufficient.
See `docs/contributing/hearth-documentation-skill.md` § "Hearth source as reference" for key files.

## Hearth documentation and API reference

- **`docs/contributing/hearth-documentation-skill.md`** — finding docs, verifying APIs, hearth source reference
- **`docs/contributing/hearth-api-knowledge.md`** — quick-reference table of commonly used hearth API signatures
- Check `build.sbt` for `versions.hearth` — use `https://scala-hearth.readthedocs.io/en/latest/` for SNAPSHOT, `/en/<version>/` for stable

## Skills routing

### Type class derivation — follow `docs/contributing/type-class-derivation-skill.md`

- `FastShowPrettyMacrosImpl.scala` — reference for **encoder-style** derivation (reading fields)
- `circe-derivation/DecoderMacrosImpl.scala` — reference for **decoder-style** derivation (constructing types)
- `jsoniter-derivation/CodecMacrosImpl.scala` — reference for **combined codec** (encoder + decoder, `LambdaBuilder` pattern)

Also in `type-class-derivation-skill.md`: "Implementing a new module", "Debugging derivation", "Syncing from Hearth", "Polymorphic (HKT) type class derivation".

### Cats type class derivation — `cats-derivation/` module

Derives type classes from cats/alleycats for case classes and sealed traits:

- **Monomorphic** (kind `*`): Show, Eq, Order, PartialOrder, Hash, Semigroup, Monoid, CommutativeSemigroup, CommutativeMonoid, Empty
- **Polymorphic** (kind `* → *`): Functor, Contravariant, Invariant, Apply, Applicative, Foldable, Traverse, Reducible, NonEmptyTraverse, SemigroupK, MonoidK, Pure, EmptyK, NonEmptyAlternative, Alternative, ConsK

Key reference files:
- `SemigroupMacrosImpl.scala` — monomorphic derivation (summon field instances, combine pairwise)
- `FunctorMacrosImpl.scala` — polymorphic derivation (erased approach, two-probe field classification)
- `ConsKMacrosImpl.scala` — polymorphic with nested field handling (bridge method for type constructor summoning, runtime helper, carry-and-absorb algorithm)
- `ContravariantMacrosImpl.scala` — Function1 field classification using `Type.Ctor2.fromUntyped`
- `ApplicativeMacrosImpl.scala` — composed derivation (pure + map + ap body builders)
- `NonEmptyAlternativeMacrosImpl.scala` — multi-trait composition (Applicative + SemigroupK patterns)

### Collection & map integration — follow `docs/contributing/collection-integration-skill.md`

- `cats-integration/CatsCollectionAndMapProviders.scala` — reference for **collection/map providers** (NonEmptyList, NonEmptyVector, NonEmptyChain, Chain, NonEmptyMap, NonEmptySet)
- `cats-integration/runtime/CatsConversions.scala` — reference for **runtime helper pattern** (Newtype aliases)

Key patterns: helper method pattern (path-dependent types), runtime helper pattern (Newtype aliases), `fromUntyped` for cross-compilation-boundary matching.

### Fixing a bug

1. Write a failing test reproducing the bug
2. Clean affected modules (incremental compilation does not re-expand macros)
3. Verify the test fails, apply fix, clean again, verify all tests pass
4. Cross-platform if relevant: `sbt --client "test-js-3 ; test-native-3"`

### Finishing work in a worktree

When done with a worktree, suggest `sbt --client shutdown` to free memory — each worktree gets its own SBT server that persists after the worktree is removed.
