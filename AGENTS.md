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

Full details in `docs/contributing/type-class-derivation-skill.md` § "Cross-compilation pitfalls":

- **Path-dependent types in `Expr.quote`** — fails on Scala 2; use `LambdaBuilder`, runtime type witness, or helper method pattern (see `collection-integration-skill.md` §3a)
- **Macro-internal types leak** — `??`, `Expr_??` inside `Expr.quote` cause reification failures; extract to `val` before quote
- **`Array` needs `ClassTag`** — Hearth's `IsCollectionProviderForArray` summons `ClassTag[T]` via `Expr.summonImplicit[ClassTag[T]]` at macro expansion time. If the ClassTag is available in the user's implicit scope, `Array[T]` works automatically. If not, the `IsCollection` match silently skips and derivation fails. For macro-internal arrays (e.g., building `Array` inside `Expr.quote`), use `List` and `::` instead
- **`Expr.upcast` only widens** — use `.asInstanceOf` inside `Expr.quote` for narrowing; also needs `Type[A]` in scope
- **Macro methods need concrete types** — don't wrap macro calls in generic helpers
- **Phantom type param inference** — unconstrained `A` (not in params/return type) infers `Nothing` on Scala 2, `Any` on Scala 3; guard against both
- **Sibling `Expr.splice` isolation (Scala 3)** — each splice gets its own `Quotes`; pre-derive with `LambdaBuilder` in one `runSafe` call
- **`IsMap`/`IsCollection` path-dependent types** — `import isMap.{Key, Value, CtorResult}` before `Expr.quote`
- **`Type.of[A]` bootstrap cycle in extensions** — cross-quotes `Type.of[A]` resolves `implicit Type[A]` at evaluation time; when defining that implicit, causes SOE. Bypass cross-quotes: Scala 2 use `UntypedType.toTyped[A](sc2.c.universe.typeOf[A])`, Scala 3 use `scala.quoted.Type.of[A].asInstanceOf[Type[A]]`; for shared code, move `Type.of` into a helper object where the self-referential implicit is not in scope
- **`ValDefsCache` wrapping scope** — `vals.toValDefs.use` must wrap the outermost expression containing all references
- **`MacroExtension` ClassTag erasure** — `MacroExtension[A & B & C]` ClassTag only preserves first component; use runtime `match`/`asInstanceOf` in `extend()` for custom traits
- **Cross-quotes unused `Type` implicit warnings** — implicit `Type[X]` for `Expr.quote` flagged as "never used" with `-Xfatal-warnings`; wrap in `@nowarn("msg=is never used") def`
- **`IsMap` before `IsCollection` ordering** — `Map <: Iterable` so `IsCollection` matches maps; always check `IsMap` first
- **`summonExprIgnoring` vs OOM** — `Expr.summonImplicit` without ignoring library auto-derivation methods causes infinite macro expansion → OOM → SBT crash; always use `summonExprIgnoring`
- **Newtype aliases in `Expr.quote`** — cats Newtype types (`NonEmptyChain`, `NonEmptyMap`, `NonEmptySet`) fail on Scala 2 with "not found: value data"; use runtime helper pattern (see `collection-integration-skill.md` §3b)
- **`Type.Ctor2.of[Function1].unapply` wrong on Scala 3** — `Impl.unapply` returns `Nothing` for first type arg of `Function1[Int, Boolean]`; always wrap with `Type.Ctor2.fromUntyped[Function1](impl.asUntyped)` for reliable decomposition
- **`primaryConstructor` strict type checking** — `primaryConstructor(Map[String, Expr_??])` checks `Underlying <:< paramType`; can't pass `Expr[Any]` for non-`Any` fields; use helper method pattern to preserve field type through transformations
- **HKT type constructor summoning across compilation boundary** — summoning `ConsK[G]` where `G` is a field's type constructor (e.g., `List` from `List[A]`) requires platform-specific APIs to extract the constructor and build the type; use an abstract bridge method implemented in Scala 2/3 bridges (see `ConsKMacrosImpl.scala`)
- **Erased approach for polymorphic type classes** — Scala 2 macros can't handle free type variables in generated trees; work with `F[Any]` and `Any => Any`, cast with `asInstanceOf` at boundaries; safe due to JVM type erasure (see `FunctorMacrosImpl.scala`)
- **`parTraverse` and `ValDefsCache` (fixed in Hearth 0.2.0-268+)** — prior versions forked cache state per branch, causing exponential re-derivation and 100% CPU hangs on large type graphs; fixed by `MLocal.unsafeSharedParallel` which threads cache writes between branches

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
