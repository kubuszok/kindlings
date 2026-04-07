# Cross-compilation pitfalls

This file is the live reference for "things that bite when writing cross-compiled
Kindlings macros". Each entry describes the failure mode, the platform(s) it affects, and
the workaround. For the higher-level rules around extension loading and `LambdaBuilder`
scope see `load-standard-extensions-skill.md` and `lambda-builder-when-to-use-skill.md`.

## Splice and quote scoping

### Sibling `Expr.splice` isolation (Scala 3)

Each `Expr.splice` inside a single `Expr.quote { … }` receives its own `Quotes` instance
on Scala 3. A `def` or closure that captures an outer `Quotes` and is invoked from inside
a splice triggers `scala.quoted.runtime.impl.ScopeException: Expression created in a
splice was used outside of that splice`.

The fix is **not** `LambdaBuilder` (an earlier version of this document said it was —
that was a misdiagnosis). The fix is to keep all derivation outside any splice and emit
the per-method bodies as cached `def`s via `ValDefsCache`:

1. Run derivation at `Q0` (the outer `MIO.scoped { runSafe => … }` block) using a
   shared `ValDefsCache`.
2. For each method, `forwardDeclare` a cached `def` and populate its body via
   `buildCachedWith`.
3. Retrieve `cache.getNAry` helper-call functions.
4. Wrap the outer `Expr.quote { new T[A] { … } }` with `cacheState.toValDefs.use`.
5. Inside each method body's splice, call the helper function — this builds a
   *name reference* to the outer-scope cached def, not an `Expr` value carrying its own
   `Quotes`.

Full recipe and worked examples (cats `HashMacrosImpl`, jsoniter combined codec) in
`def-caching-skill.md`.

### `Expr.quote { (x: T) => … }` is supported

Cross-quotes accept Scala function literals directly: `Expr.quote { (x: T) => body }`
returns `Expr[T => …]`. Use this whenever the lambda is **not** going to be passed into a
collection iteration helper — it is far simpler than `LambdaBuilder` and produces the
same generated code.

If the body needs to capture a path-dependent reference (e.g. `isValueType.value.unwrap`),
extract a helper method that closes over the path-dependent value and returns the
`Expr[T => U]`. The helper isolates the path-dependent reference from the cross-quotes
reifier.

### Path-dependent types in `Expr.quote`

Fails on Scala 2 ("not found: type Underlying" or similar). Fixes:

- Move the body into a helper method with regular type parameters
  (`mkXyz[A, E]`).
- Or use a runtime type witness pattern.
- Or use the helper-method pattern documented in `collection-integration-skill.md` §3a.

### Macro-internal types leaking into `Expr.quote`

`??`, `Expr_??`, and other macro-internal existentials inside an `Expr.quote` cause
reification failures. Extract them to a `val` (with concrete `import …Underlying`) before
the quote.

### `Expr.upcast` only widens

It cannot narrow. Use `.asInstanceOf[T]` *inside* an `Expr.quote` for narrowing. Requires
`Type[T]` in scope.

### `ValDefsCache` wrapping scope

`vals.toValDefs.use { _ => result }` must wrap the **outermost** expression that contains
all references to those vals. Wrapping a nested sub-expression hides cached vals from
other branches and emits duplicates.

## Type system details

### Macro methods need concrete types

You cannot wrap macro calls in generic helper methods — the call site needs the concrete
`Type[A]` available. Always inline the macro call or use a separate macro per concrete
type.

### Phantom type-parameter inference

An unconstrained `A` (not appearing in any value parameter or return type) is inferred as
`Nothing` on Scala 2 and `Any` on Scala 3. Guard against both: assert
`Type[A] =:= Type.of[Nothing]` and `Type[A] =:= Type.of[Any]` in entry points and report a
clear "type parameter inferred as Nothing/Any" error.

### `Type.of[A]` bootstrap cycle in extensions

Cross-quotes `Type.of[A]` resolves the implicit `Type[A]` lazily at evaluation time. When
defining that very implicit, you get a stack overflow.

Bypass cross-quotes:

- Scala 2: `UntypedType.toTyped[A](sc2.c.universe.typeOf[A])`
- Scala 3: `scala.quoted.Type.of[A].asInstanceOf[Type[A]]`

For shared code that should not see the self-referential implicit, move the call into a
helper object where that implicit is not in scope.

### `IsMap` before `IsCollection` ordering

`Map[K, V] <: Iterable[(K, V)]`, so the `IsCollection` rule will swallow `Map` types if
checked first. Always pattern-match on `IsMap` before `IsCollection`.

### `Type.Ctor2.of[Function1].unapply` wrong on Scala 3

`Impl.unapply` returns `Nothing` for the first type arg of `Function1[Int, Boolean]` on
Scala 3 (compilation-context-sensitive matching). Always wrap with
`Type.Ctor2.fromUntyped[Function1](impl.asUntyped)` for reliable decomposition. Same fix
applies to any `Type.Ctor2` use across SPI boundaries — see `MEMORY.md` for more context.

### `primaryConstructor` strict type checking

`primaryConstructor(Map[String, Expr_??])` checks `Underlying <:< paramType`; you cannot
pass `Expr[Any]` for non-`Any` fields. Use the helper-method pattern to preserve field
types through transformations.

### `Array` needs `ClassTag`

Hearth's `IsCollectionProviderForArray` summons `ClassTag[T]` via
`Expr.summonImplicit[ClassTag[T]]` at expansion time. If the user has it in scope,
`Array[T]` works automatically. If not, the `IsCollection` match silently skips and
derivation fails. For **macro-internal** arrays (built inside `Expr.quote`), use `List`
and `::` instead — they do not need a `ClassTag`.

## Implicit summoning

### `summonExprIgnoring` vs OOM

`Expr.summonImplicit` without ignoring the library's auto-derivation methods causes
infinite macro expansion → OOM → SBT crash. **Always** call `summonExprIgnoring(…)` and
pass the methods you want to exclude.

### Newtype aliases in `Expr.quote`

cats Newtype types (`NonEmptyChain`, `NonEmptyMap`, `NonEmptySet`) fail on Scala 2 with
`"not found: value data"`. Use the runtime helper pattern (see
`collection-integration-skill.md` §3b).

### Cross-quotes "unused `Type` implicit" warnings

Implicit `Type[X]` declared for an `Expr.quote` is flagged as `"never used"` with
`-Xfatal-warnings`. Wrap the declaration in `@nowarn("msg=is never used") def`.

## HKT (kind `* → *`) derivation

### HKT type constructor summoning across compilation boundary

Summoning `ConsK[G]` where `G` is a field's type constructor (e.g. `List` from
`List[A]`) requires platform-specific APIs to extract the constructor and re-build the
type. Use an abstract bridge method implemented in Scala 2/3 bridge classes — see
`ConsKMacrosImpl.scala`.

### Erased approach for polymorphic type classes

Scala 2 macros cannot handle free type variables in generated trees. Work with `F[Any]`
and `(Any => Any)`, cast with `.asInstanceOf` at the boundaries — safe due to JVM type
erasure. See `FunctorMacrosImpl.scala`.

### `MacroExtension` ClassTag erasure

`MacroExtension[A & B & C]` ClassTag only preserves the first component. Use a runtime
`match`/`asInstanceOf` in `extend()` for custom traits.

## Resolved Hearth issues (for context only)

These bullets used to belong in this list because they bit Kindlings work directly. They
have since been fixed upstream in Hearth and are kept here only so that an old PR
description or memory entry that references them is not confusing.

- **`parTraverse` and `ValDefsCache`** — fixed in Hearth `0.2.0-268+` by
  `MLocal.unsafeSharedParallel`. Earlier versions forked cache state per branch, causing
  exponential re-derivation and 100% CPU hangs on large type graphs.
- **`loadStandardExtensions` repeated registration** — fixed in Hearth `0.2.0-257`.
  Hearth now deduplicates already-applied extensions internally, but the call still has
  ServiceLoader cost and emits info logs — see `load-standard-extensions-skill.md` for
  the project-level rule of "load once per bundle".
- **Cross-quotes `Type.Ctor1` resolution in HKT contexts** — fixed in Hearth
  `0.2.0-263+`.
