# `Type.Ctor2.of[Function1].unapply` returns `Nothing` for the contravariant argument, but only for SUBTYPES of `Function1` (Scala 3)

**Status:** fixed in kubuszok/hearth#389 (Scala 3 `CtorN.of(...).unapply` reads the arguments off the scrutinee's base
type, and widens singletons first). Kindlings dropped its `Type.CtorN.fromUntyped[F](Type.CtorN.of[F].asUntyped)`
wrappers (cats-derivation `Contravariant`/`Invariant`, cats-integration collection/map and `Validated`/`Const`
providers) in favour of plain `Type.CtorN.of[F]`.

**Verdict:** the kindlings claim is half fixed and half still a bug, narrower than described:

- `Type.Ctor2.of[Function1].unapply(Type[Int => Boolean])` returning `Nothing` for the first argument is **fixed** on
  both platforms. It now returns `Some((Int, Boolean))`.
- "A Ctor created in one compilation unit and unapplied in another (via the MacroExtension SPI) needs
  `fromUntyped(of[F].asUntyped)`" was **never true** as stated. Same unit, a different file, and a `ServiceLoader`-loaded
  `MacroExtension` all behave identically. The compilation unit is irrelevant.
- **Still a bug (Scala 3 only):** when the scrutinee is a *subtype* of `Function1` that is not headed by `Function1`
  itself (`class IntPredicate extends (Int => Boolean)`), `Ctor2.of[Function1].unapply` returns
  `Some((Nothing, Boolean))`. The contravariant parameter is solved to its lower bound. `fromUntyped(of.asUntyped)`
  returns the correct `Some((Int, Boolean))`, which explains why the kindlings workaround "worked".

**Platforms:** Scala 3 (tested 3.3.8). Scala 2.13 is correct in all cases.

## Reproducer

`KindlingsFeedbackFixturesImpl.testFunction1Unapply[A]` unapplies `Type[A]` with four constructors:

1. `Type.Ctor2.of[Function1]`, created in the fixture itself;
2. `Type.Ctor2.fromUntyped[Function1](Type.Ctor2.of[Function1].asUntyped)` (the kindlings workaround);
3. `Type.Ctor2.of[Function1]` created in a different compilation unit (`KindlingsFeedbackOtherUnit.function1Ctor(ctx)`);
4. `Type.Ctor2.of[Function1]` created in a `MacroExtension` discovered through `ServiceLoader`
   (`KindlingsFeedbackFunction1Extension`, registered via `macroExtensionTraits` in `build.sbt`, loaded with
   `Environment.loadMacroExtensions`).

Spec `KindlingsFeedbackSpec` group `(7)`:

| `A` | Scala 2.13 (all 4) | Scala 3: 1, 3, 4 (`of`) | Scala 3: 2 (`fromUntyped`) |
|---|---|---|---|
| `Int => Boolean` | `Some((Int, Boolean))` | `Some((Int, Boolean))` | `Some((Int, Boolean))` |
| `String => List[Int]` | ok | ok | ok |
| `IntPredicate` (extends `Int => Boolean`) | `Some((Int, Boolean))` | **`Some((Nothing, Boolean))`** | `Some((Int, Boolean))` |

Scala 3 output:

```
==> X ...(7) Type.Ctor2.of[Function1].unapply / IntPredicate extends (Int => Boolean) (subtype - not headed by Function1)
Ctor2.of[Function1] (other compilation unit):
  - "Some((scala.Int, scala.Boolean))"
  + "Some((scala.Nothing, scala.Boolean))"
Ctor2.of[Function1] (same unit):
  - "Some((scala.Int, scala.Boolean))"
  + "Some((scala.Nothing, scala.Boolean))"
Ctor2.of[Function1] (via MacroExtension SPI):
  - "Some((scala.Int, scala.Boolean))"
  + "Some((scala.Nothing, scala.Boolean))"
```

The `Int => Boolean` and `String => List[Int]` tests pass on Scala 3, and all 3 tests pass on Scala 2.13.

## Root cause

`project/TypeConstructorsGen.scala`, the generated Scala 3 `Ctor2+.Impl.unapply`:

- The fast path (when the dealiased `typeSymbol == fastPathHktSymbol`, i.e. a type headed by `Function1`) reads the
  `AppliedType` arguments directly. This is the change that fixed the original report.
- A class-headed scrutinee that only *extends* `Function1` passes the cheap base-class check and falls through to the
  QuoteMatcher pattern `case '[HKT[a, b]] =>`. Matching `IntPredicate <: Function1[a, b]` instantiates the
  contravariant `a` to its minimal solution, `Nothing`. It is a valid solution for `<:`, but not the type argument the
  user wants.
- `FromUntypedImpl.unapply` (and `CtorK1`) instead use `aRepr.baseType(hktSymbol)` and read its `AppliedType` arguments.
  That is correct for any variance.

## Proposed fix

In the class-headed branch, after the base-class check succeeds, extract the arguments from
`aRepr.baseType(fastPathHktSymbol)`. That is an `AppliedType(_, args)` with the real `Int`/`Boolean`. Only fall back to
the QuoteMatcher for abstract, intersection or bottom heads. This affects every `CtorN` whose HKT has a contravariant
(or invariant-but-inferred) parameter, for example user `trait Consumer[-A]`, not only `Function1`. Add an
`IntPredicate`-style test to the generated `CrossCtorInjectionSpec`, or to `TypesSpec`.

kindlings docs to update: `docs/contributing/hearth-cross-compilation/pitfalls.md` §11 and the SKILL.md rule
"fromUntyped for ALL SPI-boundary type matching". The real rule is "on Scala 3, `of[F].unapply` on a *subtype* of
`F[contravariant, …]` yields `Nothing`; `fromUntyped` (baseType) does not". The compilation unit or SPI has nothing to
do with it.
