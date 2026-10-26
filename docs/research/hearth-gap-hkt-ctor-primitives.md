# Hearth gap: higher-kinded type-constructor primitives at macro runtime

Last updated: 2026-10-25. Status: **RESOLVED** in Hearth 0.4.x
([kubuszok/hearth#284](https://github.com/kubuszok/hearth/issues/284) closed, `applyTypeArgs` semantics aligned in
#312). Both `cats-derivation` and `cats-tagless-derivation` have moved to the cross-platform API (verified with
Hearth 0.4.3 on 2.13 and 3 JVM). Neither module has field-level `c.universe` or `quotes.reflect` reflection any more.
What is left is one line of entry glue per Scala 2 macro, tracked separately in
[`hearth-gap-scala2-ctor-of-macro-type-param.md`](hearth-gap-scala2-ctor-of-macro-type-param.md).

## What the gap was

HKT derivation works with type constructors that are only discovered *during expansion*. For example: a field
`x: G[A]` of the case class being derived has some constructor `G`, and the macro has to find `G` and summon `TC[G]`
for it. Hearth originally had no cross-platform way to extract such a constructor, re-apply it, or summon for it.
Every HKT module therefore needed per-platform `c.universe` / `quotes.reflect` bridges.

## First-order constructors (`G: * -> *`): resolved earlier (0.3.1-45)

- `Type.decompose1[A]` / `Type.decompose2[A]` extract a constructor and its args.
- `Type.Ctor1.fromUntyped` and `Ctor1#apply` re-apply a discovered constructor.
- `Type.CtorK1.of[TC]` + `CtorK1#apply(ctor1)` build `Type[TC[G]]`; `Expr.summonImplicit` then summons it.
- `UntypedType.sameTypeConstructorAs` / `Type.hasSameTypeConstructor` compare constructors.

`cats-derivation` (`FunctorMacrosImpl`, `ConsKMacrosImpl`, `FoldableMacrosImpl`, `TraverseMacrosImpl`) uses these.
Since 2026-10-25 the top-level `Type[TC[F]]` is also built in shared code: `Type.CtorK1.of[TC].apply[F](using FCtor)`
for `F[_]` type classes, and `UntypedType.applyTypeArgs(Type.of[TC[Either]].asUntyped, List(FCtor.asUntyped))` for
`Bifunctor`/`Bifoldable`/`Bitraverse`. This replaced 19 Scala 2 `appliedType(weakTypeOf[TC[Any]].typeConstructor, …)`
blocks and 19 Scala 3 `scala.quoted.Type.of[TC[G]]` helpers.

## Higher-order constructors (`Alg: (* -> *) -> *`): resolved 2026-10-25

The earlier version of this doc asked for `decomposeK1`, a `CtorK2`, or "a cross-platform re-apply for
arbitrary-kinded arguments". The last of these was enough, and Hearth 0.4.x has it as the `UntypedType` primitives
(`hearth/untyped/UntypedTypes.scala`), implemented on both platforms:

| need | Hearth API |
|---|---|
| strip aliases | `UntypedType.dealias` |
| the args of `InnerAlg[Option]` | `UntypedType.typeArguments` |
| the constructor `InnerAlg` | `UntypedType.typeConstructor` (dealiases; on Scala 3 it returns the `AppliedType`'s `tycon`) |
| `InvariantK[DummyHKT]` re-applied to `InnerAlg` | `UntypedType.applyTypeArgs(anchor, List(ctor))` (applies to the anchor's constructor on both platforms since #312) |
| back to the typed layer | `UntypedType#asTyped[A]`, `Type.CtorK1.fromUntyped[HKT]` |

`cats-tagless-derivation` now uses them in shared code (`InvariantKMacrosImpl`):

- `constructTypeClassTypeForField`: dealias, require exactly one type argument, take the constructor, and
  `applyTypeArgs` it into the type-class anchor. This replaces `TaglessKBridgeScala2` / `TaglessKBridgeScala3` (both
  deleted).
- `applyTypeClassToAlgebra`: builds `Type[FunctorK[Alg]]` / `InvariantK` / `ContravariantK` / `SemigroupalK` /
  `ApplyK` / `Instrument` for the entry algebra. This replaces the Scala 2 `appliedType` blocks and the Scala 3
  `mkXKType` helpers.
- `mkWCtor1/2/3` = `Type.Ctor1.of[CatsTaglessFactories.WCtorN]` in shared code (the `type WCtorN[A] = Any` aliases
  work on both platforms). This replaces the Scala 2 `weakTypeOf[WCtorN[Any]].typeConstructor` and the Scala 3
  `mkCtor1` copies.
- The algebra name for `Instrument` is `AlgCtorK1.apply[Option].shortName` (cross-platform). This replaces Scala 2
  `typeSymbol.name` and Scala 3 `TypeRepr.of[HKT].typeSymbol.name`.

Behaviour note: Scala 2 used to accept nested fields applied to *any* number of arguments, and did not dealias them.
The shared implementation follows the old Scala 3 semantics (dealias, exactly one argument), which matches
`Type.decompose1`. A test for a nested algebra referenced through a type alias
(`InvariantKSpec` "nested algebra through a type alias") passes on both platforms.

## Reference

- `cats-derivation/src/main/scala/.../*MacrosImpl.scala`: `derive<TC>[F](FCtor0)` builds its own `Type[TC[F]]`.
- `cats-tagless-derivation/src/main/scala/.../InvariantKMacrosImpl.scala`: `applyTypeClassToAlgebra`,
  `constructTypeClassTypeForField`.
- `docs/contributing/hearth-hkt-derivation/SKILL.md`: the derivation patterns.
