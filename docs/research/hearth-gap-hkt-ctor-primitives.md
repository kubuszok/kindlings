# Hearth gap: no cross-platform primitives for type constructors obtained at macro runtime

> **RESOLVED in Hearth `0.3.1-45-gca3bcdb` (verified 2026-06-15).** All six operations are
> now provided cross-platform: `Type.decompose1`/`decompose2` (1+2 extract ctor+args),
> `Type.CtorN.apply`/`fromUntyped` (3 re-apply), `Type.CtorK1.of`/`#apply` + `Expr.summonImplicit`
> (4+5 build `Type[TC[G]]` and summon), `UntypedType.sameTypeConstructorAs` (6 compare).
> `cats-derivation/FunctorMacrosImpl.scala` already uses them. **Kindlings cleanup pending:**
> the remaining HKT bridges (cats-derivation `ConsK`/`Foldable`/`Traverse`, cats-tagless) still
> carry the old per-platform code and are being migrated off it; this doc deletes once they are.
> Original report (now historical) below.

Last updated: 2026-06-12. Status: ~~open~~ **resolved upstream; kindlings migration in progress.**
Filed as [kubuszok/hearth#284](https://github.com/kubuszok/hearth/issues/284).

## Problem

`Type.Ctor1.of[G]` / `Type.Ctor2.of[G]` require a *compile-time literal* type
constructor. HKT derivation (Functor, Traverse, FunctorK, ...) instead works with type
constructors discovered *during expansion* — e.g. "field `x: G[A]` of the case class
being derived; what is `G`?". Hearth offers no cross-platform way to:

1. **Extract** the type constructor from an applied `Type[A]`
   (Scala 2: `tpe.typeConstructor`; Scala 3: `AppliedType(ctor, _)`).
2. **Extract type arguments** from an applied `Type[A]`
   (Scala 2: `tpe.dealias.typeArgs`; Scala 3: `AppliedType(_, args)`).
3. **Re-apply** a constructor obtained that way
   (Scala 2: `c.universe.appliedType(ctor, args)`; Scala 3: `ctor.appliedTo(args)`).
4. **Summon an implicit for a constructed applied type** — e.g. build `Functor[G]` from
   anchor `Functor[_]` + extracted `G`, then search
   (Scala 2: `c.inferImplicitValue`; Scala 3: `Implicits.search`).
5. Work with **higher-order constructors** `Alg[_[_]]` (`CtorK1`-level): apply them to a
   `Ctor1`, get a `Type[Alg[F]]`.
6. **Compare** two constructors for identity (`typeSymbol` equality is what both
   platforms end up using; nothing in Hearth exposes this for constructors).

## Impact in Kindlings

Every HKT module carries a per-platform "bridge" trait implementing 1–6 with raw
`c.universe` / `quotes.reflect` code:

- `cats-derivation`: `FunctorMacros.scala` (63 lines Scala 2 / 76 lines Scala 3),
  `ConsKMacros.scala`, `FoldableMacros.scala`, `TraverseMacros.scala` — ~213 lines.
- `cats-tagless-derivation`: `TaglessKBridgeScala2/3.scala` plus per-typeclass bridges
  (`ApplyKMacros`, `FunctorKMacros`, `ContravariantKMacros`, `InvariantKMacros`,
  `SemigroupalKMacros`, `InstrumentMacros`) — ~577 lines.

Total: **17 files, ~790 lines** of near-identical pairs that differ only in which
compiler API spells the operation. The bridges also force `Type[Any]`/`Expr[Any]`
through their signatures, spreading untyped plumbing into otherwise-typed derivation
code.

## Reproducer

```scala
// Field of the derived case class: x: Option[Int].
// Goal (all at macro-expansion time, cross-platform):
val fieldType: Type[Any] = ???           // Type for Option[Int]

// 1+2: decompose — WANT: Hearth API; HAVE: per-platform raw reflection
// Scala 2:
val tpe  = fieldType.asInstanceOf[c.WeakTypeTag[Any]].tpe
val ctor = tpe.typeConstructor           // Option
val args = tpe.dealias.typeArgs          // List(Int)
// Scala 3:
TypeRepr.of(using fieldType.asInstanceOf[scala.quoted.Type[Any]]).dealias match {
  case AppliedType(ctor, args) => ...    // Option, List(Int)
}

// 4: summon Functor[Option] given only those pieces
// Scala 2: c.inferImplicitValue(appliedType(functorCtor, List(ctor)))
// Scala 3: Implicits.search(functorCtor.appliedTo(ctor))
```

Note `Type.Ctor1.fromUntyped[List](untypedCtor)` *exists* and is the one piece that
works — but getting the `untypedCtor` and doing anything else with it requires dropping
to platform APIs.

## Proposed Hearth API

```scala
object Type {
  /** Decompose an applied type into constructor + args; None if not applied. */
  def decompose1[A: Type]: Option[(Type.Ctor1[AnyK], ??)]           // F[X]
  def decompose2[A: Type]: Option[(Type.Ctor2[AnyK2], ??, ??)]      // F[X, Y]

  object CtorK1 {                                                    // Alg[_[_]]
    def of[HKT[_[_]]](implicit tag: ...): Type.CtorK1[HKT]
    def fromUntyped[HKT[_[_]]](untyped: UntypedType): Type.CtorK1[HKT]
    // .apply[F[_]](ctor: Type.Ctor1[F]): Type[HKT[F]] — already partially present
  }
}

// Summon for a type assembled from extracted parts (anchor-style):
// given anchor Type[TC[Probe]] and a Ctor1, search TC[G].
def summonForCtor1[TC[_[_]], G[_]](anchor: Type[TC[AnyK]], g: Type.Ctor1[G]): Option[Expr_??]

// Constructor identity:
def Type.Ctor1.sameConstructorAs(other: UntypedType): Boolean      // typeSymbol equality
```

Exact shapes negotiable — the requirement is that the six operations above be doable
without `c.universe` / `quotes.reflect`.

## Reference

- `cats-derivation/src/main/scala-2/.../FunctorMacros.scala` and
  `.../scala-3/.../FunctorMacros.scala` — minimal complete pair showing all operations.
- `cats-tagless-derivation/src/main/scala-{2,3}/.../TaglessKBridgeScala{2,3}.scala` —
  the higher-order (`CtorK1`) variants.
- `docs/contributing/hearth-hkt-derivation/SKILL.md` — the derivation patterns forced
  around this gap.
