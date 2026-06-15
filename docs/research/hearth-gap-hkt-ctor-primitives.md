# Hearth gap: higher-order (`CtorK1`-argument) type-constructor primitives at macro runtime

Last updated: 2026-06-15. Status: **partially resolved.** The first-order (`Ctor1`, `* → *`)
case is fixed upstream and migrated off in Kindlings; the **higher-order (`CtorK1`-argument,
`(* → *) → *`) case is still open**. Filed as
[kubuszok/hearth#284](https://github.com/kubuszok/hearth/issues/284).

## What was the gap

HKT derivation works with type constructors discovered *during expansion* (e.g. "field
`x: G[A]` of the case class being derived — what is `G`, and can I summon `TC[G]`?").
Originally Hearth had no cross-platform way to extract / re-apply / summon-for such a
discovered constructor, forcing per-platform `c.universe` / `quotes.reflect` bridges in every
HKT module.

## RESOLVED: first-order constructors (`Ctor1`, field type `G[A]` with `G: * → *`)

Hearth `0.3.1-45-gca3bcdb` (verified 2026-06-15) provides everything needed for the
first-order case, cross-platform:

- `Type.decompose1[A]: Option[(Ctor1[AnyK1], ??)]` / `Type.decompose2[A]` — extract ctor + args.
- `Type.Ctor1.fromUntyped` / `Ctor1#apply` / `Ctor2#apply` — re-apply a discovered ctor.
- `Type.CtorK1.of[TC]` + `Type.CtorK1#apply(ctor1)` builds `Type[TC[G]]`; then
  `Expr.summonImplicit[…]` summons the type class — valid when `TC`'s slot is `_` (i.e.
  `TC[_[_]]` applied to a `* → *` constructor, like `cats.Functor`).
- `UntypedType.sameTypeConstructorAs` / `Type.hasSameTypeConstructor` — constructor identity.
- `Type.fqcn[A]` — name-based matching.

**Kindlings migration (done):** `cats-derivation` no longer carries any first-order bridge
code — `FunctorMacrosImpl`, `ConsKMacrosImpl`, `FoldableMacrosImpl`, `TraverseMacrosImpl` use
`Type.decompose1` + `Type.CtorK1.of` + `Expr.summonImplicit` directly. All
`c.inferImplicitValue` / `Implicits.search` / field-level `appliedType`+`typeConstructor`
reflection is gone (only the statically-known top-level-`F` entry wiring remains, which is not
this gap). 279/269 tests green on 2.13 + 3 × JVM/JS/Native.

## STILL OPEN: higher-order constructors (`CtorK1` argument, field type `Alg[F]` with `Alg: (* → *) → *`)

`cats-tagless-derivation` cannot migrate, because its algebras are `Alg[_[_]]` (kind
`(* → *) → *`). For a nested field `InnerAlg[Option]`:

1. **No `decomposeK1`.** `Type.decompose1` yields a `Ctor1[AnyK1]` (a `* → *` constructor) — it
   cannot represent the field's `(* → *) → *`-kinded constructor `InnerAlg`. There is no
   `decomposeK1: Option[(CtorK1[AnyHKT], …)]`.
2. **`CtorK1.of` is ill-kinded for the type class.** `cats.tagless.InvariantK` (and
   `FunctorK`/`ContravariantK`/`SemigroupalK`/`ApplyK`) has kind `((* → *) → *) → *`, so
   `Type.CtorK1.of[cats.tagless.InvariantK]` fails to compile (*"type `_` has 1 type parameter,
   but type `_` has 0"*) — `CtorK1`'s argument slot is `_` (`* → *`), not `_[_]`.
3. **No higher-order re-apply.** There is no cross-platform way to re-apply a
   `((* → *) → *) → *` constructor to a discovered `(* → *) → *` constructor (the bridges use
   raw `c.universe.appliedType` / `quotes.reflect.appliedTo` on opaque `UntypedType`s).

So `cats-tagless-derivation` keeps its per-platform bridges (`TaglessKBridgeScala2/3` + the
per-typeclass bridges for `ApplyK`/`FunctorK`/`ContravariantK`/`InvariantK`/`SemigroupalK`/
`Instrument`).

### Precise remaining Hearth ask

1. `Type.decomposeK1[A]: Option[(CtorK1[AnyHKT], ??)]` — decompose an applied type whose
   constructor takes a `_[_]` argument (e.g. `InnerAlg[Option]` → the `(* → *) → *` constructor
   `InnerAlg` + the `* → *` arg `Option`).
2. A `CtorK2`-style (or `CtorK1`-of-`CtorK1`) `apply` that re-applies a `((* → *) → *) → *`
   type-class constructor to a discovered `CtorK1` — i.e. build `Type[TC[Alg]]` from
   `Type[TC]`-as-higher-order-ctor + a `CtorK1` for `Alg` — then summon.
3. (Equivalently) a cross-platform `UntypedType.appliedTo(args)` re-apply primitive that works
   for arbitrary-kinded arguments, replacing `c.universe.appliedType` / `quotes.reflect.appliedTo`.

When these land, the cats-tagless migration is a near-mechanical mirror of the cats-derivation
one (shared `constructTypeClassTypeForField[TC[_[_]]](fieldType, tcCtor)`, drop the
`TaglessKBridgeScala{2,3}` mixins from the 6 entry classes per platform, delete both bridge
files) — at which point this doc can be deleted.

## Reference

- **Migrated (first-order) pattern:** `cats-derivation/.../FunctorMacrosImpl.scala`,
  `ConsKMacrosImpl.scala`, `FoldableMacrosImpl.scala`, `TraverseMacrosImpl.scala`.
- **Still-bridged (higher-order):** `cats-tagless-derivation/src/main/scala-{2,3}/.../TaglessKBridgeScala{2,3}.scala`.
- `docs/contributing/hearth-hkt-derivation/SKILL.md` — the derivation patterns.
