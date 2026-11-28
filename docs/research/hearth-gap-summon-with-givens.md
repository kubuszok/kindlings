# Hearth gap: summoning with extra givens, and the reason of a failed search

**Status:** OPEN (Hearth 0.4.5-18-g99ac2ae). Worked around in Kindlings (`derivation-commons`:
`UserInstances`, `UserInstancesPlatform`, `KindlingsDependency`). Found 2026-11-28 from a user report: a user's generic
instance for a field was silently skipped when its dependency had to be derived.

## What a derivation needs

```scala
case class Item(n: Int)
case class Page[A](items: List[A])
object Page {
  implicit def enc[A](implicit a: Encoder[A]): Encoder[Page[A]] = ... // not the structural JSON
}
case class HoldsItems(page: Page[Item])

KindlingsEncoder.derived[HoldsItems]
```

The "use implicit when available" rule summons `Encoder[Page[Item]]`; it fails because `Encoder[Item]` does not exist
(Kindlings would derive it), and the rule chain derives `Page[Item]` structurally: a different JSON, no error.

To use `Page.enc`, the macro has to search `Encoder[Page[Item]]` **as if** an `Encoder[Item]` (that it will derive)
existed, and then build the found instance with the derived one. Hearth has no API for that:
`Expr.summonImplicit`/`summonImplicitIgnoring` search at the expansion site only.

Separately, `SummoningResult.NotFound` drops the reason of the failure. On Scala 3 the reason
(`ImplicitSearchFailure.explanation`) tells a candidate whose macro **threw** (`Exception occurred while executing
macro expansion. java.lang.IllegalStateException: ...`) from a candidate that does not apply; a derivation should fail
on the former instead of deriving the type structurally. (Kindlings 0.4.0 hit this with Hearth 0.4.5's "MIO timeout is
already set" in a nested expansion (fixed in Hearth 985e5e6): the assertion error was read as "not found".)

## Workarounds in Kindlings

**Scala 2:** `c.typecheck` of `{ implicit val d: Encoder[Item] = null.asInstanceOf[Encoder[Item]];
implicitly[Encoder[Page[Item]]] }` at the expansion site (silent). On success, the vals the found tree refers to are the
dependencies it uses; the generated code is the same block with the derived instances instead of the placeholders, and
the instance is resolved again where the code ends up. A candidate's macro that throws is reported by the Scala 2
compiler itself (`exception during macro expansion`), so no reason is needed there.

**Scala 3:** a macro cannot add givens to the context of `Implicits.search`. Searching the context function type
`Encoder[Item] ?=> Encoder[Page[Item]]` looks like it works (`(a: Encoder[Item]) ?=> Page.enc[Item](a)`), but it is
eta-expansion of a candidate with a matching `using` clause, not a search with `a` given: a Scala 2-style
`implicit def` with an `implicit` clause, or any other implicit method in scope (circe's `encodeIterable[A, C[_]](implicit
a: Encoder[A], ev: C[A] => Iterable[A])`), makes it fail. What works is a probe with marker types:

```scala
sealed trait KindlingsDependency0[F[_]]
object KindlingsDependency0 {
  inline given provided[F[_], X]: F[X & KindlingsDependency0[F]] = ... // a macro: null.asInstanceOf[...]
}
```

The probe searches `Encoder[Page[Item & KindlingsDependency0[Encoder]]]`: `Page.enc[A]` infers
`A = Item & KindlingsDependency0[Encoder]`, and its `Encoder[A]` is found in the companion of the marker (part of the
implicit scope of `A`). The markers used in the found tree are the dependencies it needs. The generated code is
`{ given Encoder[Item] = derived; scala.compiletime.summonInline[Encoder[Page[Item]]] }`, resolved again where it ends
up. The given has to be `F[X & Marker[F]]`, not `[T] => T`: an unconstrained given is found for everything (also for the
macro's own `Type[T]` in the companion) and the compiler suggests importing it in unrelated "no given instance" errors;
`[F[_], X] => F[X & Marker]` (without `Marker[F]`) still matches function types through inferred type lambdas.

The reason of a failed search comes from calling `Implicits.searchIgnoring` directly instead of
`summonImplicitIgnoring`.

## What Hearth could offer

1. `SummoningResult.NotFound` (and the others) carrying the compiler's reason, or a `SummoningResult.Crashed` for a
   candidate's macro that threw (Scala 3: the explanation starts with "Exception occurred while executing macro
   expansion").
2. `Expr.summonImplicitWith[A](given: (Type, Expr)*)` - a search with extra local implicit values - returning the
   found expression with the given expressions in place (Scala 2: the typecheck above; Scala 3: the marker probe, or a
   compiler-internal context with the givens entered).

## Kindlings tests

`circe-derivation` and `jsoniter-derivation` `NestedUserInstanceSpec` (shared), `integration-tests`
`NestedDerivationEntryPointSpec` (a library's entry point nested in a derivation, a crashing implicit macro),
`derivation-commons` `UserInstancesSpec` (the reason of a crash).
