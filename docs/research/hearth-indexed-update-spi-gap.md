# Hearth gap: no indexed / positional-update SPI for `IsCollection`

## Context

`kindlings-optics` `.each`/`.eachLeft`/`.eachRight` were reimplemented on top of Hearth's std SPI matchers:

| optics step | Hearth utility |
|---|---|
| `.each` / `.eachWhere` over a collection | `IsCollection` (`asIterable` + `factory`/`build`) |
| `.each` over map values | `IsMap` (`key`/`value`/`pair` + `factory`/`build`) |
| `.each` / `.at` over `Option` | `IsOption` (`fold`/`of`/`empty`/`getOrElse`) |
| `.eachLeft` / `.eachRight` | `IsEither` (`fold`/`left`/`right`) |

This made `.each` work over **every** container a provider supports — built-ins, `Array`/`IArray`, java collections,
and cats `NonEmpty*`/`NonEmptyMap` (via `kindlings-cats-integration`) — with no per-type code, just a provider jar on the
classpath.

## The gap

The **indexed** steps `.at(i)` / `.index(i)` / `.atOrElse(i, default)` over a `Seq` (positional) or `Map` (by key) have
**no Hearth SPI equivalent**. `IsCollectionOf`/`IsMapOf` expose only iterate-and-rebuild (`asIterable`, `foreach`,
`factory`, `build`); they cannot express:

- `seq.updated(i, f(seq(i)))` (positional read + replace),
- `seq.appended(default)` (insert past the end, for `atOrElse`),
- `map.get(key)` / `map.updated(key, _)` (keyed read + replace + insert).

So optics still ships a small bespoke runtime type-class family for these — `QuicklensIndexedFunctor[F, Int]` (Seq),
`QuicklensMapAtFunctor[Map, K]` (Map), `QuicklensSingleAtFunctor[Option]` — summoned by the macro. Consequence: `.at`
extensibility is limited to those shapes, whereas `.each` is open via the SPI.

## Proposed Hearth addition

An `IsIndexed`/positional SPI (or extra methods on `IsCollectionOf` for ordered/indexed collections), e.g.

```scala
trait IsIndexedOf[CollA, Idx, Item] {
  def get(coll: Expr[CollA], idx: Expr[Idx]): Expr[Option[Item]]
  def updated(coll: Expr[CollA], idx: Expr[Idx], value: Expr[Item]): Expr[CollA]
  // optionally: insert/append semantics for the `.atOrElse` "past the end" case
}
```

with providers for `Seq`-like (Int-indexed) and `Map` (key-indexed) collections, mirroring how `IsCollection`/`IsMap`
providers are registered. That would let optics drop its last bespoke runtime type classes and make `.at`/`.index`/
`.atOrElse` classpath-extensible too (cats `NonEmptyMap`, custom keyed structures, …).

## Status

Reported as a Hearth enhancement request. Until it lands, the `.at` family stays on the bespoke
`QuicklensIndexedFunctor`/`QuicklensMapAtFunctor`/`QuicklensSingleAtFunctor` in
`optics/src/main/scala/hearth/kindlings/optics/QuicklensFunctors.scala`.
