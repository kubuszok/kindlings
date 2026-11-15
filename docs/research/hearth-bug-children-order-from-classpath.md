# Hearth bug: order of `directChildren` depends on where the sealed type was compiled (Scala 3)

Status: **FIXED** in Hearth `1bba7e2b` (first in `0.4.4-10-ga030def-SNAPSHOT`): Scala 3 children now come in
declaration order (the order `Mirror.SumOf` uses) also for types read from the classpath. Scala 3 only (for Scala 2, where
class files only know the names, see `hearth-gap-scala2-children-order.md`: Kindlings uses `ChildrenOrder.ByName` there).
Was open in Hearth `0.4.4-6-gb8ae5aa-SNAPSHOT`. Found 2026-11-15 by the circe upstream oracle
(`upstream-oracle-tests`, `CirceGenericOracleSpec` "strict decoding with defaults").

## Symptom

```scala
sealed trait CiShape
final case class CiCircle(radius: Double) extends CiShape
final case class CiRectangle(widthCm: Double, heightCm: Double) extends CiShape
case object CiNoShape extends CiShape
```

`Type[CiShape].directChildren` lists the subtypes

- in declaration order (`CiCircle, CiRectangle, CiNoShape`) when `CiShape` is compiled in the same compiler run as the
  macro expansion, but
- in alphabetical order (`CiCircle, CiNoShape, CiRectangle`) when `CiShape` comes from the classpath (another module, or
  an earlier incremental compilation of the same module).

So the same sources give different macro output depending on incremental compilation, and every type defined in a
dependency is seen in alphabetical order. The compiler's own `Mirror.SumOf` (and so circe-core's derivation) always uses
declaration order.

Observable in Kindlings' circe derivation: the strict wrapper failure (`Strict decoding CiShape - expected a single key
json object with one of: ...`, circe's `constructorNames`) lists the subtypes alphabetically for a type from the
classpath, where circe lists them in declaration order. Anything else that depends on the subtype order is affected the
same way (e.g. "the first intermediate in declaration order" for a leaf under several intermediate sealed traits; enum
symbol order in schema-producing modules).

## Cause

`UntypedTypesScala3.directChildrenList` sorts `classificationSymbol(instanceTpe).children` with `symbolOrdering`:
`Ordering.by(_.pos).orElse(name)`. Symbols loaded from TASTy on the classpath have no `pos` during macro expansion, so
the ordering falls back to the names. The unsorted `children` list is already in declaration order (it is what
`Mirror.SumOf` uses), for both cases.

A possible fix: keep the `children` order (it is the order the compiler uses for `Mirror` ordinals), or sort by position
only when every child has one.

## Reproducer in Kindlings

The circe oracle's fixtures live in `upstream-oracle-tests/src/main/scala` (so the tests always read them from the
classpath). `CirceGenericOracleSpec`/`CirceCoreOracleSpec` listed the strict-wrapper message order as an expected
difference on Scala 3 (`hearthChildrenOrder`); it was removed with the Hearth bump, and the fixtures stay in the main
sources as the regression test (any order difference now fails the oracle).

Hearth test shape: a sealed trait with children declared in non-alphabetical order, compiled in a separate module (or
test fixture compilation unit) from the macro call; `Type[T].directChildren.keys` must be the declaration order.
