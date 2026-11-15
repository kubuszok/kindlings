# Hearth gap: Scala 2 sealed-trait children order depends on the build

**Status:** open (worked around in avro-derivation).

## Symptom

On Scala 2, `Enum.parse[A].directChildren` returns the children of a sealed trait sorted by `symbolOrdering`
(`hearth/untyped/UntypedTypesScala2.scala`): by source position, falling back to the name. When the sealed trait is
compiled in the same run as the macro expansion the children have positions (declaration order); when it comes from a
class file (an incremental build that does not recompile it, or a dependency) they have none, so they are sorted by
name. The same expansion therefore yields a different order depending on what the build recompiled.

Found by the avro4s oracle (`upstream-oracle-tests`, `AvroOracleSpec`): the ENUM symbols of

```scala
sealed trait AvColor
case object AvRed extends AvColor
case object AvGreen extends AvColor
case object AvBlue extends AvColor
```

were `AvRed, AvGreen, AvBlue` after a clean build and `AvBlue, AvGreen, AvRed` after an incremental one.

## Reproducer

Derive anything order-sensitive for a sealed trait in module A, used from module B (or from a file that is not
recompiled): compare the order after `clean` and after touching only the using file.

## Workaround

avro-derivation sorts sealed traits' case objects by full name on Scala 2 (which is also what avro4s 4 does, since
Magnolia sorts subtypes by name), so the output no longer depends on the build. Other modules relying on declaration
order on Scala 2 (e.g. ordinal-based encodings) are affected the same way.

## Suggested fix (Hearth)

Use an order that survives class files on Scala 2 — e.g. the order of the `children` recorded in the sealed class'
pickle (if it is stable), or document that Scala 2 children order is by name unless compiled together.
