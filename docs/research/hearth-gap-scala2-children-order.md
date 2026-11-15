# Hearth gap: Scala 2 sealed-trait children order depends on the build

**Status:** fixed in Hearth `1bba7e2b` (first in `0.4.4-10-ga030def-SNAPSHOT`): Hearth documents the Scala 2 rule and
adds `Type.directChildrenInOrder(ChildrenOrder.ByName)`, an order that is the same in every build. Kindlings uses it
consistently on Scala 2 (see "Resolution" below).

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

## Workaround (before the Hearth fix)

avro-derivation sorts sealed traits' case objects by full name on Scala 2 (which is also what avro4s 4 does, since
Magnolia sorts subtypes by name), so the output no longer depends on the build. Other modules relying on declaration
order on Scala 2 (e.g. ordinal-based encodings) are affected the same way.

## Suggested fix (Hearth)

Use an order that survives class files on Scala 2 — e.g. the order of the `children` recorded in the sealed class'
pickle (if it is stable), or document that Scala 2 children order is by name unless compiled together.

## Resolution (Hearth `0.4.4-10-ga030def-SNAPSHOT`)

Scala 2 class files store the subtypes of a sealed type sorted by name, so the declaration order is only known for a
sealed type compiled in the same run; Hearth keeps reporting the declaration order when it knows it
(`ChildrenOrder.DeclarationWhenAvailable`, the order of `directChildren`) and adds `ChildrenOrder.ByName` (simple name
in natural order, then the full name), which is the same in every build and on both Scala versions.

Kindlings' rule: **on Scala 3 the declaration order, on Scala 2 `Type.directChildrenInOrder(ChildrenOrder.ByName)`**,
for every module where the subtype order is observable. The shared helper is
`hearth.kindlings.derivation.compiletime.StableChildrenOrder` (derivation-commons): it reorders the children of an
`Enum` by Hearth's `ByName` order (for flattened leaves of nested sealed hierarchies: depth-first through the `ByName`
children). Java enums keep their constants' order (stored in the class file) and `scala.Enumeration` values the order
Hearth reports (see "Remaining" below). Modules using it: avro (ENUM symbols; it replaces avro's own Scala 2 sort by
full name), circe (strict-wrapper constructor names, nested-sum flattening), jsoniter (ADT leaves, enum map keys),
ubjson (ADT leaves), tapir (subtypes / enum values of schemas), yaml (untagged try-each decoding order, names in
errors), xml, pureconfig and sconfig enum readers/writers, scalacheck (`Gen.oneOf` order, shrinking, `Cogen` case
index), cats `Order` (the ordinal of a subtype). avro unions are sorted at runtime by full name anyway (as avro4s).

Upstream Scala 2 libraries (shapeless, Magnolia) sort subtypes by `fullName` (plain `String` order). That agrees with
`ByName` for the usual sealed hierarchy (subtypes in one scope, names without numbers); it differs only for numbered
names (`Item2`/`Item10`: natural order vs `String` order) and for subtypes declared in different scopes (simple name
first vs full name first). None of the oracle fixtures has such subtypes.

## Remaining

`scala.Enumeration` values on Scala 2: Hearth sorts the enumeration object's members by position, falling back to the
name, so the same build-dependence applies to them (the class file keeps the declaration order of the members, which
Hearth could use, as it does for Java enum constants). Kindlings leaves their order as Hearth reports it, since
upstream libraries use the runtime `values` order (declaration/id order) for them, which a by-name order would not
match either.
