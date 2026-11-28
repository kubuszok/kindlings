# Hearth: the order of a Java enum's children is not the declaration order in every compilation (Scala 3)

**Status:** reproduced and explained - see `hearth-bug-java-enum-unreferenced-constants.md` (minimal reproducer,
cause, Kindlings workaround). Kept for the original observation.

## Observation (avro-derivation, 2026-11)

For this enum, compiled from Java source in the same sbt module as the Scala test that derives for it
(`upstream-oracle-tests/src/test/java/hearth/kindlings/oracle/avro/AuWineWithAnnotations.java`):

```java
public enum AuWineWithAnnotations { Malbec, Shiraz, @AvroJavaEnumDefault CabSav, Merlot }
```

the Scala 3 (3.8.4) build listed the constants as `Malbec, Shiraz, CabSav, Merlot` in most compilations, and as
`CabSav, Shiraz, Merlot, Malbec` in one (a build in which the Scala sources of the module were recompiled without the
Java source changing). Scala 2.13 always listed them in declaration order.

## Explanation

When the Scala sources are recompiled without the Java source, the enum is read from javac's class file, and dotc
registers the constants of a class-file Java enum as its children in the order in which it completes them - the
constants referred to by the compiled code (here the tests' `CabSav`, `Shiraz`, `Merlot`, `Malbec` values) first. With
the Java source in the same compilation the children are sorted by position (declaration order). A JDK enum shows it
deterministically:

```scala
val referred = java.time.format.TextStyle.SHORT_STANDALONE
EnumChildren.of[java.time.format.TextStyle]
// SHORT_STANDALONE, FULL, FULL_STANDALONE, SHORT, NARROW, NARROW_STANDALONE
```

## What Kindlings does

- `StableChildrenOrder.inStableOrder` (`derivation-commons`) puts the children of a Java enum in the declaration order
  of its constants (the order of the static fields in the class file), for every module using it (cats `Order`, tapir
  enumerations, scalacheck generators, pureconfig/sconfig/yaml/jsoniter/ubjson/xml/circe sealed handling, ...). Test:
  the cats `Order` of `java.time.format.TextStyle` with `SHORT_STANDALONE` referred to
  (`cats-derivation/src/test/scalajvm/.../UnreferencedJavaEnumJvmSpec.scala`).
- avro-derivation also sorts the symbols at runtime by `ordinal` / `id` (`AvroDerivationUtils.symbolsInDeclarationOrder`).

Related, and deterministic: for a `scala.Enumeration` the children are listed by name, not by id (declaration order)
- on Scala 2.13 at least (`AuColours { val Red, Amber, Green = Value }` gives `Amber, Green, Red`); avro-derivation sorts
those by `id` at runtime too.
