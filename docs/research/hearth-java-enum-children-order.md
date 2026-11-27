# Hearth: the order of a Java enum's children is not the declaration order in every compilation (Scala 3)

**Status:** observed in Kindlings (avro-derivation, 2026-11); not minimised to a Hearth-only reproducer yet.

## Observation

`Enum.parse[A].directChildren` for a Java enum `A` is expected to list the constants in declaration order (their
ordinals) — `StableChildrenOrder` relies on it and leaves Java enums alone.

For this enum, compiled from Java source in the same sbt module as the Scala test that derives for it
(`upstream-oracle-tests/src/test/java/hearth/kindlings/oracle/avro/AuWineWithAnnotations.java`):

```java
public enum AuWineWithAnnotations { Malbec, Shiraz, @AvroJavaEnumDefault CabSav, Merlot }
```

the Scala 3 (3.8.4) build listed the constants as `Malbec, Shiraz, CabSav, Merlot` in most compilations, and as
`CabSav, Shiraz, Merlot, Malbec` in one (a build in which the Scala sources of the module were recompiled without the
Java source changing). The Avro ENUM derived from it then had its symbols in that order — a different wire format
from the same sources. Scala 2.13 always listed them in declaration order in these runs. A plain enum in the same
directory (`AuWine`, no annotations) was not seen out of order, which may be chance.

Related, and deterministic: for a `scala.Enumeration` the children are listed by name, not by id (declaration order)
— on Scala 2.13 at least (`AuColours { val Red, Amber, Green = Value }` gives `Amber, Green, Red`).

## What Kindlings does

avro-derivation no longer depends on that order: for Java enums and `scala.Enumeration` values the generated schema
sorts the symbols at runtime by `ordinal` / `id` (`AvroDerivationUtils.symbolsInDeclarationOrder`). The regression
tests are `AvroUpstreamExtrasOracleSpec` ("java enum annotations (both Scala versions)", "scala enum symbols (both
Scala versions)") and `AvroAvro4sParitySpec` ("scala.Enumeration: symbols in the order of their ids").

Other modules that turn a Java enum's children into an ordered wire format (none known: JSON codecs match constants by
name) would be affected the same way.

## To do in Hearth

Find where the children of a Java enum come from on Scala 3 when the enum is read from a class file / from the Java
source of the same compilation (the order of `Symbol.children`, or of the declared fields, is probably not
guaranteed), and list them by ordinal; consider an id-ordered listing for `scala.Enumeration`.
