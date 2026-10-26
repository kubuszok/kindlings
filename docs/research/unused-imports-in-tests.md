# Unused-import linting in test sources (2026-10-25)

Main sources keep `-Wunused:imports` / `-Ywarn-unused:imports` off (see `build.sbt`). Test sources now have it on
and fatal. Before enabling it, every report was checked by removing the flagged import (or selector) and recompiling
and testing the module on 2.13 and 3 (JVM).

## Results

19 reports across all JVM test sources:

| Import | Flagged on | Removed → | Verdict |
|---|---|---|---|
| `cats.data.Tuple2K`, `cats.tagless.aop.Instrumentation` (cats-tagless `TaglessKTraitSpec`) | 3 | 43/43 pass | unused, removed |
| `org.virtuslab.yaml.{Node, Tag}`: `Tag` (yaml `YamlScala3Spec`) | 3 | 273/273 pass | unused, removed (`Node` is used) |
| `scala.collection.immutable.{SortedMap, SortedSet}`: `SortedSet` (scalacheck `CatsContainersSpec`) | 2.13 + 3 | 87/87, 90/90 pass | unused, removed |
| `iron.constraint.string.*` ×6, `iron.constraint.any.*` (integration-tests Iron*) | 3 | 222/222 pass | unused, removed |
| `hearth.kindlings.optics.*` (optics `BareImportSpec`) | 2.13 | 82/82 pass | redundant: the spec was **inside** package `hearth.kindlings.optics`, so it never tested the bare import it is named after. Moved to package `hearth.kindlings.opticsbareimport`, where the import is needed and not flagged |
| `hearth.kindlings.{dicats,mock,optics}.debug.*` (`GenerationLoggingSpec` ×3) | 3 | tests pass, **but the generation log disappears** (the same compile prints "Mock.mock — generation log" with the import and 0 log lines without it) | **false positive**: the `LogGeneration` marker is summoned only by the macro |
| `AvroOpaqueTypes.*` (avro `AvroScala3Spec`) | 3 | 3 tests fail at runtime ("Not a valid schema field: make", wrong UNION schema) | **false positive**: givens summoned only by the macro |
| `iron.constraint.collection.{given, *}`: `*` (`IronParserSpec`) | 3 | compile error: `MinLength` not found (`type AtLeastTwo = List[Int] :| MinLength[2]`) | **false positive**: wildcard used only in a type alias |
| `io.estatico.newtype.macros.{newsubtype, newtype}` (`newtypeExamples`) | 3 | 2.13 compile error: "not found: type newtype" | **false positive** on Scala 3 (macro-annotation import in a shared source) |

The four false-positive files are suppressed per source (`-Wconf:src=…&msg=unused import:s`, Scala 3 only) in
`build.sbt`. Add a file there only with the same kind of evidence.

## What this says about the main-source exclusion

- Scala 3's unused-import check does not see uses that only exist inside a macro expansion (implicits summoned by the
  macro) or only in a type alias. That is the same class of false positive that motivated turning the lint off for
  main sources.
- The 2.13 comment in `build.sbt` cites `BareImportSpec` (hearth#320) as the false positive. That spec was mis-placed
  and its import really was redundant. Moved out of the package, the bare import is not flagged on 2.13, so the
  comment's evidence no longer holds.
- Main sources were not re-checked here.

## sbt 2 gotchas met while doing this

- `clean` restores compiler outputs from the disk cache and does not replay warnings, so a lint run over cached
  modules prints nothing. A throwaway `-Wconf:msg=<unique-string>:s` option changes the cache key and forces a real
  recompile.
- After a cache restore, javac-compiled test classes (`src/test/java/**/JavaColor.java`) are symlinks into the
  read-only CAS, and recompiling fails with "error while writing …JavaColor.class". The failure is then cached
  (`CachedCompileFailure`). Fix: delete that module's `test-classes` dir and change the cache key.
