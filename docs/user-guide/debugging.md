# Debugging & Diagnostics

Kindlings provides three diagnostic tools: debug logging (to see what the macro generates), flame graphs (to profile compilation performance), and configurable timeouts (to prevent runaway derivation).

## Debug logging

Every module supports two ways to enable debug logging: a scoped import (per-file) or a scalac option (project-wide).

### Via import (per-file)

Add the module's debug import to the file where you want to inspect derivation:

```scala
// Circe
import hearth.kindlings.circederivation.debug._

// Jsoniter
import hearth.kindlings.jsoniterderivation.debug._

// Cats
import hearth.kindlings.catsderivation.debug._
```

The import places a `LogDerivation` implicit in scope. When the macro sees it, it prints the generated code, matched rules, and summoned implicits to the compiler output.

### Via scalac option (project-wide)

Add a `-Xmacro-settings` option to your build — no code changes needed:

```scala
// build.sbt
scalacOptions += "-Xmacro-settings:circeDerivation.logDerivation=true"
```

### Module reference

| Module | Debug import | Scalac setting |
|--------|-------------|----------------|
| circe-derivation | `hearth.kindlings.circederivation.debug._` | `circeDerivation.logDerivation=true` |
| jsoniter-derivation | `hearth.kindlings.jsoniterderivation.debug._` | `jsoniterDerivation.logDerivation=true` |
| cats-derivation | `hearth.kindlings.catsderivation.debug._` | `catsDerivation.logDerivation=true` |
| avro-derivation | `hearth.kindlings.avroderivation.debug._` | `avroDerivation.logDerivation=true` |
| pureconfig-derivation | `hearth.kindlings.pureconfigderivation.debug._` | `pureconfigDerivation.logDerivation=true` |
| tapir-schema-derivation | `hearth.kindlings.tapirschemaderivation.debug._` | `tapirSchemaDerivation.logDerivation=true` |
| yaml-derivation | `hearth.kindlings.yamlderivation.debug._` | `yamlDerivation.logDerivation=true` |
| scalacheck-derivation | `hearth.kindlings.scalacheckderivation.debug._` | `scalacheckDerivation.logDerivation=true` |
| sconfig-derivation | `hearth.kindlings.sconfigderivation.debug._` | `sconfigDerivation.logDerivation=true` |
| ubjson-derivation | `hearth.kindlings.ubjsonderivation.debug._` | `ubjsonDerivation.logDerivation=true` |
| xml-derivation | `hearth.kindlings.xmlderivation.debug._` | `xmlDerivation.logDerivation=true` |
| fast-show-pretty | `hearth.kindlings.fastshowpretty.debug._` | `fastShowPretty.logDerivation=true` |

## Flame graph generation

Profile how long each derivation step takes using Hearth's built-in flame graph support. Add these scalac options:

```scala
// build.sbt
scalacOptions ++= Seq(
  "-Xmacro-settings:hearth.mioBenchmarkScopes=true",
  "-Xmacro-settings:hearth.mioBenchmarkFlameGraphDir=/tmp/kindlings-flamegraphs"
)
```

This generates `.speedscope.json` files in the specified directory — one per macro expansion. Open them at [speedscope.app](https://www.speedscope.app/) to visualize where compilation time is spent.

!!! tip
    This is most useful when a specific type takes noticeably long to derive. Focus on the hot paths: rule evaluation (type comparisons), implicit search, and expression tree construction.

## Derivation timeout

All modules enforce a configurable timeout to prevent runaway macro expansions. Default: **5 seconds**. If a derivation takes longer, compilation fails with a clear error — preventing silent long waits.

Override per module via `-Xmacro-settings` when a specific type hierarchy legitimately needs more time:

```scala
// build.sbt
scalacOptions ++= Seq(
  "-Xmacro-settings:circeDerivation.timeout=30s",
  "-Xmacro-settings:jsoniterDerivation.timeout=1m"
)
```

Supported formats:

| Format | Example | Meaning |
|--------|---------|---------|
| Plain integer | `300` | 300 seconds |
| Seconds | `300s` or `300seconds` | 300 seconds |
| Milliseconds | `5000ms` or `5000milliseconds` | 5 seconds |
| Minutes | `5m` or `5minutes` | 5 minutes |

Invalid values emit a compiler warning and fall back to the 120-second default.

### Settings namespace per module

| Module | Settings namespace |
|--------|-------------------|
| circe-derivation | `circeDerivation` |
| jsoniter-derivation | `jsoniterDerivation` |
| cats-derivation | `catsDerivation` |
| avro-derivation | `avroDerivation` |
| pureconfig-derivation | `pureconfigDerivation` |
| tapir-schema-derivation | `tapirSchemaDerivation` |
| yaml-derivation | `yamlDerivation` |
| scalacheck-derivation | `scalacheckDerivation` |
| sconfig-derivation | `sconfigDerivation` |
| ubjson-derivation | `ubjsonDerivation` |
| xml-derivation | `xmlDerivation` |
| fast-show-pretty | `fastShowPretty` |
