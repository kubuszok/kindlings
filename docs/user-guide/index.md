# Kindlings

Type class derivation that compiles faster, runs faster, and works the same on Scala 2.13 and Scala 3. Drop-in replacements for derivation in Circe, Jsoniter Scala, Avro, and more — built on [Hearth](https://github.com/kubuszok/hearth), powered by macros, free of the trade-offs you've learned to accept.

## Quick start

!!! example "sbt"

    ```scala
    // derivations
    libraryDependencies += "com.kubuszok" %% "kindlings-avro-derivation" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-cats-derivation" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-circe-derivation" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-fast-show-pretty" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-jsoniter-derivation" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-pureconfig-derivation" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-scalacheck-derivation" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-sconfig-derivation" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-tapir-schema-derivation" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-ubjson-derivation" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-yaml-derivation" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-xml-derivation" % "{{ kindlings_version() }}"
    // integrations
    libraryDependencies += "com.kubuszok" %% "kindlings-cats-integration" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-iron-integration" % "{{ kindlings_version() }}"
    libraryDependencies += "com.kubuszok" %% "kindlings-refined-integration" % "{{ kindlings_version() }}"
    // extra
    libraryDependencies += "com.kubuszok" %% "kindlings-jsoniter-json" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    // derivations
    //> using dep com.kubuszok::kindlings-avro-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-cats-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-pureconfig-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-scalacheck-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-sconfig-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-ubjson-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-yaml-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    // intagrations
    //> using dep com.kubuszok::kindlings-cats-integration:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-iron-integration:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-refined-integration:{{ kindlings_version() }}
    // extra
    //> using dep com.kubuszok::kindlings-jsoniter-json:{{ kindlings_version() }}
    ```

??? example "Minimal example"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation._
    import io.circe._

    case class Person(name: String, age: Int)

    // inline encoding — no implicit needed
    val json: Json = KindlingsEncoder.encode(Person("Alice", 30))
    println(json.noSpaces) // {"name":"Alice","age":30}

    // inline decoding
    val parsed = io.circe.parser.parse("""{"name":"Bob","age":25}""")
    println(parsed.flatMap(KindlingsDecoder.decode[Person](_)))
    // Right(Person(Bob,25))
    ```


## Why Kindlings?

Most Scala libraries derive type class instances using Shapeless (Scala 2), Scala 3 Mirrors, or Magnolia. These approaches work, but they come with trade-offs that compound as your project grows: slow compilation, poor error messages, runtime overhead from intermediate representations, and API fragmentation between Scala 2 and Scala 3.

Kindlings takes a different path. Built on [Hearth](https://github.com/kubuszok/hearth), it uses macros to generate code that is closer to what you'd write by hand — while providing a better developer experience than any of the alternatives.

### One API across Scala 2.13 and Scala 3

No conditional imports, no platform-specific code. Your derivation calls look the same regardless of the Scala version. Migration between Scala 2.13 and Scala 3 requires zero changes to your derivation code.

### Recursive derivation out of the box

Kindlings handles recursive data types without lazy wrappers, manual knot-tying, or tricks. No `Lazy[_]`, no `implicit lazy val`, no special configuration — it just works.

```scala
case class Tree(value: Int, children: List[Tree])

// works with every Kindlings module — no special handling needed
```

### Sanely-automatic derivation

In most libraries, you choose between **automatic** and **semi-automatic** derivation — and each comes with trade-offs. Automatic derivation (like `import io.circe.generic.auto._`) is convenient but re-derives instances at every call site, slowing compilation and sometimes producing worse runtime code. Semi-automatic derivation avoids that but requires explicit `implicit val` / `given` boilerplate for every type.

Kindlings eliminates this choice:

- **Semi-automatic is recursive.** `KindlingsEncoder.derive[Person]` derives not just `Person` but also `Address`, `List[Address]`, and anything else reachable — no need to define instances for nested types manually.
- **Automatic imposes no overhead.** For a single derivation site, automatic and semi-automatic produce identical code — same compilation cost, same runtime performance. The generated code is as fast as what you'd write by hand.
- **Errors are informative and actionable.** When derivation fails, you get a clear message telling you exactly which type is missing an instance and where in the hierarchy the problem is.

If the same type is auto-derived at multiple call sites, each site derives independently — but this is still cheaper than Shapeless/Mirrors-based automatic derivation, because Kindlings' macro expansion is lightweight by design.

> For a deeper dive, see [Sanely-automatic derivation](https://kubuszok.com/2025/sanely-automatic-derivation/).

### Debug logging on demand

When derivation fails or you want to understand what the macro produces, enable debug logging with a single import:

```scala
import hearth.kindlings.circederivation.debug._
```

Or globally via a scalac option — no code changes needed:

```
-Xmacro-settings:circeDerivation.logDerivation=true
```

Every module supports both approaches. The output shows exactly what code the macro generates, which rules matched each field, and where summoned implicits came from.

### Flame graph generation on demand

Profile macro compilation performance with Hearth's built-in flame graph support:

```
-Xmacro-settings:hearth.mioBenchmarkScopes=true
-Xmacro-settings:hearth.mioBenchmarkFlameGraphDir=/tmp/kindlings-flamegraphs
```

Generates `.speedscope.json` files you can visualize at [speedscope.app](https://www.speedscope.app/) — useful for diagnosing slow compilation in large projects.

### Macro timeouts prevent runaway compilation

Every derivation module enforces a **5-second timeout** by default. If a macro expansion takes longer than that, compilation fails with a clear error — no more silently waiting minutes for a derivation that will never finish.

If a specific type hierarchy legitimately needs more time, you can raise the limit per module:

```
-Xmacro-settings:circeDerivation.timeout=30s
-Xmacro-settings:jsoniterDerivation.timeout=1m
```

Supported formats: plain integer (seconds), `Ns`, `Nms`, `Nm`.

### No imports for integration modules

Refined types, Iron types, and Cats collections work automatically. Just add the integration dependency to your build — no imports, no configuration. The macro extension system discovers them at compile time.

```scala
// build.sbt — just add the dependency
libraryDependencies += "com.kubuszok" %% "kindlings-refined-integration" % "{{ kindlings_version() }}"

// your code — no extra imports, refined types just work
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive

case class Order(quantity: Int Refined Positive, item: String)
// Circe/Jsoniter/Avro/... derivation handles Refined fields automatically
```

## Comparison at a glance

| | Kindlings | Shapeless / Mirrors | Magnolia | Library-specific macros |
|---|---|---|---|---|
| Same API on Scala 2.13 and 3 | Yes | No | No | varies |
| Auto derivation without overhead | Yes | No | No | varies |
| Inline derivation | Yes | No | No | some |
| Recursive types (no tricks) | Yes | needs semiauto + `lazy val` / `Lazy` | Yes | varies |
| Clear error messages | Yes | No | partial | varies |
| Code preview | Yes | No | No | rare |
| Named tuples, opaque types | Yes | No | No | rare |
| Scala 3 enums, Java enums | Yes | partial | partial | varies |

## Available modules

| Module | Replaces | Derived type classes |
|---|---|---|
| [kindlings-avro-derivation](avro-derivation.md) | avro4s (JVM only) | `AvroSchemaFor`, `AvroEncoder`, `AvroDecoder` |
| [kindlings-cats-derivation](cats-derivation.md) | kittens | `Show`, `Eq`, `Order`, `Hash`, `Functor`, `Traverse`, and [29 more](cats-derivation.md) |
| [kindlings-circe-derivation](circe-derivation.md) | circe-generic-extras | `Encoder`, `Encoder.AsObject`, `Decoder` |
| [kindlings-fast-show-pretty](fast-show-pretty.md) | _(original)_ | `FastShowPretty` |
| [kindlings-jsoniter-derivation](jsoniter-derivation.md) | jsoniter-scala `JsonCodecMaker` | `JsonValueCodec`, `JsonCodec`, `JsonKeyCodec` |
| [kindlings-pureconfig-derivation](pureconfig-derivation.md) | PureConfig generic (JVM only) | `ConfigReader`, `ConfigWriter`, `ConfigConvert` |
| [kindlings-scalacheck-derivation](scalacheck-derivation.md) | manual instances | `Arbitrary`, `Cogen`, `Shrink` |
| [kindlings-sconfig-derivation](sconfig-derivation.md) | _(original)_ | `ConfigReader`, `ConfigWriter`, `ConfigCodec` |
| [kindlings-tapir-schema-derivation](tapir-schema-derivation.md) | Tapir `Schema.derived` | `Schema` |
| [kindlings-ubjson-derivation](ubjson-derivation.md) | _(original)_ | `UBJsonValueCodec` |
| [kindlings-xml-derivation](xml-derivation.md) | _(original)_ | `XmlEncoder`, `XmlDecoder` |
| [kindlings-yaml-derivation](yaml-derivation.md) | scala-yaml `derives` | `YamlEncoder`, `YamlDecoder` |

All modules are cross-compiled for Scala 2.13 and 3, on JVM, Scala.js, and Scala Native — except `kindlings-avro-derivation` and `kindlings-pureconfig-derivation`, which are JVM-only.
