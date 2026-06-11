# Circe Derivation

Drop-in replacement for `circe-generic` / `circe-generic-extras` — derives `Encoder`, `Encoder.AsObject`, and `Decoder` for case classes, sealed traits, Scala 3 enums, Java enums, and more.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-circe-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-circe-derivation" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    ```

## Quick start

??? example "Encoding and decoding a case class"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation._
    import io.circe._

    case class Person(name: String, age: Int)

    // Inline encoding — no implicit needed
    val json: Json = KindlingsEncoder.encode(Person("Alice", 30))
    println(json.noSpaces)
    // expected output:
    // {"name":"Alice","age":30}

    // Inline decoding
    val parsed = io.circe.parser.parse("""{"name":"Bob","age":25}""")
    println(parsed.flatMap(KindlingsDecoder.decode[Person](_)))
    // expected output:
    // Right(Person(Bob,25))
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsEncoder.derived[A]` | `KindlingsEncoder[A]` | Sanely-automatic encoder (given/implicit, also usable as semi-automatic) |
| `KindlingsEncoder.deriveAsObject[A]` | `Encoder.AsObject[A]` | Object encoder |
| `KindlingsEncoder.encode[A](value)` | `Json` | Inline encoding (no instance allocation) |
| `KindlingsDecoder.derived[A]` | `KindlingsDecoder[A]` | Sanely-automatic decoder (given/implicit, also usable as semi-automatic) |
| `KindlingsDecoder.decode[A](json)` | `Either[DecodingFailure, A]` | Inline decoding |
| `KindlingsCodecAsObject.derived[A]` | `KindlingsCodecAsObject[A]` | Sanely-automatic codec (given/implicit, also usable as semi-automatic) |

All methods take an implicit/using `Configuration` parameter (defaults to `Configuration.default`).

### Type hierarchy

`KindlingsEncoder[A]` extends `Encoder[A]` and `KindlingsDecoder[A]` extends `Decoder[A]`, so derived instances work anywhere the original Circe types are expected.

## Configuration

All derivation methods accept an implicit `Configuration`:

```scala
import hearth.kindlings.circederivation._

implicit val config: Configuration = Configuration.default
  .withSnakeCaseMemberNames
  .withDiscriminator("type")
  .withDefaults
```

| Builder method | Description |
|---------------|-------------|
| `withSnakeCaseMemberNames` | `fieldName` → `field_name` |
| `withKebabCaseMemberNames` | `fieldName` → `field-name` |
| `withPascalCaseMemberNames` | `fieldName` → `FieldName` |
| `withScreamingSnakeCaseMemberNames` | `fieldName` → `FIELD_NAME` |
| `withTransformMemberNames(f)` | Custom field name transform |
| `withSnakeCaseConstructorNames` | `MyType` → `my_type` in discriminator |
| `withKebabCaseConstructorNames` | `MyType` → `my-type` in discriminator |
| `withPascalCaseConstructorNames` | `MyType` → `MyType` in discriminator |
| `withScreamingSnakeCaseConstructorNames` | `MyType` → `MY_TYPE` in discriminator |
| `withTransformConstructorNames(f)` | Custom constructor name transform |
| `withDefaults` | Use case class default values for missing fields |
| `withoutDefaults` | Require all fields (default) |
| `withDiscriminator(field)` | ADT discriminator field name |
| `withoutDiscriminator` | No discriminator (default — wrapping object) |
| `withStrictDecoding` | Fail on unexpected JSON fields |
| `withoutStrictDecoding` | Ignore unexpected fields (default) |
| `withEnumAsStrings` | Encode Scala 3 / Java enums as strings |

## Annotations

| Annotation | Description |
|-----------|-------------|
| `@fieldName("json_name")` | Override JSON field name for a case class field |
| `@transientField` | Exclude a field from encoding/decoding (must have a default value) |

```scala
import hearth.kindlings.circederivation.annotations._

case class User(
  @fieldName("user_name") name: String,
  @transientField internalId: Long = 0L
)
```

## Usage examples

??? example "Sealed trait with discriminator"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation._
    import io.circe._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val config: Configuration = Configuration.default
      .withDiscriminator("type")
      .withSnakeCaseConstructorNames

    val shape: Shape = Circle(5.0)
    println(KindlingsEncoder.encode(shape).noSpaces)
    // expected output:
    // {"type":"circle","radius":5.0}

    val decoded = io.circe.parser.parse("""{"width":3,"height":4,"type":"rectangle"}""")
      .flatMap(KindlingsDecoder.decode[Shape](_))
    println(decoded)
    // expected output:
    // Right(Rectangle(3.0,4.0))
    ```

??? example "Recursive data types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}

    import hearth.kindlings.circederivation._
    import io.circe._

    case class Tree(value: String, children: List[Tree])

    val tree = Tree("root", List(
      Tree("left", Nil),
      Tree("right", List(Tree("leaf", Nil)))
    ))
    println(KindlingsEncoder.encode(tree).noSpaces)
    // expected output:
    // {"value":"root","children":[{"value":"left","children":[]},{"value":"right","children":[{"value":"leaf","children":[]}]}]}
    ```

??? example "Case class with defaults"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation._
    import io.circe._

    implicit val config: Configuration = Configuration.default.withDefaults

    case class Settings(host: String, port: Int = 8080, debug: Boolean = false)

    val parsed = io.circe.parser.parse("""{"host":"localhost"}""")
    println(parsed.flatMap(KindlingsDecoder.decode[Settings](_)))
    // expected output:
    // Right(Settings(localhost,8080,false))
    ```

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.circederivation.debug._
```

Or enable project-wide via scalac option:

```scala
// build.sbt
scalacOptions += "-Xmacro-settings:circeDerivation.logDerivation=true"
```

## Comparison with circe-generic

### Feature differences

| Feature | circe-generic | Kindlings |
|---------|--------------|-----------|
| Same API on Scala 2.13 and 3 | No (different modules, different APIs) | Yes |
| Automatic derivation without overhead | No (re-derives at every use site) | Yes (sanely-automatic) |
| Inline encoding/decoding | No | Yes (`encode[A]`, `decode[A]`) |
| Recursive types | Needs `Lazy` / workarounds | Just works |
| Named tuples | No | Yes |
| Opaque types | No | Yes |
| Scala 3 enums | Partial | Yes |
| Java enums | No | Yes |
| `@ConfiguredJsonCodec` annotation | Yes | No (use `Configuration` directly) |

### Benchmarks

All values in ops/s (higher is better). Measured on macOS, JVM temurin 17.

#### Encode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 30.8M | 30.9M | 20.9M | 20.9M | **1.5x faster** |
| SimpleCC | 3 | 33.8M | 38.5M | 17.6M | 17.6M | **2.2x faster** |
| SimpleADT | 2.13 | 32.1M | 28.7M | 14.8M | 18.4M | **1.7x faster** |
| SimpleADT | 3 | 31.3M | 31.2M | 25.5M | 25.6M | **1.2x faster** |
| Person | 2.13 | 3.8M | 3.7M | 2.6M | 2.9M | **1.3x faster** |
| Person | 3 | 3.8M | 3.4M | 2.4M | 2.1M | **1.6x faster** |
| Event | 2.13 | 3.1M | 3.1M | 2.3M | 2.4M | **1.3x faster** |
| Event | 3 | 3.1M | 3.1M | 2.0M | 2.0M | **1.5x faster** |

#### Decode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 46.1M | 46.3M | 38.5M | 38.8M | **1.2x faster** |
| SimpleCC | 3 | 40.6M | 41.3M | 18.3M | 17.9M | **2.3x faster** |
| SimpleADT | 2.13 | 38.8M | 39.1M | 30.2M | 30.2M | **1.3x faster** |
| SimpleADT | 3 | 39.5M | 41.6M | 25.7M | 25.1M | **1.6x faster** |
| Person | 2.13 | 3.1M | 3.2M | 2.3M | 2.5M | **1.3x faster** |
| Person | 3 | 3.1M | 3.1M | 2.4M | 2.5M | **1.3x faster** |
| Event | 2.13 | 2.5M | 2.5M | 2.2M | 2.1M | **1.2x faster** |
| Event | 3 | 2.5M | 2.6M | 2.0M | 1.9M | **1.3x faster** |

### End-to-end with jsoniter-scala-circe booster

[jsoniter-scala-circe](https://github.com/plokhotnyuk/jsoniter-scala/tree/master/jsoniter-scala-circe) is a Circe booster that replaces the default parser/printer with jsoniter-scala's faster implementation. The table below benchmarks the full pipeline (domain type to bytes/String).

The booster is an optional add-on — Kindlings works with standard Circe parsing out of the box. When combined with the booster, Kindlings + jsoniter-scala-circe is the fastest way to use Circe.

#### Encode (domain type to bytes/String)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **8.3M** | 7.3M | 7.2M | 6.7M |
| SimpleCC | 3 | **12.1M** | 7.6M | 8.0M | 6.6M |
| SimpleADT | 2.13 | **13.2M** | 7.1M | 7.6M | 6.4M |
| SimpleADT | 3 | **9.7M** | 8.8M | 7.3M | 7.4M |
| Person | 2.13 | **1.5M** | 1.4M | 922.5K | 845.8K |
| Person | 3 | **1.6M** | 1.4M | 1.0M | 917.5K |
| Event | 2.13 | **1.3M** | 1.1M | 795.0K | 714.9K |
| Event | 3 | **1.3M** | 1.1M | 872.2K | 768.4K |

#### Decode (bytes/String to domain type)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **8.0M** | 6.8M | 7.5M | 6.8M |
| SimpleCC | 3 | **9.6M** | 6.2M | 7.3M | 5.1M |
| SimpleADT | 2.13 | **9.0M** | 8.8M | 7.2M | 7.2M |
| SimpleADT | 3 | **10.7M** | 8.9M | 9.3M | 7.3M |
| Person | 2.13 | **957.0K** | 799.3K | 695.8K | 521.2K |
| Person | 3 | **1.1M** | 1.0M | 747.5K | 704.6K |
| Event | 2.13 | **845.5K** | 717.6K | 598.3K | 576.1K |
| Event | 3 | **992.4K** | 787.3K | 683.3K | 627.8K |

Note: Kindlings semi-automatic and automatic derivation produce identical performance — this is the "sanely-automatic" design.
