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

    import hearth.kindlings.circederivation.*
    import io.circe.*
    import io.circe.syntax.*

    case class Person(name: String, age: Int)

    // Sanely-automatic: KindlingsEncoder.derived[Person] is resolved by the compiler
    val json: Json = Person("Alice", 30).asJson
    println(json.noSpaces)
    // {"name":"Alice","age":30}

    // Semi-automatic:
    val decoder: Decoder[Person] = KindlingsDecoder.derive[Person]
    println(io.circe.parser.decode[Person]("""{"name":"Bob","age":25}""")(decoder))
    // Right(Person(Bob,25))
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsEncoder.derive[A]` | `Encoder[A]` | Semi-automatic encoder |
| `KindlingsEncoder.deriveAsObject[A]` | `Encoder.AsObject[A]` | Semi-automatic object encoder |
| `KindlingsEncoder.encode[A](value)` | `Json` | Inline encoding (no instance allocation) |
| `KindlingsEncoder.derived[A]` | `KindlingsEncoder[A]` | Sanely-automatic (given/implicit) |
| `KindlingsDecoder.derive[A]` | `Decoder[A]` | Semi-automatic decoder |
| `KindlingsDecoder.decode[A](json)` | `Either[DecodingFailure, A]` | Inline decoding |
| `KindlingsDecoder.derived[A]` | `KindlingsDecoder[A]` | Sanely-automatic (given/implicit) |
| `KindlingsCodecAsObject.derive[A]` | `Codec.AsObject[A]` | Semi-automatic codec |
| `KindlingsCodecAsObject.derived[A]` | `KindlingsCodecAsObject[A]` | Sanely-automatic codec |

All methods take an implicit/using `Configuration` parameter (defaults to `Configuration.default`).

### Type hierarchy

`KindlingsEncoder[A]` extends `Encoder[A]` and `KindlingsDecoder[A]` extends `Decoder[A]`, so derived instances work anywhere the original Circe types are expected.

## Configuration

All derivation methods accept an implicit `Configuration`:

```scala
import hearth.kindlings.circederivation.*

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
import hearth.kindlings.circederivation.annotations.*

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

    import hearth.kindlings.circederivation.*
    import io.circe.*
    import io.circe.syntax.*

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val config: Configuration = Configuration.default
      .withDiscriminator("type")
      .withSnakeCaseConstructorNames

    val shape: Shape = Circle(5.0)
    println(shape.asJson.noSpaces)
    // {"radius":5.0,"type":"circle"}

    val decoded = io.circe.parser.decode[Shape]("""{"width":3,"height":4,"type":"rectangle"}""")
    println(decoded)
    // Right(Rectangle(3.0,4.0))
    ```

??? example "Recursive data types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation.*
    import io.circe.syntax.*

    case class Tree(value: String, children: List[Tree])

    val tree = Tree("root", List(
      Tree("left", Nil),
      Tree("right", List(Tree("leaf", Nil)))
    ))
    println(tree.asJson.noSpaces)
    // {"value":"root","children":[{"value":"left","children":[]},{"value":"right","children":[{"value":"leaf","children":[]}]}]}
    ```

??? example "Case class with defaults"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation.*
    import io.circe.*

    implicit val config: Configuration = Configuration.default.withDefaults

    case class Settings(host: String, port: Int = 8080, debug: Boolean = false)

    val decoder = KindlingsDecoder.derive[Settings]
    println(io.circe.parser.decode[Settings]("""{"host":"localhost"}""")(decoder))
    // Right(Settings(localhost,8080,false))
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
| SimpleCC | 2.13 | 30.6M | 31.1M | 19.3M | 19.0M | **1.6x faster** |
| SimpleCC | 3 | 30.9M | 30.7M | 21.8M | 21.3M | **1.4x faster** |
| SimpleADT | 2.13 | 27.2M | 27.9M | 14.1M | 14.0M | **2.0x faster** |
| SimpleADT | 3 | 26.1M | 26.5M | 25.7M | 25.4M | ~tied |
| Person | 2.13 | 4.5M | 4.5M | 3.0M | 3.0M | **1.5x faster** |
| Person | 3 | 4.3M | 4.3M | 3.1M | 3.1M | **1.4x faster** |
| Event | 2.13 | 3.4M | 3.4M | 2.3M | 2.4M | **1.4x faster** |
| Event | 3 | 3.3M | 3.4M | 2.3M | 2.3M | **1.4x faster** |

#### Decode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 51.0M | 50.9M | 42.4M | 42.8M | **1.2x faster** |
| SimpleCC | 3 | 52.7M | 48.9M | 20.0M | 20.6M | **2.6x faster** |
| SimpleADT | 2.13 | 61.3M | 61.0M | 25.0M | 26.5M | **2.3x faster** |
| SimpleADT | 3 | 47.7M | 46.5M | 29.2M | 27.8M | **1.6x faster** |
| Person | 2.13 | 4.2M | 4.2M | 3.5M | 3.5M | **1.2x faster** |
| Person | 3 | 4.2M | 4.2M | 2.7M | 2.7M | **1.6x faster** |
| Event | 2.13 | 2.8M | 2.8M | 2.7M | 2.7M | ~tied |
| Event | 3 | 2.9M | 2.9M | 2.1M | 2.1M | **1.4x faster** |

### End-to-end with jsoniter-scala-circe booster

[jsoniter-scala-circe](https://github.com/plokhotnyuk/jsoniter-scala/tree/master/jsoniter-scala-circe) is a Circe booster that replaces the default parser/printer with jsoniter-scala's faster implementation. The table below benchmarks the full pipeline (domain type to bytes/String).

The booster is an optional add-on — Kindlings works with standard Circe parsing out of the box. When combined with the booster, Kindlings + jsoniter-scala-circe is the fastest way to use Circe.

#### Encode (domain type to bytes/String)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **14.8M** | 10.2M | 6.8M | 5.5M |
| SimpleCC | 3 | **15.3M** | 11.9M | 7.2M | 6.6M |
| SimpleADT | 2.13 | **15.2M** | 8.0M | 7.8M | 5.8M |
| SimpleADT | 3 | **15.6M** | 12.1M | 8.1M | 6.9M |
| Person | 2.13 | **1.6M** | 1.4M | 979.1K | 906.8K |
| Person | 3 | **1.6M** | 1.4M | 1.1M | 959.5K |
| Event | 2.13 | **1.3M** | 1.1M | 841.8K | 731.6K |
| Event | 3 | **1.3M** | 1.1M | 918.8K | 800.3K |

#### Decode (bytes/String to domain type)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **8.3M** | 7.8M | 6.0M | 6.2M |
| SimpleCC | 3 | **8.0M** | 6.6M | 6.9M | 5.7M |
| SimpleADT | 2.13 | **10.6M** | 9.0M | 9.3M | 7.6M |
| SimpleADT | 3 | **10.4M** | 9.1M | 9.6M | 7.9M |
| Person | 2.13 | **1.1M** | 1.1M | 925.0K | 883.8K |
| Person | 3 | **1.2M** | 985.2K | 974.8K | 850.6K |
| Event | 2.13 | 929.5K | 932.5K | 672.8K | 689.6K |
| Event | 3 | **931.7K** | 826.3K | 783.3K | 719.1K |

Note: Kindlings semi-automatic and automatic derivation produce identical performance — this is the "sanely-automatic" design.
