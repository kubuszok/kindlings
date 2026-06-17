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
| SimpleCC | 2.13 | 30.3M | 30.9M | 18.8M | 19.0M | **1.63x faster** |
| SimpleCC | 3 | 31.2M | 31.2M | 21.8M | 20.9M | **1.43x faster** |
| SimpleADT | 2.13 | 27.5M | 27.1M | 13.4M | 13.9M | **1.98x faster** |
| SimpleADT | 3 | 26.8M | 25.7M | 26.6M | 27.1M | **0.99x faster** |
| Person | 2.13 | 4.5M | 4.5M | 3.0M | 3.1M | **1.45x faster** |
| Person | 3 | 4.4M | 4.5M | 3.1M | 3.2M | **1.41x faster** |
| Event | 2.13 | 3.4M | 3.4M | 2.3M | 2.4M | **1.42x faster** |
| Event | 3 | 3.3M | 3.4M | 2.4M | 2.3M | **1.42x faster** |

#### Decode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 88.3M | 93.2M | 42.0M | 42.6M | **2.19x faster** |
| SimpleCC | 3 | 91.9M | 92.1M | 20.5M | 21.2M | **4.34x faster** |
| SimpleADT | 2.13 | 56.3M | 55.9M | 25.0M | 25.7M | **2.19x faster** |
| SimpleADT | 3 | 58.3M | 54.6M | 27.9M | 28.0M | **2.08x faster** |
| Person | 2.13 | 5.4M | 5.3M | 3.5M | 3.6M | **1.50x faster** |
| Person | 3 | 5.5M | 5.4M | 2.7M | 2.6M | **2.04x faster** |
| Event | 2.13 | 3.3M | 3.5M | 2.7M | 2.7M | **1.30x faster** |
| Event | 3 | 3.5M | 3.3M | 2.1M | 2.2M | **1.59x faster** |

### End-to-end with jsoniter-scala-circe booster

[jsoniter-scala-circe](https://github.com/plokhotnyuk/jsoniter-scala/tree/master/jsoniter-scala-circe) is a Circe booster that replaces the default parser/printer with jsoniter-scala's faster implementation. The table below benchmarks the full pipeline (domain type to bytes/String).

The booster is an optional add-on — Kindlings works with standard Circe parsing out of the box. When combined with the booster, Kindlings + jsoniter-scala-circe is the fastest way to use Circe.

#### Encode (domain type to bytes/String)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **13.9M** | 10.5M | 6.8M | 5.4M |
| SimpleCC | 3 | **15.5M** | 12.0M | 7.2M | 6.7M |
| SimpleADT | 2.13 | **14.3M** | 8.1M | 7.8M | 5.9M |
| SimpleADT | 3 | **15.6M** | 11.7M | 8.1M | 6.9M |
| Person | 2.13 | **1.6M** | 1.4M | 985K | 882K |
| Person | 3 | **1.7M** | 1.5M | 1.1M | 964K |
| Event | 2.13 | **1.3M** | 1.1M | 831K | 764K |
| Event | 3 | **1.4M** | 1.2M | 939K | 805K |

#### Decode (bytes/String to domain type)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **9.3M** | 8.1M | 6.1M | 5.9M |
| SimpleCC | 3 | **8.8M** | 6.6M | 7.1M | 5.9M |
| SimpleADT | 2.13 | **11.2M** | 9.1M | 8.7M | 7.4M |
| SimpleADT | 3 | **10.9M** | 9.2M | 9.8M | 8.5M |
| Person | 2.13 | **1.3M** | 1.1M | 918K | 879K |
| Person | 3 | **1.3M** | 1.0M | 1.1M | 874K |
| Event | 2.13 | **1.0M** | 906K | 736K | 724K |
| Event | 3 | **996K** | 825K | 836K | 703K |

Note: Kindlings semi-automatic and automatic derivation produce identical performance — this is the "sanely-automatic" design.
