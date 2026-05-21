# Jsoniter Scala Derivation

Drop-in replacement for `jsoniter-scala-macros` `JsonCodecMaker` — derives `JsonValueCodec`, `JsonCodec`, and `JsonKeyCodec` for case classes, sealed traits, Scala 3 enums, Java enums, and more.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-jsoniter-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-jsoniter-derivation" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    ```

!!! note
    You also need `jsoniter-scala-core` as a runtime dependency:

    ```scala
    libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "{{ libraries.jsoniterScala }}"
    ```

## Quick start

??? example "Encoding and decoding with JsonValueCodec"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterderivation._
    import hearth.kindlings.fastshowpretty._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    case class Person(name: String, age: Int)

    implicit val codec: JsonValueCodec[Person] = KindlingsJsonValueCodec.derive[Person]

    val bytes = writeToArray(Person("Alice", 30))
    println(new String(bytes))
    // expected output:
    // {"name":"Alice","age":30}

    val person = readFromArray[Person]("""{"name":"Bob","age":25}""".getBytes)
    println(FastShowPretty.render(person, RenderConfig.Default))
    // expected output:
    // Person(
    //   name = "Bob",
    //   age = 25
    // )
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsJsonValueCodec.derive[A]` | `JsonValueCodec[A]` | Semi-automatic codec |
| `KindlingsJsonValueCodec.derived[A]` | `KindlingsJsonValueCodec[A]` | Sanely-automatic (given/implicit) |
| `KindlingsJsonValueCodec.writeToString[A](value)` | `String` | Inline encoding |
| `KindlingsJsonValueCodec.readFromString[A](json)` | `Either[JsonReaderException, A]` | Inline decoding |
| `KindlingsJsonCodec.derive[A]` | `JsonCodec[A]` | Combined value + key codec |
| `KindlingsJsonCodec.deriveKeyCodec[A]` | `JsonKeyCodec[A]` | Key codec (for map keys) |
| `KindlingsJsonCodec.derived[A]` | `KindlingsJsonCodec[A]` | Sanely-automatic |

All methods take an implicit/using `JsoniterConfig` parameter (defaults to `JsoniterConfig.default`).

## Configuration

```scala
import hearth.kindlings.jsoniterderivation._

implicit val config: JsoniterConfig = JsoniterConfig.default
  .withSnakeCaseFieldNames
  .withDiscriminator("type")
  .withSkipUnexpectedFields(true)
```

| Builder method | Description |
|---------------|-------------|
| `withSnakeCaseFieldNames` | `fieldName` → `field_name` |
| `withKebabCaseFieldNames` | `fieldName` → `field-name` |
| `withPascalCaseFieldNames` | `fieldName` → `FieldName` |
| `withScreamingSnakeCaseFieldNames` | `fieldName` → `FIELD_NAME` |
| `withFieldNameMapper(f)` | Custom field name transform |
| `withSnakeCaseAdtLeafClassNames` | ADT subtype name → `snake_case` |
| `withKebabCaseAdtLeafClassNames` | ADT subtype name → `kebab-case` |
| `withAdtLeafClassNameMapper(f)` | Custom ADT subtype name transform |
| `withDiscriminator(field)` | ADT discriminator field name |
| `withSkipUnexpectedFields(skip)` | Skip unexpected JSON fields |
| `withEnumAsStrings` | Encode enums as strings |
| `withMapAsArray` | Encode maps as arrays of key-value pairs |
| `withStringified` | Encode numbers/booleans as strings |
| `withDecodingOnly` | Derive only decoder |
| `withEncodingOnly` | Derive only encoder |
| `withCirceLikeObjectEncoding` | Encode ADTs as `{"SubType": {...}}` |
| `withTransientDefault` | Skip fields with default values during encoding |
| `withTransientEmpty` | Skip empty collections during encoding |
| `withTransientNone` | Skip `None` fields during encoding |
| `withRequireCollectionFields` | Fail if collection field is missing (instead of empty default) |
| `withRequireDefaultFields` | Fail if field with default is missing |
| `withCheckFieldDuplication` | Fail on duplicate JSON field names |
| `withBigDecimalPrecision(n)` | Max BigDecimal precision |
| `withBigDecimalScaleLimit(n)` | Max BigDecimal scale |
| `withBigDecimalDigitsLimit(n)` | Max BigDecimal digits |
| `withMapMaxInsertNumber(n)` | Max map entries |
| `withSetMaxInsertNumber(n)` | Max set entries |
| `withUseScalaEnumValueId` | Use enum value id for Scala enumerations |

## Annotations

| Annotation | Description |
|-----------|-------------|
| `@fieldName("json_name")` | Override JSON field name |
| `@transientField` | Exclude field from codec (must have default) |
| `@stringified` | Encode this field as a string |

## Usage examples

??? example "Sealed trait with discriminator"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterderivation._
    import hearth.kindlings.fastshowpretty._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val config: JsoniterConfig = JsoniterConfig.default
      .withDiscriminator("type")

    implicit val codec: JsonValueCodec[Shape] = KindlingsJsonValueCodec.derive[Shape]

    println(writeToString[Shape](Circle(5.0)))
    // expected output:
    // {"type":"Circle","radius":5.0}

    val decoded: Shape = readFromString[Shape]("""{"type":"Rectangle","width":3,"height":4}""")
    println(FastShowPretty.render(decoded, RenderConfig.Default))
    // expected output:
    // (Rectangle(
    //     width = 3.0d,
    //     height = 4.0d
    //   )): Shape
    ```

??? example "Transient fields and defaults"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterderivation._
    import hearth.kindlings.fastshowpretty._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    implicit val config: JsoniterConfig = JsoniterConfig.default
      .withTransientDefault
      .withTransientNone

    case class Settings(
      host: String,
      port: Int = 8080,
      debug: Option[Boolean] = None
    )

    implicit val codec: JsonValueCodec[Settings] = KindlingsJsonValueCodec.derive[Settings]

    // Default and None fields are omitted
    println(writeToString(Settings("localhost")))
    // expected output:
    // {"host":"localhost"}

    // Missing fields use defaults
    println(FastShowPretty.render(readFromString[Settings]("""{"host":"example.com"}"""), RenderConfig.Default))
    // expected output:
    // Settings(
    //   host = "example.com",
    //   port = 8080,
    //   debug = None
    // )
    ```

## Comparison with jsoniter-scala macros

### Feature differences

| Feature | jsoniter-scala macros | Kindlings |
|---------|----------------------|-----------|
| Same API on Scala 2.13 and 3 | Partial (different compile deps) | Yes |
| Sanely-automatic derivation | No | Yes |
| Inline encoding/decoding | No | Yes (`writeToString`, `readFromString`) |
| Recursive types | Needs workarounds | Just works |
| Named tuples | No | Yes |
| Opaque types | No | Yes |
| Scala 3 enums | Yes | Yes |
| Java enums | Yes | Yes |
| `make[A]` convenience factory | Yes | No (use `derive[A]`) |

### Benchmarks

All values in ops/s (higher is better). Measured on macOS, JVM temurin 17.

!!! note
    Kindlings matches or exceeds jsoniter-scala's own macros for all case class benchmarks. SimpleCC reads are slightly faster; Person read/write are at parity. ADT write has a small gap from discriminator overhead.

#### Write

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 59.8M | 59.5M | 59.7M | **~tied** |
| SimpleCC | 3 | 62.7M | 62.9M | 63.8M | **~tied** |
| Person | 2.13 | 4.7M | 4.7M | 4.6M | **~tied** |
| Person | 3 | 5.3M | 5.3M | 5.2M | **~tied** |
| Event | 2.13 | 4.3M | 4.2M | 4.0M | **1.08x faster** |
| Event | 3 | 4.7M | 4.7M | 4.8M | **0.98x** |

#### Read

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 36.1M | 36.0M | 34.8M | **1.04x faster** |
| SimpleCC | 3 | 35.6M | 35.5M | 34.2M | **1.04x faster** |
| Person | 2.13 | 3.6M | 3.7M | 3.6M | **~tied** |
| Person | 3 | 3.6M | 3.6M | 3.6M | **~tied** |
