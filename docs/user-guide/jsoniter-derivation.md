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

    implicit val codec: JsonValueCodec[Person] = KindlingsJsonValueCodec.derived[Person]

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
| `KindlingsJsonValueCodec.derived[A]` | `KindlingsJsonValueCodec[A]` | Sanely-automatic codec (given/implicit, also usable as semi-automatic) |
| `KindlingsJsonValueCodec.writeToString[A](value)` | `String` | Inline encoding |
| `KindlingsJsonValueCodec.readFromString[A](json)` | `Either[JsonReaderException, A]` | Inline decoding |
| `KindlingsJsonCodec.derived[A]` | `KindlingsJsonCodec[A]` | Sanely-automatic combined value + key codec (given/implicit, also usable as semi-automatic) |
| `KindlingsJsonCodec.deriveKeyCodec[A]` | `JsonKeyCodec[A]` | Key codec (for map keys) |

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

    implicit val codec: JsonValueCodec[Shape] = KindlingsJsonValueCodec.derived[Shape]

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

    implicit val codec: JsonValueCodec[Settings] = KindlingsJsonValueCodec.derived[Settings]

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

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.jsoniterderivation.debug._
```

Or enable project-wide via scalac option:

```scala
// build.sbt
scalacOptions += "-Xmacro-settings:jsoniterDerivation.logDerivation=true"
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
    Kindlings is roughly at parity with jsoniter-scala's own macros: writes range from ~tied to 1.6x faster depending on the type, reads from ~0.9x to 1.6x. The small gaps in both directions are dominated by fork-to-fork JIT variance.

#### Write

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 59.5M | 60.2M | 62.4M | 0.97x |
| SimpleCC | 3 | 62.0M | 62.4M | 58.4M | **1.07x faster** |
| Person | 2.13 | 2.4M | 3.8M | 3.0M | **1.3x faster** |
| Person | 3 | 4.7M | 4.8M | 4.8M | **~tied** |
| Event | 2.13 | 3.2M | 2.4M | 2.1M | **1.6x faster** |
| Event | 3 | 3.8M | 3.3M | 4.3M | 0.90x |

#### Read

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 26.0M | 28.5M | 30.9M | 0.92x |
| SimpleCC | 3 | 27.2M | 27.9M | 30.8M | 0.91x |
| Person | 2.13 | 3.2M | 2.6M | 2.0M | **1.6x faster** |
| Person | 3 | 2.2M | 2.2M | 2.6M | 0.85x |
