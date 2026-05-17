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
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterderivation._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    case class Person(name: String, age: Int)

    implicit val codec: JsonValueCodec[Person] = KindlingsJsonValueCodec.derive[Person]

    val bytes = writeToArray(Person("Alice", 30))
    println(new String(bytes))
    // {"name":"Alice","age":30}

    val person = readFromArray[Person]("""{"name":"Bob","age":25}""".getBytes)
    println(person)
    // Person(Bob,25)
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
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterderivation._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val config: JsoniterConfig = JsoniterConfig.default
      .withDiscriminator("type")

    implicit val codec: JsonValueCodec[Shape] = KindlingsJsonValueCodec.derive[Shape]

    println(writeToString[Shape](Circle(5.0)))
    // {"type":"Circle","radius":5.0}

    println(readFromString[Shape]("""{"type":"Rectangle","width":3,"height":4}"""))
    // Rectangle(3.0,4.0)
    ```

??? example "Transient fields and defaults"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterderivation._
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
    // {"host":"localhost"}

    // Missing fields use defaults
    println(readFromString[Settings]("""{"host":"example.com"}"""))
    // Settings(example.com,8080,None)
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
    Kindlings is now at near-parity with jsoniter-scala's own macros for writes, and slightly faster for SimpleCC reads. Complex types still show a small gap.

#### Write

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 60.1M | 60.0M | 60.1M | **~tied** |
| SimpleCC | 3 | 62.8M | 63.4M | 63.5M | **~tied** |
| SimpleADT | 2.13 | 61.8M | 61.3M | 68.3M | 0.90x |
| SimpleADT | 3 | 63.6M | 64.8M | 71.5M | 0.91x |
| Person | 2.13 | 4.3M | 4.4M | 4.7M | 0.94x |
| Person | 3 | 4.7M | 4.6M | 5.2M | 0.90x |
| Event | 2.13 | 4.1M | 4.2M | 4.4M | 0.95x |
| Event | 3 | 4.2M | 4.2M | 4.4M | 0.95x |

#### Read

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 36.2M | 36.2M | 34.9M | **1.04x faster** |
| SimpleCC | 3 | 36.1M | 35.9M | 34.9M | **1.03x faster** |
| Person | 2.13 | 2.8M | 2.7M | 3.7M | 0.76x |
| Person | 3 | 2.7M | 2.7M | 3.7M | 0.73x |
