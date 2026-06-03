# UBJson Derivation

Original module -- derives `UBJsonValueCodec` for case classes, sealed traits, Scala 3 enums, Java enums, and more. [UBJson](https://ubjson.org/) is a binary JSON format with the same data model as JSON but more compact wire representation.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-ubjson-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-ubjson-derivation" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-ubjson-derivation:{{ kindlings_version() }}
    ```

## Quick start

??? example "Encoding and decoding a case class"

    ```scala
    //> using dep com.kubuszok::kindlings-ubjson-derivation:{{ kindlings_version() }}

    import hearth.kindlings.ubjsonderivation._

    case class Person(name: String, age: Int)

    val codec: UBJsonValueCodec[Person] = UBJsonValueCodec.derived[Person]

    // Encode to UBJson binary
    val writer = new UBJsonWriter()
    codec.encode(writer, Person("Alice", 30))
    val bytes: Array[Byte] = writer.toByteArray

    // Decode back
    val decoded: Person = codec.decode(new UBJsonReader(bytes))
    println(decoded)
    // expected output:
    // Person(Alice,30)
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `UBJsonValueCodec.derived[A]` | `UBJsonValueCodec[A]` | Semi-automatic codec |
| `UBJsonValueCodec.derived[A]` | `UBJsonValueCodec[A]` | Sanely-automatic (given/implicit) |

All methods take an implicit/using `UBJsonConfig` parameter (defaults to `UBJsonConfig.default`).

### Type class interface

`UBJsonValueCodec[A]` provides three members:

| Member | Signature | Description |
|--------|-----------|-------------|
| `decode` | `(reader: UBJsonReader): A` | Decode a value from a UBJson reader |
| `encode` | `(writer: UBJsonWriter, value: A): Unit` | Encode a value to a UBJson writer |
| `nullValue` | `A` | The null/default value for type `A` |

### Extension methods

Import `UBJsonValueCodecExtensions._` for extra combinators on codec instances:

| Method | Description |
|--------|-------------|
| `codec.map[B](f)(g)` | Transform decoded values with `f`, encoded values with `g` |
| `codec.mapDecode[B](f)(g)` | Like `map` but `f` returns `Either[String, B]` for validation |

## Configuration

```scala
import hearth.kindlings.ubjsonderivation._

implicit val config: UBJsonConfig = UBJsonConfig.default
  .withSnakeCaseFieldNames
  .withDiscriminator("type")
  .withTransientNone
```

| Builder method | Description |
|---------------|-------------|
| `withSnakeCaseFieldNames` | `fieldName` -> `field_name` |
| `withKebabCaseFieldNames` | `fieldName` -> `field-name` |
| `withPascalCaseFieldNames` | `fieldName` -> `FieldName` |
| `withScreamingSnakeCaseFieldNames` | `fieldName` -> `FIELD_NAME` |
| `withFieldNameMapper(f)` | Custom field name transform |
| `withSnakeCaseAdtLeafClassNames` | ADT subtype name -> `snake_case` |
| `withKebabCaseAdtLeafClassNames` | ADT subtype name -> `kebab-case` |
| `withAdtLeafClassNameMapper(f)` | Custom ADT subtype name transform |
| `withDiscriminator(field)` | ADT discriminator field name |
| `withSkipUnexpectedFields(skip)` | Skip unexpected fields during decoding |
| `withEnumAsStrings` | Encode Scala 3 / Java enums as strings |
| `withTransientDefault` | Skip fields with default values during encoding |
| `withTransientEmpty` | Skip empty collections during encoding |
| `withTransientNone` | Skip `None` fields during encoding |
| `withRequireCollectionFields` | Fail if collection field is missing |
| `withRequireDefaultFields` | Fail if field with default is missing |
| `withCheckFieldDuplication` | Fail on duplicate field names |
| `withBigDecimalPrecision(n)` | Max BigDecimal precision (default: 34) |
| `withBigDecimalScaleLimit(n)` | Max BigDecimal scale (default: 6178) |
| `withBigDecimalDigitsLimit(n)` | Max BigDecimal digits (default: 308) |
| `withMapMaxInsertNumber(n)` | Max map entries (DoS protection) |
| `withSetMaxInsertNumber(n)` | Max set entries (DoS protection) |

## Annotations

| Annotation | Package | Description |
|-----------|---------|-------------|
| `@fieldName("name")` | `hearth.kindlings.ubjsonderivation.annotations` | Override the UBJson field name |
| `@transientField` | `hearth.kindlings.ubjsonderivation.annotations` | Exclude field from codec (must have default) |
| `@stringified` | `hearth.kindlings.ubjsonderivation.annotations` | Encode numeric/boolean field as a string |

```scala
import hearth.kindlings.ubjsonderivation.annotations._

case class User(
  @fieldName("user_name") name: String,
  @transientField cache: Option[String] = None
)
```

## Usage examples

??? example "Sealed trait with discriminator"

    ```scala
    //> using dep com.kubuszok::kindlings-ubjson-derivation:{{ kindlings_version() }}

    import hearth.kindlings.ubjsonderivation._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val config: UBJsonConfig = UBJsonConfig.default
      .withDiscriminator("type")

    val codec: UBJsonValueCodec[Shape] = UBJsonValueCodec.derived[Shape]

    val writer = new UBJsonWriter()
    codec.encode(writer, Circle(5.0): Shape)

    val decoded = codec.decode(new UBJsonReader(writer.toByteArray))
    println(decoded)
    // expected output:
    // Circle(5.0)
    ```

??? example "Transient fields and defaults"

    ```scala
    //> using dep com.kubuszok::kindlings-ubjson-derivation:{{ kindlings_version() }}

    import hearth.kindlings.ubjsonderivation._

    implicit val config: UBJsonConfig = UBJsonConfig.default
      .withTransientDefault
      .withTransientNone

    case class Settings(
      host: String,
      port: Int = 8080,
      debug: Option[Boolean] = None
    )

    val codec: UBJsonValueCodec[Settings] = UBJsonValueCodec.derived[Settings]

    // Default and None fields are omitted during encoding
    val writer = new UBJsonWriter()
    codec.encode(writer, Settings("localhost"))

    // Missing fields use defaults during decoding
    val decoded = codec.decode(new UBJsonReader(writer.toByteArray))
    println(decoded)
    // expected output:
    // Settings(localhost,8080,None)
    ```

??? example "Recursive data types"

    ```scala
    //> using dep com.kubuszok::kindlings-ubjson-derivation:{{ kindlings_version() }}

    import hearth.kindlings.ubjsonderivation._

    case class TreeNode(value: Int, children: List[TreeNode])

    // Recursive types just work -- no special setup needed
    val codec: UBJsonValueCodec[TreeNode] = UBJsonValueCodec.derived[TreeNode]

    val tree = TreeNode(1, List(TreeNode(2, Nil), TreeNode(3, List(TreeNode(4, Nil)))))
    val writer = new UBJsonWriter()
    codec.encode(writer, tree)

    val decoded = codec.decode(new UBJsonReader(writer.toByteArray))
    println(decoded)
    // expected output:
    // TreeNode(1,List(TreeNode(2,List()), TreeNode(3,List(TreeNode(4,List())))))
    ```

??? example "Field name mapping"

    ```scala
    //> using dep com.kubuszok::kindlings-ubjson-derivation:{{ kindlings_version() }}

    import hearth.kindlings.ubjsonderivation._

    implicit val config: UBJsonConfig = UBJsonConfig.default
      .withSnakeCaseFieldNames

    case class UserProfile(firstName: String, lastName: String, emailAddress: String)

    // Fields encoded as: first_name, last_name, email_address
    val codec: UBJsonValueCodec[UserProfile] = UBJsonValueCodec.derived[UserProfile]

    val writer = new UBJsonWriter()
    codec.encode(writer, UserProfile("Alice", "Smith", "alice@example.com"))

    val decoded = codec.decode(new UBJsonReader(writer.toByteArray))
    println(decoded)
    // expected output:
    // UserProfile(Alice,Smith,alice@example.com)
    ```

## Debugging

Import the debug package to log the generated codec during compilation:

```scala
import hearth.kindlings.ubjsonderivation.debug._
```

Or enable project-wide via scalac option:

```scala
// build.sbt
scalacOptions += "-Xmacro-settings:ubjsonDerivation.logDerivation=true"
```
