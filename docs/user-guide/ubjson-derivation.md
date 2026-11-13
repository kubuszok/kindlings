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
| `withFieldNameMapper(f)` | Custom field name transform (a `String => String` or a `PartialFunction`: names it is not defined at stay unchanged) |
| `withSnakeCaseAdtLeafClassNames` | ADT subtype name -> `snake_case` |
| `withKebabCaseAdtLeafClassNames` | ADT subtype name -> `kebab-case` |
| `withAdtLeafClassNameMapper(f)` | Custom ADT subtype name transform; receives the **fully-qualified** leaf name (`pkg.Outer.Leaf`, Scala 3 enum cases as `pkg.Enum.Case`), default `UBJsonConfig.simpleClassName`; also a `PartialFunction` |
| `withJavaEnumValueNameMapper(f)` | Java enum value name transform (also a `PartialFunction`) |
| `withDiscriminator(field)` | ADT discriminator field name |
| `withDiscriminatorFieldName(opt)` | Same, `None` for wrapper-style ADTs |
| `withRequireDiscriminatorFirst(b)` | `false`: the discriminator may be anywhere in the object (default `true`: first field) |
| `withCirceLikeObjectEncoding` | Without a discriminator, case objects are `{"Leaf":{}}` instead of `"Leaf"` |
| `withSkipUnexpectedFields(skip)` | Skip unexpected fields during decoding |
| `withEnumAsStrings` | ADTs of case objects only are strings also with a discriminator |
| `withBigIntDigitsLimit(n)` | Max `BigInt` digits when decoding (default: 308) |
| `withTransientDefault` | Skip fields with default values during encoding |
| `withTransientEmpty` | Skip empty collections during encoding |
| `withTransientNone` | Skip `None` fields during encoding |
| `withRequireCollectionFields` | Fail if a collection/map field without a default value is missing |
| `withRequireDefaultFields` | Fail if field with default is missing |
| `withCheckFieldDuplication` | Fail on duplicate field names |
| `withBigDecimalPrecision(n)` | Max BigDecimal precision (default: 34) |
| `withBigDecimalScaleLimit(n)` | Max BigDecimal scale (default: 6178) |
| `withBigDecimalDigitsLimit(n)` | Max BigDecimal digits (default: 308) |
| `withMapMaxInsertNumber(n)` | Max map entries (DoS protection) |
| `withSetMaxInsertNumber(n)` | Max set entries (DoS protection) |
| `withUseScalaEnumValueId` | `scala.Enumeration` values are their ids (integers) instead of their names |
| `withSkipNestedOptionValues` | An `Option` nested in an `Option` is encoded as a plain `Option` (`Some(None)` is `null`, decoded back as `Some(None)`) |
| `withScalaTransientSupport` | `scala.transient` fields are treated as `@transientField` (also with a runtime config) |
| `withTransientNull` | Skip `null` fields of nullable types (Scala 3 `T \| Null`) during encoding |

A field absent from the decoded object gets its default value, `None` for an `Option`, `null` for a Scala 3 `T | Null`,
or an empty collection/map; any other missing field fails with `UBJsonReaderException` (`missing required field
"<name>"`). As in jsoniter-scala, a `null` for a reference-typed field with a default value (also an `Option` with a
default) and an empty array/object for a collection/map field with a default value decode as that default (so `None` in
a field whose default is a `Some` decodes as the default). The `transient*` options only affect encoding.

The config is evaluated at compile time when possible (`UBJsonConfig.default`, a config passed explicitly, and on
Scala 3 an `implicit val`/`given` initialized with such an expression), and the codec then has no runtime config
checks; configs read from a `var` or computed at runtime are checked when the codec runs, with the same behaviour.

### Encoding (as jsoniter-derivation / jsoniter-scala)

- **ADTs** without a discriminator (the default) wrap case classes in a single-field object keyed by the leaf name
  (`{"Circle":{"radius":1.0}}`) and write case objects as strings (`"Origin"`, or `{"Origin":{}}` with
  `circeLikeObjectEncoding`); with a discriminator case classes are `{"type":"Circle","radius":1.0}` and case objects
  `{"type":"Origin"}`. With `enumAsStrings` ADTs of case objects only are strings also with a discriminator.
- **Java enums and `scala.Enumeration`** values are always strings (Java enum values named by
  `javaEnumValueNameMapper`, `Enumeration` values by their `toString`), or with `useScalaEnumValueId` integers
  (`Enumeration` values' ids).
- **Tuples** are arrays (`[1,"a"]`).
- **Name mappers**: the jsoniter-scala helpers are available on the companion: `UBJsonConfig.simpleClassName`,
  `enforceCamelCase`, `EnforcePascalCase`, `enforce_snake_case`, `enforce_snake_case2`, `` `enforce-kebab-case` ``,
  `` `enforce-kebab-case2` ``, `screamingSnakeCase`; the `with*CaseFieldNames` builders use them.
- **Validation**: contradictory settings (`requireCollectionFields` + `transientEmpty`, `requireDefaultFields` +
  `transientDefault`, a discriminator + `circeLikeObjectEncoding`) fail the compilation when the config is known at
  compile time, and otherwise throw `IllegalArgumentException` (jsoniter-scala's messages) when the codec is created.
  Duplicated keys (after mapping, `@fieldName`, `@named`) and ADT leaves mapping to the same name fail the derivation
  when the names are known at compile time.

- **Classes**: case classes and (as in jsoniter-scala) plain non-abstract classes are objects of their
  primary-constructor parameters, read through their `val`/`var` accessors (a parameter without one fails the
  derivation), with all parameter lists; defaults of non-first parameter lists may depend on the preceding parameters.
  A non-abstract sealed class is one of the leaves of its own ADT.
- **Nested sealed hierarchies** are flattened to their leaves (`sealed trait A; sealed trait B extends A; case class
  C() extends B` encodes `C` as a leaf of `A`, not wrapped twice).
- **Nested options**: an `Option` nested in an `Option` is encoded as the ADT `Option` (leaves `scala.Some` with a
  `value` field and `scala.None`, using the config's ADT encoding: `{"Some":{"value":1}}` / `"None"`, or
  `{"type":"Some","value":1}` / `{"type":"None"}` with a discriminator), so `Some(None)` survives a round-trip; with
  `skipNestedOptionValues` it is a plain `Option` instead.
- **Scala 3 `T | Null`** fields are `null` or a `T`; absent is `null` (never required), and `transientNull` skips them
  when `null`.

`isStringified` / `@stringified` (jsoniter-scala's numbers and booleans as JSON strings) are not supported: UBJson
numbers are typed binary values (`i`/`U`/`I`/`l`/`L`/`d`/`D`, `H` for arbitrary precision), so there is no textual
representation to choose, and `UBJsonReader`/`UBJsonWriter` have no string-number support. The `@stringified`
annotation is kept for source compatibility, but it has no effect.

## Annotations

| Annotation | Package | Description |
|-----------|---------|-------------|
| `@fieldName("name")` | `hearth.kindlings.ubjsonderivation.annotations` | Override the UBJson field name |
| `@named("name")` | `hearth.kindlings.ubjsonderivation.annotations` | On an ADT leaf: its name, used as is; on a field: like `@fieldName` |
| `@transientField` | `hearth.kindlings.ubjsonderivation.annotations` | Exclude field from codec (must have default) |
| `@stringified` | `hearth.kindlings.ubjsonderivation.annotations` | No effect: UBJson numbers are typed binary values (see above) |

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
