# YAML Derivation

Drop-in replacement for scala-yaml's built-in `derives` — derives `YamlEncoder`, `YamlDecoder`, and `YamlCodec` for case classes, sealed traits, Scala 3 enums, Java enums, and more.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-yaml-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-yaml-derivation" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-yaml-derivation:{{ kindlings_version() }}
    ```

!!! note
    You also need `scala-yaml` as a runtime dependency:

    ```scala
    libraryDependencies += "org.virtuslab" %%% "scala-yaml" % "{{ libraries.scalaYaml }}"
    ```

## Quick start

??? example "Encoding and decoding a case class"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-yaml-derivation:{{ kindlings_version() }}
    //> using dep org.virtuslab::scala-yaml:{{ libraries.scalaYaml }}

    import hearth.kindlings.yamlderivation._

    case class Person(name: String, age: Int)

    // Encode to YAML string
    val yaml: String = KindlingsYamlEncoder.toYamlString(Person("Alice", 30))
    println(yaml)
    // expected output:
    // name: Alice
    // age: 30

    // Decode from YAML string
    val result = KindlingsYamlDecoder.fromYamlString[Person]("name: Bob\nage: 25")
    println(result)
    // expected output:
    // Right(Person(Bob,25))
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsYamlEncoder.derived[A]` | `YamlEncoder[A]` | Semi-automatic encoder |
| `KindlingsYamlEncoder.encode[A](value)` | `Node` | Inline encoding (no instance allocation) |
| `KindlingsYamlEncoder.toYamlString[A](value)` | `String` | Inline encoding to YAML string |
| `KindlingsYamlEncoder.derived[A]` | `KindlingsYamlEncoder[A]` | Sanely-automatic (given/implicit) |
| `KindlingsYamlDecoder.derived[A]` | `YamlDecoder[A]` | Semi-automatic decoder |
| `KindlingsYamlDecoder.decode[A](node)` | `Either[ConstructError, A]` | Inline decoding from a YAML node |
| `KindlingsYamlDecoder.fromYamlString[A](yaml)` | `Either[YamlError, A]` | Inline decoding from a YAML string |
| `KindlingsYamlDecoder.derived[A]` | `KindlingsYamlDecoder[A]` | Sanely-automatic (given/implicit) |
| `KindlingsYamlCodec.derived[A]` | `KindlingsYamlCodec[A]` | Semi-automatic codec (encoder + decoder) |
| `KindlingsYamlCodec.derived[A]` | `KindlingsYamlCodec[A]` | Sanely-automatic codec |

All methods take an implicit/using `YamlConfig` parameter (defaults to `YamlConfig.default`).

### Type hierarchy

`KindlingsYamlEncoder[A]` extends `YamlEncoder[A]` and `KindlingsYamlDecoder[A]` extends `YamlDecoder[A]`, so derived instances work anywhere the original scala-yaml types are expected.

### Syntax extensions

The `syntax` object provides extension methods for convenient inline usage:

```scala
import hearth.kindlings.yamlderivation.syntax._

val yaml: String = Person("Alice", 30).toYamlString
val result: Either[YamlError, Person] = "name: Bob\nage: 25".fromYamlString[Person]
```

## Configuration

All derivation methods accept an implicit `YamlConfig`:

```scala
import hearth.kindlings.yamlderivation._

implicit val config: YamlConfig = YamlConfig.default
  .withSnakeCaseMemberNames
  .withDiscriminator("type")
```

The defaults match scala-yaml's own derivation (`derives YamlCodec`): absent fields with a constructor default use it,
and the subtypes of sealed traits / enums are written **untagged** (a case class as its fields, a case object as `{}`)
and decoded by trying every subtype in declaration order (on Scala 2, which has no scala-yaml derivation, in the order of
the subtypes' names, see the subtype order note in `feature-parity.md`; the first that decodes wins; when none does, the failure is
scala-yaml's `Cannot parse ...`). Union types (no scala-yaml counterpart) are always wrapped, and Java enum /
`scala.Enumeration` values are written as their names.

| Builder method | Description |
|---------------|-------------|
| `withSnakeCaseMemberNames` | `fieldName` -> `field_name` |
| `withKebabCaseMemberNames` | `fieldName` -> `field-name` |
| `withPascalCaseMemberNames` | `fieldName` -> `FieldName` |
| `withScreamingSnakeCaseMemberNames` | `fieldName` -> `FIELD_NAME` |
| `withTransformMemberNames(f)` | Custom field name transform |
| `withSnakeCaseConstructorNames` | `MyType` -> `my_type` in discriminator |
| `withKebabCaseConstructorNames` | `MyType` -> `my-type` in discriminator |
| `withTransformConstructorNames(f)` | Custom constructor name transform |
| `withDiscriminator(field)` | ADT discriminator field name |
| `withEnumAsStrings` | Encode Scala 3 / Java enums as strings |
| `withWrappedSubtypes` | Without a discriminator, wrap each subtype in a single-key mapping `{ Circle: { radius: 1.0 } }` (Kindlings 0.3.x's default) |
| `withUntaggedSubtypes` | Untagged subtypes, decoded by trying each one (the default, as scala-yaml) |
| `withUseDefaults` | Use case class default values for missing fields (the default, as scala-yaml) |
| `withoutUseDefaults` | Missing fields fail even when they have a default (Kindlings 0.3.x's default) |

`YamlConfig.kindlings03Defaults` restores both defaults of Kindlings 0.3.x (`withoutUseDefaults.withWrappedSubtypes`).

## Annotations

| Annotation | Description |
|-----------|-------------|
| `@fieldName("yaml_name")` | Override YAML field name for a case class field |
| `@transientField` | Exclude a field from encoding/decoding (must have a default value) |

```scala
import hearth.kindlings.yamlderivation.annotations._

case class User(
  @fieldName("user_name") name: String,
  @transientField internalId: Long = 0L
)
```

## Usage examples

??? example "Sealed trait with discriminator"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-yaml-derivation:{{ kindlings_version() }}
    //> using dep org.virtuslab::scala-yaml:{{ libraries.scalaYaml }}

    import hearth.kindlings.yamlderivation._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val config: YamlConfig = YamlConfig.default
      .withDiscriminator("type")
      .withSnakeCaseConstructorNames

    val yaml = KindlingsYamlEncoder.toYamlString[Shape](Circle(5.0))
    println(yaml)
    // expected output:
    // type: circle
    // radius: 5.0

    val decoded = KindlingsYamlDecoder.fromYamlString[Shape]("width: 3\nheight: 4\ntype: rectangle")
    println(decoded)
    // expected output:
    // Right(Rectangle(3.0,4.0))
    ```

??? example "Case class with defaults"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-yaml-derivation:{{ kindlings_version() }}
    //> using dep org.virtuslab::scala-yaml:{{ libraries.scalaYaml }}

    import hearth.kindlings.yamlderivation._

    // constructor defaults fill in absent fields by default, as in scala-yaml (`withoutUseDefaults` turns it off)
    case class Settings(host: String, port: Int = 8080, debug: Boolean = false)

    val result = KindlingsYamlDecoder.fromYamlString[Settings]("host: localhost")
    println(result)
    // expected output:
    // Right(Settings(localhost,8080,false))
    ```

??? example "Snake-case field names"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-yaml-derivation:{{ kindlings_version() }}
    //> using dep org.virtuslab::scala-yaml:{{ libraries.scalaYaml }}

    import hearth.kindlings.yamlderivation._

    implicit val config: YamlConfig = YamlConfig.default
      .withSnakeCaseMemberNames

    case class DatabaseConfig(hostName: String, portNumber: Int, maxConnections: Int)

    val yaml = KindlingsYamlEncoder.toYamlString(DatabaseConfig("localhost", 5432, 10))
    println(yaml)
    // expected output:
    // host_name: localhost
    // port_number: 5432
    // max_connections: 10
    ```

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.yamlderivation.debug._
```

Or enable project-wide via scalac option:

```scala
// build.sbt
scalacOptions += "-Xmacro-settings:yamlDerivation.logDerivation=true"
```

## Comparison with scala-yaml built-in derivation

### Feature differences

| Feature | scala-yaml `derives` | Kindlings |
|---------|---------------------|-----------|
| Same API on Scala 2.13 and 3 | No (Scala 3 only) | Yes |
| Sealed trait / ADT support | Untagged: the subtype's value without type information, decoded by trying each subtype ([#363](https://github.com/VirtusLab/scala-yaml/issues/363)) | The same by default; opt-in single-key wrapper (`withWrappedSubtypes`) or discriminator (`withDiscriminator`) |
| Configuration (naming, discriminator) | No | Yes (`YamlConfig`) |
| Field name annotations | No | Yes (`@fieldName`) |
| Default values | Always on, not configurable | On by default, as scala-yaml; `withoutUseDefaults` turns them off |
| Absent `Option` field | `None` | `None` |
| Failure messages of derived products | `Key x doesn't exist in parsed document`, `Expected MappingNode, got ...` | Same |
| Enum handling | Broken in some cases | Correct |
| Option decoding | Broken in some cases | Correct |
| Inline encoding/decoding | No | Yes (`toYamlString`, `fromYamlString`) |
| Sanely-automatic derivation | No | Yes |
