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
    // name: Alice
    // age: 30

    // Decode from YAML string
    val result = KindlingsYamlDecoder.fromYamlString[Person]("name: Bob\nage: 25")
    println(result)
    // Right(Person(Bob,25))
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsYamlEncoder.derive[A]` | `YamlEncoder[A]` | Semi-automatic encoder |
| `KindlingsYamlEncoder.encode[A](value)` | `Node` | Inline encoding (no instance allocation) |
| `KindlingsYamlEncoder.toYamlString[A](value)` | `String` | Inline encoding to YAML string |
| `KindlingsYamlEncoder.derived[A]` | `KindlingsYamlEncoder[A]` | Sanely-automatic (given/implicit) |
| `KindlingsYamlDecoder.derive[A]` | `YamlDecoder[A]` | Semi-automatic decoder |
| `KindlingsYamlDecoder.decode[A](node)` | `Either[ConstructError, A]` | Inline decoding from a YAML node |
| `KindlingsYamlDecoder.fromYamlString[A](yaml)` | `Either[YamlError, A]` | Inline decoding from a YAML string |
| `KindlingsYamlDecoder.derived[A]` | `KindlingsYamlDecoder[A]` | Sanely-automatic (given/implicit) |
| `KindlingsYamlCodec.derive[A]` | `KindlingsYamlCodec[A]` | Semi-automatic codec (encoder + decoder) |
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
  .withUseDefaults
```

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
| `withUseDefaults` | Use case class default values for missing fields |

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
    // radius: 5.0
    // type: circle

    val decoded = KindlingsYamlDecoder.fromYamlString[Shape]("width: 3\nheight: 4\ntype: rectangle")
    println(decoded)
    // Right(Rectangle(3.0,4.0))
    ```

??? example "Case class with defaults"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-yaml-derivation:{{ kindlings_version() }}
    //> using dep org.virtuslab::scala-yaml:{{ libraries.scalaYaml }}

    import hearth.kindlings.yamlderivation._

    implicit val config: YamlConfig = YamlConfig.default.withUseDefaults

    case class Settings(host: String, port: Int = 8080, debug: Boolean = false)

    val result = KindlingsYamlDecoder.fromYamlString[Settings]("host: localhost")
    println(result)
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
    // host_name: localhost
    // port_number: 5432
    // max_connections: 10
    ```

## Comparison with scala-yaml built-in derivation

### Feature differences

| Feature | scala-yaml `derives` | Kindlings |
|---------|---------------------|-----------|
| Same API on Scala 2.13 and 3 | No (Scala 3 only) | Yes |
| Sealed trait / ADT support | No | Yes |
| Configuration (naming, discriminator) | No | Yes (`YamlConfig`) |
| Field name annotations | No | Yes (`@fieldName`) |
| Default values | No | Yes (`withUseDefaults`) |
| Enum handling | Broken in some cases | Correct |
| Option decoding | Broken in some cases | Correct |
| Inline encoding/decoding | No | Yes (`toYamlString`, `fromYamlString`) |
| Sanely-automatic derivation | No | Yes |
