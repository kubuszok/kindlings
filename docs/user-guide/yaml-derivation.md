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

`toYamlString` prints with scala-yaml's own printer, which by default writes an empty sequence or mapping as an empty
value (`tags: ` rather than `tags: []`), exactly as scala-yaml; that reads back as `null`, so such a string does not
decode back into an empty collection. `YamlConfig.default.withEmptyCollectionsAsFlow` prints them as `[]` / `{}` (also
nested), so the printed YAML decodes back (see [feature parity](feature-parity.md#yaml-derivation)).

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
| `withEmptyCollectionsAsFlow` | `toYamlString` prints empty sequences as `[]` and empty mappings as `{}` (also nested), so the printed YAML decodes back (default: an empty value, as scala-yaml's printer) |
| `withoutEmptyCollectionsAsFlow` | Empty collections printed as an empty value (the default, as scala-yaml) |

`YamlConfig.kindlings03Defaults` restores both defaults of Kindlings 0.3.x (`withoutUseDefaults.withWrappedSubtypes`).

The config is evaluated at compile time when possible (`YamlConfig.default`, a config passed explicitly, and on Scala 3
a local `implicit val`/`given`): sealed traits and enums then get only the applicable encoding and decoding
(discriminator, wrapper, untagged or names) with constant subtype names. A config only known at runtime gives the same
results, read when the instance runs; a derived encoder/decoder then computes the field and subtype names its
`transformMemberNames`/`transformConstructorNames` give once per instance (the inline `KindlingsYamlEncoder.encode`/
`KindlingsYamlDecoder.decode` have no instance and compute them on every call), and resolves the instances of its fields and subtypes (e.g. an
`implicit def` `YamlEncoder`) once per instance, not on every call.

## Map keys

The keys of a YAML mapping are scalars, so maps derived by Kindlings read and write their keys with **key codecs**,
`YamlKeyDecoder[K]` (`String => Either[ConstructError, K]`) and `YamlKeyEncoder[K]` (`K => String`), as circe's
`KeyDecoder` / `KeyEncoder` and jsoniter-scala's `JsonKeyCodec` do. For a key type `K`, Kindlings looks for:

1. a `YamlKeyDecoder[K]` / `YamlKeyEncoder[K]` in implicit scope (yours always wins, also for `String`);
2. a built-in one: `String`, `Char`, `Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `BigInt`,
   `BigDecimal`, `java.util.UUID` (numbers and booleans are read as scala-yaml reads them: `0x10`, `0o17`, `1_000`);
3. a derived one: enums of case objects (Scala 3 enums, Java enums, `scala.Enumeration`, sealed traits of case objects)
   by the names of their cases (after `transformConstructorNames`), and value types (`AnyVal`, Scala 3 opaque types,
   refined / Iron / neotype types) over a type with a key codec. A value type's smart constructor validates the key:
   a rejected key is a decoding failure, reported at the key;
4. otherwise `K` is a complex (non-scalar) key: it is read and written with its `YamlDecoder[K]` / `YamlEncoder[K]`
   (yours, or a derived one, e.g. a case class written as a mapping), as scala-yaml does for every key.

If none of them applies, the derivation fails with `No YamlKeyDecoder[K] for the map's keys: ...`.

A `YamlDecoder[K]` / `YamlEncoder[K]` is **not** used for a key type that has a key codec: a custom `YamlDecoder[String]`
changes `String` fields and values, not `String` keys (scala-yaml's `forMap` would use it for keys too). Provide a
`YamlKeyDecoder[K]` / `YamlKeyEncoder[K]` to change how keys are read and written; `YamlKeyDecoder` has `map` and `emap`,
`YamlKeyEncoder` has `contramap`, and both companions have `instance` and the built-in instances to build on.

```scala
//> using scala {{ scala.2_13 }}
//> using dep com.kubuszok::kindlings-yaml-derivation:{{ kindlings_version() }}
//> using dep org.virtuslab::scala-yaml:{{ libraries.scalaYaml }}

import hearth.kindlings.yamlderivation._

sealed trait Color
case object Red extends Color
case object Green extends Color

final case class UserId(value: String) extends AnyVal
object UserId {
  implicit val keyDecoder: YamlKeyDecoder[UserId] =
    YamlKeyDecoder.forString.emap(s => if (s.startsWith("u-")) Right(UserId(s)) else Left(s"Not a user id: $s"))
}

case class Inventory(byColor: Map[Color, Int], byUser: Map[UserId, Int])

println(KindlingsYamlDecoder.fromYamlString[Inventory]("byColor:\n  Red: 1\nbyUser:\n  u-1: 2"))
// expected output:
// Right(Inventory(Map(Red -> 1),Map(UserId(u-1) -> 2)))
println(KindlingsYamlDecoder.fromYamlString[Inventory]("byColor:\n  Blue: 1\nbyUser: {}").left.map(_.msg.linesIterator.next()))
// expected output:
// Left(Unknown key Blue. Expected one of: Green, Red)
```

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
| Repeated key in a mapping decoded as a case class | Its last value | Same |
| Key that is not a scalar in such a mapping | `Parameter of a class must be a scalar value` | Same |
| Default value expressions | Evaluated only when the field is absent, on every such decode | Same |
| `private val` constructor fields | Written and read | Same |
| Collections, maps and `Option`s of derived types | Through the element's `given` instance | Derived in place, with the same results and failures (`Could't construct scala.collection.immutable.List from ...`) |
| Map keys | Any type with a `YamlDecoder` / `YamlEncoder` (no key type class: a custom `YamlDecoder[String]` applies to keys too) | Key codecs (`YamlKeyDecoder` / `YamlKeyEncoder`, see [Map keys](#map-keys)): built in for scalars, derived for enums of case objects and value types; `YamlDecoder` / `YamlEncoder` only for complex keys |
| After a failing field | Every field is decoded, the first failure is reported | The first failure is reported; later fields are not decoded (their defaults are not evaluated) |
| Recursive types | `derives` / a `given` never finishes initializing | Yes |
| Value classes | Not derivable (no `Mirror`) | Unwrapped |
| Enum handling | Broken in some cases | Correct |
| Option decoding | Broken in some cases | Correct |
| Inline encoding/decoding | No | Yes (`toYamlString`, `fromYamlString`) |
| Sanely-automatic derivation | No | Yes |
