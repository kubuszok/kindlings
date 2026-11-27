# Tapir Schema Derivation

Drop-in replacement for Tapir's built-in `Schema.derived` -- derives `Schema[A]` for case classes, sealed traits, Scala 3 enums, Java enums, and more.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-tapir-schema-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-tapir-schema-derivation" % "{{ kindlings_version() }}"
    ```

    You also need `tapir-core`:

    ```scala
    libraryDependencies += "com.softwaremill.sttp.tapir" %%% "tapir-core" % "{{ libraries.tapir }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}
    ```

## Quick start

??? example "Deriving Schema for a case class"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import hearth.kindlings.circederivation._
    import sttp.tapir.Schema

    case class Person(name: String, age: Int)

    // Semi-automatic
    val schema: Schema[Person] = KindlingsSchema.derived[Person].schema
    println(schema.name)
    // expected output:
    // Some(SName(Person,List()))
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsSchema.derived[A]` | `KindlingsSchema[A]` | Semi-automatic schema derivation |
| `KindlingsSchema.derived[A].schema` | `Schema[A]` | The underlying Tapir `Schema[A]` |
| `KindlingsSchema.derived[A]` | `KindlingsSchema[A]` | Sanely-automatic (given/implicit) |
| `KindlingsSchema.derivedEnumeration[A]` | `KindlingsSchema[A]` | A string-enum schema for a sealed trait / `enum` whose children are all case objects (like Tapir's `Schema.derivedEnumeration[A].defaultStringBased`; other children are a compile error) |

`derivedEnumeration` needs no JSON configuration and lists the **plain case names** (`Red`, `Green`, ...), as Tapir's
`derivedEnumeration` does with its default `toString` encoding. It does not apply circe's `transformConstructorNames`
or jsoniter-scala's `adtLeafClassNameMapper`; to get the names your JSON codec writes, use `KindlingsSchema.derived[A]`
with `enumAsStrings` (circe) or the matching jsoniter configuration in scope.

Unlike other Kindlings modules, Tapir Schema derivation takes **no configuration parameter**. Instead, it automatically discovers JSON configuration from sibling modules (see below).

### Automatic JSON configuration discovery

When `kindlings-circe-derivation` or `kindlings-jsoniter-derivation` is on the classpath, `KindlingsSchema` reads their configuration at compile time and applies the same field name transforms. This means your **schema matches your codecs by design** — the API documentation (OpenAPI / Swagger) always reflects the actual JSON payloads, without maintaining a separate configuration.

For example, if your Circe `Configuration` uses `withSnakeCaseMemberNames`, the generated Tapir schema will also use `snake_case` field names — no additional configuration needed, no risk of drift.

#### Required and optional fields

A field is optional in the schema exactly when the decoder derived with the same configuration accepts it missing
from the JSON object:

| Field | jsoniter `JsoniterConfig` | circe `Configuration` |
|-------|---------------------------|-----------------------|
| `Option[A]` | optional | optional |
| has a default value | optional unless `requireDefaultFields` | optional with `useDefaults` |
| collection or map that can be empty (no default) | optional unless `requireCollectionFields` | required |
| non-empty collection (`NonEmptyList`, ... from `kindlings-cats-integration`) | required (unless it has a default value, see above) | required (likewise) |
| Scala 3 nullable type `T \| Null` | optional (and nullable) | like any other field of its type |
| anything else | required | required |

The `transient*` settings of a `JsoniterConfig` only change what the encoder writes, so they do not make fields
optional.

`Option` fields are optional but **not nullable** in the schema, while both codecs write `None` as `null` by default.
To make the payloads match the documentation, use `withTransientNone` (jsoniter) or print circe JSON with
`dropNullValues = true` (see [HTTP APIs with tapir](http-api.md#pitfalls)).

#### What a `JsoniterConfig` changes

With a jsoniter `JsoniterConfig` the schemas describe what the jsoniter codecs (derived with the same config) write:

- **ADTs without a discriminator** (Kindlings' default) are described as wrapped: each leaf is a single-field object
  keyed by the leaf's name (`{"Circle":{"radius":1.0}}`), and case objects are strings (`"Origin"`), also in ADTs
  mixing case classes and case objects. With `circeLikeObjectEncoding` case objects are wrapped too (`{"Origin":{}}`).
  With a discriminator, case objects are objects with just the discriminator field (`{"type":"Origin"}`).
- **Nested sealed hierarchies** are flattened to their leaves, as the codecs read and write only the leaves.
- **Leaf names**: `@named` on a leaf is used as is, otherwise `adtLeafClassNameMapper` receives the leaf's
  fully-qualified name (`pkg.Outer.Leaf`, Scala 3 enum cases as `pkg.Enum.Case`), as in the codecs.
- **Java enums and `scala.Enumeration`** values are strings whatever the discriminator (Java enum values named by
  `javaEnumValueNameMapper`, `Enumeration` values by their `toString`), or integer `id`s with `useScalaEnumValueId`.
- **Tuples** are arrays: positional `_1`, `_2`, ... fields marked with Tapir's `Schema.Tuple` attribute, which Tapir's
  docs interpreter renders as an array with `prefixItems`.
- **Fields**: `@named` on a field renames it like `@fieldName`.
- **`Option[Option[A]]`**: the inner `Option` is described as the ADT the codecs write it as (`{"Some":{"value":1}}` /
  `"None"`, or `{"type":"Some","value":1}` / `{"type":"None"}` with a discriminator), unless `skipNestedOptionValues`.

#### What a circe `Configuration` changes

With a circe `Configuration`, the schema also follows how circe codes sealed hierarchies and value classes:

- without a discriminator, every subtype is shown in the wrapped form circe writes (`{"Circle":{"radius":1.0}}`, a
  product with a single field named after the subtype); with a discriminator, a subtype that is not a JSON object is
  shown wrapped (it cannot carry the discriminator);
- nested sealed hierarchies are flattened to their leaves, like circe writes them; an intermediate sealed trait with a
  hand-written circe `Encoder` is kept as one subtype (its schema is the intermediate's `Schema`), one whose instance
  was derived by Kindlings is flattened;
- with `unwrapValueClasses = false` (`withoutValueClassUnwrapping`), `AnyVal` case classes are shown as objects.

#### Disambiguating multiple JSON configs

If both Circe and Jsoniter configurations are on the classpath, the macro finds multiple configs and fails with
`Multiple JSON library configurations found: circe, jsoniter-scala.` Note that `kindlings-tapir-openapi-jsoniter`
depends on `kindlings-jsoniter-derivation`, so circe users who render OpenAPI with it always hit this. Import an
implicit `PreferSchemaConfig` to tell the macro which one to use (the error message lists the exact declaration and
the `-Xmacro-settings:tapirSchemaDerivation.preferConfig=circe|jsoniter-scala` setting for each configuration found):

```scala
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.tapirschemaderivation._
import hearth.kindlings.circederivation._

implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
```

## Tapir annotations

`KindlingsSchema` supports Tapir's standard annotations from `sttp.tapir.Schema.annotations`:

| Annotation | Target | Description |
|-----------|--------|-------------|
| `@description("text")` | Type, Field | Add OpenAPI description |
| `@title("text")` | Type, Field | Set the schema title |
| `@encodedName("name")` | Type, Field | On a field: override the encoded field name. On a type: name its schema (without type parameters) |
| `@format("fmt")` | Type, Field | Set the schema format (e.g. `"int32"`, `"email"`) |
| `@validate(validator)` | Type, Field | Add a validator |
| `@default(value)` | Type, Field | Set the default value |
| `@deprecated` | Type, Field | Mark schema as deprecated |
| `@hidden` | Field | Hide field from the schema (Tapir's `Schema.derived` ignores it on fields) |
| `@encodedExample(value)` | Type, Field | Add an OpenAPI encoded example |
| `@validateEach(validator)` | Field | Validate the elements of a collection/`Option` field |
| `@customise(schema => schema)` | Type, Field | Apply an arbitrary transform to the derived schema |

As in Tapir's derivation:

- annotations are applied in declaration order, so several `@customise` on one element run one after another, after
  the metadata annotations listed before them;
- a subtype of a sealed trait inherits the trait's annotations (except `@encodedName`), and a field inherits the
  annotations of the parent's member with the same name (e.g. `@description` on `def name: String` of the sealed
  trait), unless it has an annotation of the same kind itself;
- the values of a `scala.Enumeration` are described by a schema named after the enumeration object, carrying the
  object's annotations.

Unlike Tapir's derivation, the annotations of a sealed trait are also applied to the trait's own schema, and the
discriminator value of a subtype is the one the JSON codec writes, whatever its `@encodedName`.

```scala
import sttp.tapir.Schema.annotations._
import sttp.tapir.Validator

@description("A user account")
@title("User")
case class User(
  @description("The username") name: String,
  @format("int32") @validate(Validator.min(0)) age: Int,
  @hidden internalId: Long
)
```

## Usage examples

??? example "Sealed trait schema"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import hearth.kindlings.circederivation._
    import sttp.tapir.Schema

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    val schema: Schema[Shape] = KindlingsSchema.derived[Shape].schema
    println(schema.name)
    // Some(SName(Shape,List()))
    ```

??? example "Schema with Tapir annotations"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import hearth.kindlings.circederivation._
    import sttp.tapir.Schema
    import sttp.tapir.Schema.annotations._
    import sttp.tapir.Validator

    @description("A person with metadata")
    @title("PersonMeta")
    case class AnnotatedPerson(
      @description("The name") name: String,
      @format("int32") age: Int
    )

    val schema: Schema[AnnotatedPerson] = KindlingsSchema.derived[AnnotatedPerson].schema
    println(schema.description)
    // Some(A person with metadata)
    ```

??? example "Schema with JSON config discovery"

    ```scala
    import hearth.kindlings.circederivation._
    import hearth.kindlings.tapirschemaderivation._
    import sttp.tapir.Schema

    // Circe configuration with snake_case
    implicit val circeConfig: Configuration = Configuration.default
      .withSnakeCaseMemberNames

    case class UserProfile(firstName: String, lastName: String)

    // Schema automatically picks up snake_case from Circe config
    val schema: Schema[UserProfile] = KindlingsSchema.derived[UserProfile].schema
    // Field names: first_name, last_name (matching JSON encoding)
    ```

??? example "Recursive data types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import hearth.kindlings.circederivation._
    import sttp.tapir.Schema

    case class TreeNode(value: Int, children: List[TreeNode])

    // Recursive types work out of the box — uses schema references
    val schema: Schema[TreeNode] = KindlingsSchema.derived[TreeNode].schema
    ```

??? example "Generic types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import hearth.kindlings.circederivation._
    import sttp.tapir.Schema

    case class Box[A](value: A)
    case class Person(name: String, age: Int)

    implicit val personSchema: Schema[Person] = KindlingsSchema.derived[Person].schema
    println(personSchema.name)
    // Some(SName(Person,List()))

    // Box[Person] picks up the Person schema via implicit
    lazy val boxSchema: Schema[Box[Person]] = KindlingsSchema.derived[Box[Person]].schema
    println(boxSchema.name)
    // Some(SName(Box,List(Person)))
    ```

Generic types can also take the schemas of their type parameters as `KindlingsSchema` evidence — e.g. with Scala 3
`case class Box[A](value: A) derives KindlingsSchema`, or on Scala 2.13 with
`implicit def boxSchema[A: KindlingsSchema]: KindlingsSchema[Box[A]] = KindlingsSchema.derived[Box[A]]` — and the schema
name includes the actual type parameter (e.g. `Box[Int]`).

## Maps, collections and other built-in types

Kindlings derives schemas for `Option`, collections, `Map`s and `Either` itself (consistently with the JSON library's
configuration, e.g. jsoniter's `mapAsArray`) instead of using Tapir's built-in `Schema.schemaForOption`,
`schemaForIterable`, `schemaForSet`, `schemaForArray`, `schemaForMap`, `schemaForEither` and
`derivedEnumerationValue`. The result is the same whether or not the element types have an implicit `Schema`:

- `Set`s are marked with unique items (as Tapir does),
- `Map`s with `String` keys are named like Tapir's `Schema.schemaForMap` (`Map_V`, e.g. a `Map_Double` component in
  OpenAPI), and those with other keys like `Schema.schemaForMap[K, V](keyToString)` (`Map_K_V`); as Tapir's, a map's
  own schema is not optional (as a field it is optional exactly when the decoder accepts it missing),
- collections and maps that cannot be empty (cats' `NonEmptyList`, `NonEmptyVector`, `NonEmptyChain`, `NonEmptySet`,
  `NonEmptyMap`, ... with `kindlings-cats-integration`) are required and validated with `Validator.minSize(1)`
  (`minItems: 1` in OpenAPI / JSON Schema), as in tapir-cats,
- `Either` and `scala.Enumeration` values are derived like the sealed hierarchies the JSON libraries encode them as,
- `Map`s are open products (`additionalProperties`) keyed by the strings the JSON codecs write as object keys: `String`
  keys as they are; primitives (and their boxes), `UUID`, `BigInt`, `BigDecimal` and `java.time` keys as their
  `toString`; families of case objects (sealed traits, Scala 3 `enum`s, Java enums) as the name the codec writes
  for the case (circe's `transformConstructorNames`, jsoniter-scala's `adtLeafClassNameMapper` / `@named`); any type
  with the library's own key codec (circe's `KeyEncoder`, jsoniter-scala's `JsonKeyCodec`, also user-provided ones,
  which take precedence) as that codec writes it; and value types wrapping any of those — `AnyVal` classes, opaque
  types (e.g. `opaque type Counter = Int`, written as `{"7":...}`), and — with the corresponding integration module —
  refined, Iron, neotype and scala-newtype types — as their underlying value. The schema is the one tapir builds with
  `Schema.schemaForMap[K, V](keyToString)`. For any other key type, provide a `Schema[Map[K, V]]`, e.g. with that
  method.

Named schemas mention their type parameters by full name, as Tapir's do; OpenAPI component names are made of the short
names (`Map_UserId_Int`). A type which is an abstract or opaque type member of an object — an Iron
`object Key extends RefinedType[String, C]` (`Key.T`), a neotype `object Name extends Newtype[String]` (`Name.Type`),
a scala-newtype `@newtype` (`Name.Type`) — is named after that object (`Map_Key_Int`), not after the member (whose short
name `T`/`Type` all of them share, so that two different maps would be documented as `Map_T_Int` and `Map_T_Int1`).
This is the name Tapir's `Schema.schemaForMap[K, V]` gives on Scala 3 when the key is written through its alias
(`type Key = Key.T`); for a scala-newtype key on Scala 2.13 Tapir uses the member (`Name.Type`, so `Map_Type_Int` for
every newtype). Type aliases among the type arguments are printed dealiased (e.g. refined's `NonEmpty` as `Not[Empty]`,
`Map_Refined_String_Not_Empty_Int`), where Tapir keeps the alias' name.

Since maps are named, two fields of the same map type share one component in OpenAPI (e.g. `Map_String`). Field-level
customisations (a `@description` on the field, or `.modify(_.field)(_.description(...))`) stay on the field: when they
differ between usages, Tapir's docs interpreter keeps the shared component without them and puts each one next to the
field's `$ref` — exactly as with Tapir's own derivation (checked by the oracle tests).

A value type wrapping a collection or map (an `AnyVal` class, an opaque type, an Iron `List[A] :| MinLength[1]`, ...)
is coded as that collection, but the decoders do not fill it in when it is absent, so — unlike a plain collection — it is
a required field (unless it has a default value).

With a discriminator, each subtype's schema gets the discriminator field appended (unless the subtype already has it),
carrying the subtype's value (`"const": "Circle"` in OpenAPI / JSON Schema), as Tapir's own derivation does.

Your own implicit `Schema` for any of these types (e.g. `Schema[Map[UserId, Int]]`, `Schema[Option[Int]]`) always
takes precedence. Tapir's schemas for primitive and other leaf types (`String`, `Int`, `UUID`, `Instant`, ...) are
used as-is, and `Char` is treated as a `String`.

## Validators for refined, Iron, neotype and scala-newtype types

With the corresponding Kindlings integration module (`kindlings-iron-integration`, `kindlings-refined-integration`,
`kindlings-neotype-integration`, `kindlings-newtype-integration`), a refined/newtype type is a value type: its schema
is the schema of the underlying type, **without validators** for its constraints. Kindlings does not translate the
constraints itself — the Tapir integration of each library does, and Kindlings uses its schemas like any other implicit
`Schema` when they are imported where the schema is derived:

| Library | Tapir integration (add it as a dependency) | Import |
|---------|--------------------------------------------|--------|
| [Iron](iron-integration.md#tapir-schemas) | `"com.softwaremill.sttp.tapir" %% "tapir-iron"` | `import sttp.tapir.codec.iron.given` |
| [refined](refined-integration.md#tapir-schemas) | `"com.softwaremill.sttp.tapir" %% "tapir-refined"` | `import sttp.tapir.codec.refined._` |
| [neotype](neotype-integration.md#tapir-schemas) | `"io.github.kitlangton" %% "neotype-tapir"` | `import neotype.interop.tapir.given` |
| [scala-newtype](newtype-integration.md#tapir-schemas) | `"com.softwaremill.sttp.tapir" %% "tapir-newtype"` (Scala 2.13 only) | `import sttp.tapir.codec.newtype._` |

```scala
import sttp.tapir.codec.iron.given // constraints become validators (minLength, pattern, minimum, ...)

val schema = KindlingsSchema.derived[Account].schema
```

The `upstream-oracle-tests` check that, with the import, the schema is identical to the one Tapir derives with the
integration (see each integration guide for what differs without it).

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.tapirschemaderivation.debug._
```

This enables the `LogDerivation` implicit for `KindlingsSchema`, printing the derivation steps to the compiler output.

## Comparison with Tapir built-in

### Feature differences

| Feature | Tapir built-in (`Schema.derived`) | Kindlings |
|---------|----------------------------------|-----------|
| Scala 2.13 support | Yes (Magnolia) | Yes |
| Same API on Scala 2.13 and 3 | No (Magnolia on 2.13, Mirrors on 3) | Yes |
| Sanely-automatic derivation | No | Yes |
| Recursive types | Manual (`implicit lazy val` on 2.13, `implicit def` on 3) | Just works |
| Named tuples | No | Yes |
| Opaque types | No | Yes |
| Java enums | Partial | Yes |
| Automatic JSON config discovery | No | Yes |
| Tapir annotation support | Yes | Yes |
| Value types (`AnyVal`) | Scala 2.13 only (on Scala 3 a hand-written schema is needed) | Yes |
| Non-`String` map keys (`Int`, `UUID`, case-object enums, types with a key codec, value types, opaque types, refined, ...) | No (needs `Schema.schemaForMap`) | Yes (the same schema) |
| `Char` | No | Yes (as `String`) |

### Checked against Tapir

The `upstream-oracle-tests` module derives the same types with Kindlings and with Tapir's own derivation (and with
tapir-cats, tapir-iron, tapir-refined, neotype-tapir and tapir-newtype) and compares the JSON Schemas Tapir's docs
interpreter renders for them, on Scala 2.13 and 3. The only differences are the documented ones: required and optional
fields follow the decoder (see "Required and optional fields" above; e.g. with circe, collections are required, while
Tapir marks every collection optional), refined/Iron constraints become validators only with the library's Tapir
integration imported (see "Validators for refined, Iron, neotype and scala-newtype types" above), and maps keyed by type
members of objects are named after the object (see "Maps, collections and other built-in types").

It also runs the cases of Tapir's own derivation tests through both derivations and compares everything a `Schema`
contains, the subtype chosen for a value and `applyValidation`. The differences found there are documented ones too:
discriminator values, union types and `Either` follow the JSON codec; `@hidden` on fields and the annotations of a
sealed trait on its own schema are applied (Tapir's derivation ignores them); subtypes are listed in declaration order
on Scala 3 (Tapir sorts them by name); and a parameterless case of a Scala 3 `enum` is named with its enum
(`pkg.Shape.Dot`, Tapir: `pkg.Dot`). See the tapir-schema-derivation section of the feature parity page.

### Benchmarks

Tapir Schema derivation generates a runtime `Schema[A]` value. At runtime, accessing the schema is just a field read, so there is no meaningful performance difference between implementations:

| Benchmark | Kindlings | Tapir built-in |
|-----------|-----------|---------------|
| Schema field access | ~3B ops/s | ~3B ops/s |

The performance is identical because both approaches produce the same runtime representation -- the work is done entirely at compile time.
