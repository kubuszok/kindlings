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
| `KindlingsEncoder.derived[A]` | `KindlingsEncoder[A]` | Sanely-automatic encoder (given/implicit, also usable as semi-automatic); for products and sums the instance is also an `Encoder.AsObject` (like circe's semiauto/`derives`) |
| `KindlingsEncoder.deriveAsObject[A]` | `Encoder.AsObject[A]` | Object encoder; a type that does not encode to a JSON object (value class, tuple, `enumAsStrings` enum known at compile time, ...) is a compile-time error |
| `KindlingsEncoder.deriveUnwrapped[A]` / `KindlingsDecoder.deriveUnwrapped[A]` | `KindlingsEncoder[A]` / `KindlingsDecoder[A]` | Explicit unwrapping of any single-field case class (generic-extras' `deriveUnwrapped*`); automatic unwrapping only applies to `AnyVal` value classes |
| `KindlingsDecoder.patch[A]` | `Decoder[A => A]` | Patch decoder (circe-generic's `deriveFor[A].patch`): every field is optional, an absent or `null` key keeps the original value |
| `KindlingsDecoder.incomplete[F]` | `Decoder[F]` | Incomplete decoder (circe-generic's `deriveFor[F].incomplete`, generic-extras' `deriveConfiguredFor[F].incomplete`): `F = (P1, ..., Pn) => C`, each `Pi` supplies the first remaining field of `C` of that type, the other fields are decoded; a `Labelled["name", T]` parameter supplies the field `name` (circe-generic's shapeless `FieldType`) |
| `KindlingsCodecAsObject.deriveEnum(...)` / `KindlingsEncoder.deriveEnum(...)` / `KindlingsDecoder.deriveEnum(...)` | `Codec[A]` / `KindlingsEncoder[A]` / `KindlingsDecoder[A]` | Scala 3 only: a sum of singletons as strings (`enumAsStrings`), with optional `transformConstructorNames`, like Scala 3 core's `ConfiguredEnumCodec.derive(...)`; a sum with non-singleton cases is a compile-time error |
| `KindlingsCodecAsObject.derive(...)` / `KindlingsEncoder.derive(...)` / `KindlingsDecoder.derive(...)` | as `derived` | Scala 3 only: the configuration as named arguments, like Scala 3 core's `ConfiguredCodec.derive(transformMemberNames = ..., ...)` |
| `@JsonCodec` / `@JsonCodec(encodeOnly = true)` / `@JsonCodec(decodeOnly = true)` / `@ConfiguredJsonCodec` | companion members | Scala 2 only (requires `-Ymacro-annotations`): adds the derived instances to the companion like circe-generic / generic-extras; `@JsonCodec` uses `Configuration.default`, `@ConfiguredJsonCodec` the `Configuration` in scope. On Scala 3 use `derives` |
| `KindlingsEncoder.encode[A](value)` | `Json` | Inline encoding (no instance allocation) |
| `KindlingsDecoder.derived[A]` | `KindlingsDecoder[A]` | Sanely-automatic decoder (given/implicit, also usable as semi-automatic) |
| `KindlingsDecoder.decode[A](json)` | `Either[DecodingFailure, A]` | Inline decoding |
| `KindlingsCodecAsObject.derived[A]` | `KindlingsCodecAsObject[A]` | Sanely-automatic codec (given/implicit, also usable as semi-automatic) |

All methods take an implicit/using `Configuration` parameter (defaults to `Configuration.default`).

### Type hierarchy

`KindlingsEncoder[A]` extends `Encoder[A]` and `KindlingsDecoder[A]` extends `Decoder[A]`, so derived instances work anywhere the original Circe types are expected.

`KindlingsDecoder[A]` and `KindlingsCodecAsObject[A]` also extend `ExtrasDecoder[A]` (a port of generic-extras'
`ExtrasDecoder`): `isStrict` is `true` for a derived case-class decoder with `strictDecoding`, and
`decodeStrict(cursor)` returns the extraneous fields of a failed strict decoding.

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
| `withEnumAsStrings` / `withoutEnumAsStrings` | Encode sums of singletons (case objects, parameterless Scala 3 enum cases, Java enums, `Enumeration`s) as strings |
| `withValueClassUnwrapping` | Code `AnyVal` case classes as their field's value (default): `UserId(5)` → `5` |
| `withoutValueClassUnwrapping` | Code `AnyVal` case classes as JSON objects, like upstream circe: `UserId(5)` → `{"value":5}` |

The name transformations are also available as functions in `renaming` (like Scala 3 core's
`io.circe.derivation.renaming`): `renaming.snakeCase`, `renaming.screamingSnakeCase`, `renaming.kebabCase`,
`renaming.pascalCase` and `renaming.replaceWith("Old" -> "new", ...)`, e.g.
`Configuration.default.withTransformConstructorNames(renaming.replaceWith("Circle" -> "round"))`.

A configuration that can be evaluated at compile time (e.g. `Configuration.default.withDiscriminator("type")` or
`Configuration(strictDecoding = true)` passed inline, and on Scala 3 also a local `implicit val`/`given`) enables
compile-time checks (e.g. `enumAsStrings` on a sum with non-singleton cases) and skips runtime configuration checks in
the generated code. Other configurations are read at runtime. On Scala 2, a local `implicit val` is only known at
runtime (Scala 2 macros cannot read a local value's definition).

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
    // {"radius":5.0,"type":"circle"}

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

## Behaviour matching upstream circe

Decoding and encoding follow circe's Scala 3 `ConfiguredEncoder`/`ConfiguredDecoder` (and circe-generic-extras on
Scala 2):

- **Nested sealed hierarchies are flattened.** With `sealed trait GrandParent; sealed trait Parent extends GrandParent;
  case class Child(a: Int) extends Parent`, `Child(1)` is encoded as `{"Child":{"a":1}}` (or `{"a":1,"type":"Child"}`
  with a discriminator), and `Child` is decoded from its own name at the top level. This also holds when `Parent` has an
  instance derived by Kindlings (`sealed trait Parent extends GrandParent derives KindlingsCodecAsObject`, or
  `implicit val codec = KindlingsCodecAsObject.derived[Parent]` in its companion): like Scala 3 core with a
  `ConfiguredCodec` of the intermediate, its output is used as it is and the names of its subtypes are dispatched to it
  (with the intermediate's own configuration). An intermediate sealed trait with a hand-written `Encoder`/`Decoder`
  keeps using it, under the intermediate's name (Scala 3 core; generic-extras on Scala 2 ignores such instances, since
  shapeless flattens the hierarchy to its leaves).
- **A leaf reachable through several intermediates** (`case class C(..) extends A with B`, `A` and `B` both under the
  same sum) is written once, under its own name; when intermediates have hand-written instances, the first one (in
  declaration order) containing the leaf is used.
- **`enumAsStrings` on nested case objects**: a hierarchy of case objects is an enum, also when an intermediate has a
  user-provided instance (like `ConfiguredEnumCodec`, which ignores such instances): the leaves are coded by name.
- **`useDefaults`**: an absent field, or a `null` that the field's decoder rejects, decodes to the default value. A `null`
  the decoder accepts (`None` for an `Option`) is kept, and a `Decoder[Option[X]]` that decodes an absent key to a value
  (overriding `tryDecode`) keeps that value; circe's `Option` decoder's absent-key `None` falls back to the default.
- **Absent `Option` fields** decode as `None` when `useDefaults` is off, whether or not the field has a default and
  whether the element type is derived or not. An absent field whose type is a value class over an `Option` decodes as
  the wrapped `None` when its decoder is derived (unwrapped) - or, with `useDefaults`, as the field's default, like a
  plain `Option` field; a user-provided decoder of the value class keeps circe's semantics (by default a missing-field
  failure).
- **Strict decoding**: the discriminator key is an expected field; in `decodeAccumulating` the strict failure is reported
  first, followed by the field errors; a wrapper object with more than one key is rejected (the failure lists the direct
  subtypes, like circe's `constructorNames`); a case object (in a sum, at the top level or as a field) rejects any key
  other than the discriminator (`{"Yes":{"extra":1}}` fails).
- **Accumulating decoding** (`decodeAccumulating`) accumulates inside nested derived types, `Option`s, collections and
  maps (every element), and for top-level derived non-products (`KindlingsDecoder.derived[List[Person]]`). A non-object
  input to a product reports only "expecting object" (Scala 3 core `ConfiguredDecoder`).
- **Error messages and cursor history** follow Scala 3 core: `type Shape has no class/object/case named 'X'.`,
  `Shape: could not find discriminator field 'type' or its null.` (at `.type`),
  `Strict decoding Person - unexpected fields: a, b; valid fields: name, age.` (unexpected fields in JSON order),
  `Strict decoding Shape - expected a single key json object with one of: Circle, Rectangle.`,
  `enum Color does not contain case: X`, `WrongTypeExpectation` (`"object"`, `"array"`, `"non-empty json object"`,
  `"string"`) for wrong JSON types, `Couldn't decode key.` at the key's cursor, and smart-constructor (value type)
  errors at the field's cursor.
- **Discriminator position**: the discriminator is appended after the fields (`{"radius":1.0,"type":"Circle"}`), like
  both Scala 3 core and generic-extras (`JsonObject.add`).
- **Tuples** (also containing derived types) encode and decode as JSON arrays of the exact arity, like circe's
  `Encoder`/`Decoder.encodeTupleN`/`decodeTupleN`.
- **Map keys**: a `KeyEncoder`/`KeyDecoder` in scope (user-provided, or circe's own) takes precedence over the built-in
  key codecs on both sides (also for `Int`/`Long`/`Double`/`Short`/`Byte` keys of a map whose values are derived);
  derived keys (value classes, enums, nested case-object hierarchies) are only used when there is none.
- **`Option` and absent keys**: a user-provided `Decoder[Option[X]]` is consulted for an absent key (its `tryDecode` on
  the failed cursor), and a derived `Decoder[Option[X]]` (`KindlingsDecoder.derived[Option[X]]`) keeps circe's
  absent-key `tryDecode` semantics.
- `KindlingsDecoder.derived[Unit]` accepts only `null`, `{}` and `[]`, like circe's `Decoder[Unit]`.
- **Case classes with several parameter lists** are rejected at compile time, also when the first list is empty
  (`case class A()(val b: Int)`): circe does not derive them either, and encoding every list but decoding only the first
  one would not round-trip.
- **`enumAsStrings` on a sum with non-singleton cases** is a compile-time error when the configuration can be evaluated
  at compile time (like circe's `ConfiguredEnumCodec`, which only derives for sums of singletons). A configuration only
  known at runtime (e.g. computed by a method, or a local `implicit val` on Scala 2) cannot be checked: then the decoder
  fails with a `DecodingFailure` and the encoder throws an `IllegalArgumentException`, both naming the problem.
- **Patch decoders** (`KindlingsDecoder.patch[A]`): each field is decoded as `Option[V]` (circe's
  `Decoder.decodeOption`), so an absent or `null` key keeps the original value, a value of the wrong type fails at decode
  time, a non-object patch fails, and nested values are replaced (not deep-merged). Member names follow the
  configuration; defaults are not used (an absent key always keeps the original value).
- **Wrapper objects with superfluous keys** (non-strict): the first key (in JSON order) naming a known constructor is
  used, so `{"extra":true,"Circle":{"radius":1.0}}` decodes as `Circle` (Scala 3 core; generic-extras picks the first
  constructor in declaration order found among the keys, which differs only for objects naming several constructors).
- **Errors inside collection elements** keep their cursor history (`DecodingFailure#pathToRootString` gives e.g.
  `.members[1].age`).
- **Value classes: Kindlings default vs upstream.** By default, `AnyVal` case classes are coded as their field's value
  (`UserId(5)` → `5`, also when the field's type is derived), a documented improvement over upstream circe, where every
  derivation (Scala 3 core, circe-generic, generic-extras) codes them as products. `withoutValueClassUnwrapping`
  (`unwrapValueClasses = false`) restores the upstream format for encoding and decoding: `{"value":5}`, and for
  value-class sum children `{"IntId":{"v":5}}` (or `{"v":5,"type":"IntId"}` with a discriminator). Other value types
  (opaque types, refined/newtype integrations) are always unwrapped; `deriveUnwrapped` unwraps explicitly.
- **Enum map keys** use the constructor name with `transformConstructorNames` on both sides (not `toString`).
- `KindlingsCodecAsObject` keeps the decoder's `decodeAccumulating`.
- **Discriminator with a child that does not encode to a JSON object** (e.g. an unwrapped value class, or a
  user-provided `Encoder` producing a string): like circe-generic-extras, the child is written in the wrapped form
  (`{"IntId":5}`) since it cannot carry the discriminator field. Kindlings also reads this form back, so it round-trips
  (generic-extras only writes it), and the tapir schema derivation shows this form for circe configurations.
- **tapir schemas** (`tapir-schema-derivation` with a circe `Configuration`) describe what the codec writes: subtypes
  wrapped as `{"Name": ..}` without a discriminator, nested sealed hierarchies flattened to their leaves (an intermediate
  with a hand-written `Encoder` kept as one subtype, one with a Kindlings-derived instance flattened), and `AnyVal` case
  classes as objects with `unwrapValueClasses = false`.

## Migration notes (wire format changes)

Changes of the encoded JSON, and of decoding, made to match upstream circe. Values written by an earlier Kindlings
version may need to be migrated:

- **Nested sealed hierarchies**: a leaf of a nested sealed trait was written under the intermediate's name
  (`{"Parent":{"Child":{..}}}`, or `"type":"Parent"` with a discriminator); it is now written under its own name
  (`{"Child":{..}}` / `"type":"Child"`), and the old nested form is no longer read. The same applies to an intermediate
  whose instance is derived by Kindlings. Intermediates with hand-written instances keep the nested form.
- **Map keys**: a user-provided `KeyEncoder` for `Int`/`Long`/`Double`/`Short`/`Byte` was ignored by the encoder when the
  map's values were derived (the decoder used it, so such maps did not round-trip); it is now used.
- **`useDefaults`**: a `Decoder[Option[X]]` decoding an absent key to a value now wins over the field's default.
- **Value classes over `Option` with a user-provided decoder**: an absent key is now decoded by that decoder's
  `tryDecode` (by default a missing-field failure) instead of as `null`.
- **Strict decoding**: case objects decoded outside a sum now reject extra keys; the strict wrapper failure lists the
  direct subtypes instead of the flattened leaves.
- **`useDefaults` and value classes over `Option`**: an absent field of such a type with a default (and a derived
  decoder) now decodes to the default instead of the wrapped `None`.
- **Compile-time configurations**: more configurations are evaluated at compile time (named arguments, Scala 3 local
  vals), so e.g. `enumAsStrings` on a sum with non-singleton cases can now be a compile-time error where it was a
  runtime failure.

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
| Scala 3 enums | Yes (object encoding of full enums; string encoding via `ConfiguredEnum*` for parameterless cases only) | Yes (object encoding of full enums; `enumAsStrings` for parameterless cases) |
| Java enums | No | Yes |
| `@JsonCodec` / `@ConfiguredJsonCodec` annotations | Scala 2 (`-Ymacro-annotations`) | Scala 2 (`-Ymacro-annotations`); Scala 3: `derives` |
| Value classes | Encoded as products | Unwrapped by default; `withoutValueClassUnwrapping` for the upstream product format |

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
