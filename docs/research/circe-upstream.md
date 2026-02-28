# Circe Upstream Library Research

## Library Versions

- **circe**: 0.14.14 (dependency used by circe-generic-extras), current latest 0.14.10+ (core)
- **circe-generic-extras**: 0.14.x (base version 0.14, depends on circe 0.14.14)
- **circe-derivation** (separate repo): Scala 2 only, superseded by circe core for Scala 3

## Repository Layout

There are **three** distinct codebases to understand:

1. **circe/circe-generic-extras** (GitHub) -- Scala 2 + Scala 3, Shapeless-based for Scala 2
   - `generic-extras/src/main/scala/` -- shared code (Configuration)
   - `generic-extras/src/main/scala-2/` -- Scala 2 macros (ConfigurableDeriver, semiauto, etc.)
   - `generic-extras/src/main/scala-3/` -- Scala 3 (semiauto, auto, UnwrappedDerivation)
   - Tests: `generic-extras/src/test/scala/`

2. **circe/circe** (core) -- `io.circe.derivation` package, Scala 3 native
   - `modules/core/shared/src/main/scala-3/io/circe/derivation/` -- Configuration, ConfiguredCodec, ConfiguredDecoder, ConfiguredEncoder, ConfiguredEnum*, renaming, Default, etc.
   - Tests: `modules/tests/shared/src/test/scala-3/io/circe/` -- ConfiguredDerivesSuite, ConfiguredEnumDerivesSuites, DerivesSuite, SemiautoDerivationSuite, FromProductSuite

3. **circe/circe-derivation** (separate repo, Scala 2 only) -- `io.circe.derivation` with macro-annotations
   - Has its own `Configuration` (sealed trait with `Codec`, `DecodeOnly`, `EncodeOnly` subtypes)
   - Has `@JsonCodec`, `@SnakeCaseJsonCodec`, `@KebabCaseJsonCodec` macro annotations
   - Has `@JsonKey` and `@JsonNoDefault` annotations

---

## Configuration Class -- All Fields

### circe-generic-extras `Configuration` (shared Scala 2/3)

| Field | Type | Default | Description |
|---|---|---|---|
| `transformMemberNames` | `String => String` | `identity` | Transforms case class field names in JSON |
| `transformConstructorNames` | `String => String` | `identity` | Transforms sealed trait/enum constructor names in JSON |
| `useDefaults` | `Boolean` | `false` | When true, missing fields use case class default values |
| `discriminator` | `Option[String]` | `None` | When `Some("type")`, ADT constructor name stored as a field instead of wrapper object key |
| `strictDecoding` | `Boolean` | `false` | When true, rejects JSON with superfluous/unexpected fields |

### circe core `Configuration` (Scala 3 only, `io.circe.derivation`)

Identical fields to circe-generic-extras:

| Field | Type | Default | Description |
|---|---|---|---|
| `transformMemberNames` | `String => String` | `Predef.identity` | Transforms case class field names |
| `transformConstructorNames` | `String => String` | `Predef.identity` | Transforms constructor names |
| `useDefaults` | `Boolean` | `false` | Use default values for missing fields |
| `discriminator` | `Option[String]` | `None` | Discriminator field name for ADTs |
| `strictDecoding` | `Boolean` | `false` | Reject JSON with extra fields |

### circe-derivation `Configuration` (separate repo, Scala 2 only)

Different design -- sealed trait with three subtypes:

| Variant | Purpose |
|---|---|
| `Configuration.Codec` | Creates both encoder and decoder |
| `Configuration.DecodeOnly` | Creates decoder only |
| `Configuration.EncodeOnly` | Creates encoder only |

Fields are similar but `useDefaults` defaults to `true` and no `strictDecoding`.

### Kindlings `Configuration` (for comparison)

Same as circe-generic-extras plus:
- `enumAsStrings: Boolean = false` (Kindlings addition)

### Configuration Builder Methods

All three libraries provide these builder methods:

| Method | Description |
|---|---|
| `withSnakeCaseMemberNames` | `thisIsAField` -> `this_is_a_field` |
| `withScreamingSnakeCaseMemberNames` | `thisIsAField` -> `THIS_IS_A_FIELD` |
| `withKebabCaseMemberNames` | `thisIsAField` -> `this-is-a-field` |
| `withPascalCaseMemberNames` | `thisIsAField` -> `ThisIsAField` |
| `withSnakeCaseConstructorNames` | `ConfigExampleFoo` -> `config_example_foo` |
| `withScreamingSnakeCaseConstructorNames` | `ConfigExampleFoo` -> `CONFIG_EXAMPLE_FOO` |
| `withKebabCaseConstructorNames` | `ConfigExampleFoo` -> `config-example-foo` |
| `withPascalCaseConstructorNames` | `pascalExampleFoo` -> `PascalExampleFoo` |
| `withDefaults` | Sets `useDefaults = true` |
| `withDiscriminator(field)` | Sets `discriminator = Some(field)` |
| `withStrictDecoding` | Sets `strictDecoding = true` |
| `withTransformMemberNames(f)` | Custom member name transform |
| `withTransformConstructorNames(f)` | Custom constructor name transform |

circe core also has: `withoutDefaults`, `withoutDiscriminator`, `withoutStrictDecoding`.

---

## Renaming Functions (Case Transformation Logic)

From `renaming.scala` in circe core:

```scala
// snakeCase: two regex patterns
private val basePattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
private val swapPattern = Pattern.compile("([a-z\\d])([A-Z])")

val snakeCase: String => String = { s =>
  val partial = basePattern.matcher(s).replaceAll("$1_$2")
  swapPattern.matcher(partial).replaceAll("$1_$2").toLowerCase
}

val screamingSnakeCase: String => String = { s =>
  // Same as snakeCase but .toUpperCase
}

val kebabCase: String => String = { s =>
  // Same as snakeCase but with '-' separator
}

val pascalCase: String => String = { s =>
  s"${s.charAt(0).toUpper}${s.substring(1)}"
}

// Also provides:
final def replaceWith(pairs: (String, String)*): String => String =
  original => pairs.toMap.getOrElse(original, original)
```

**Important**: The regex-based approach handles edge cases like consecutive capitals (`HTMLParser` -> `html_parser`) differently from a simple char-by-char approach. The Kindlings implementation uses a simpler char-by-char approach that may differ in these edge cases.

---

## Annotations

### circe-generic-extras annotations

| Annotation | Package | Purpose |
|---|---|---|
| `@JsonKey(value: String)` | `io.circe.generic.extras` | Override the JSON field name for a specific case class member |
| `@ConfiguredJsonCodec` | `io.circe.generic.extras` | Macro annotation to auto-generate encoder/decoder for annotated type |
| `@ConfiguredJsonCodec(encodeOnly = true)` | same | Only generate encoder |
| `@ConfiguredJsonCodec(decodeOnly = true)` | same | Only generate decoder |

**Note**: There is **no** `@JsonNoDefault` in circe-generic-extras itself.

### circe-derivation annotations (separate repo)

| Annotation | Package | Purpose |
|---|---|---|
| `@JsonKey(value: String)` | `io.circe.derivation.annotations` | Override JSON field name |
| `@JsonNoDefault()` | `io.circe.derivation.annotations` | Force field to be required even when `useDefaults = true` |
| `@JsonCodec` | `io.circe.derivation.annotations` | Macro annotation for codec generation |
| `@SnakeCaseJsonCodec` | `io.circe.derivation.annotations` | `@JsonCodec` with snake_case |
| `@KebabCaseJsonCodec` | `io.circe.derivation.annotations` | `@JsonCodec` with kebab-case |

### Kindlings annotations (for comparison)

| Annotation | Purpose | Upstream equivalent |
|---|---|---|
| `@fieldName(name: String)` | Override JSON field name | `@JsonKey(value)` |
| `@transientField` | Force field to be required | `@JsonNoDefault()` |

---

## Derivation Methods

### circe-generic-extras (semiauto)

| Method | Return Type | Purpose |
|---|---|---|
| `deriveConfiguredDecoder[A]` | `Decoder[A]` | Configured decoder |
| `deriveConfiguredEncoder[A]` | `Encoder.AsObject[A]` | Configured encoder |
| `deriveConfiguredCodec[A]` | `Codec.AsObject[A]` | Configured codec |
| `deriveConfiguredFor[A]` | `DerivationHelper[A]` | Helper with `incomplete` and `patch` methods |
| `deriveExtrasDecoder[A]` | `ExtrasDecoder[A]` | Decoder with strict decoding support |
| `deriveExtrasEncoder[A]` | `Encoder.AsObject[A]` | Same as configured encoder |
| `deriveExtrasCodec[A]` | `ExtrasAsObjectCodec[A]` | Codec with strict decoding support |
| `deriveEnumerationDecoder[A]` | `Decoder[A]` | Enum-as-string decoder |
| `deriveEnumerationEncoder[A]` | `Encoder[A]` | Enum-as-string encoder |
| `deriveEnumerationCodec[A]` | `Codec[A]` | Enum-as-string codec |
| `deriveUnwrappedDecoder[A]` | `Decoder[A]` | Value class unwrapped decoder |
| `deriveUnwrappedEncoder[A]` | `Encoder[A]` | Value class unwrapped encoder |
| `deriveUnwrappedCodec[A]` | `Codec[A]` | Value class unwrapped codec |

### circe core (Scala 3 derivation)

| Method | Return Type | Purpose |
|---|---|---|
| `Decoder.derived[A]` | `Decoder[A]` | Basic derivation (no config) |
| `Encoder.AsObject.derived[A]` | `Encoder.AsObject[A]` | Basic derivation (no config) |
| `Codec.AsObject.derived[A]` | `Codec.AsObject[A]` | Basic derivation (no config) |
| `ConfiguredDecoder.derived[A]` | `ConfiguredDecoder[A]` | Configured decoder |
| `ConfiguredEncoder.derived[A]` | `ConfiguredEncoder[A]` | Configured encoder |
| `ConfiguredCodec.derived[A]` | `ConfiguredCodec[A]` | Configured codec |
| `Codec.AsObject.derivedConfigured` | `ConfiguredCodec[A]` | Alternative syntax for configured codec |
| `ConfiguredEnumDecoder.derived[A]` | `ConfiguredEnumDecoder[A]` | Enum-as-string decoder |
| `ConfiguredEnumEncoder.derived[A]` | `ConfiguredEnumEncoder[A]` | Enum-as-string encoder |
| `ConfiguredEnumCodec.derived[A]` | `ConfiguredEnumCodec[A]` | Enum-as-string codec |

### Special: `ExtrasDecoder` and `ExtrasAsObjectCodec`

`ExtrasDecoder[A]` extends `Decoder[A]` with:
- `isStrict: Boolean` -- whether strict decoding is enabled (default false)
- `StrictResult[A] = Either[(DecodingFailure, List[String]), A]` -- result type that can include extraneous field names
- `decodeStrict(c: HCursor): StrictResult[A]` -- decode with strict mode

`ExtrasAsObjectCodec[A] = Codec.AsObject[A] with ExtrasDecoder[A]` -- combined type

### Special: `DerivationHelper[A]`

Returned by `deriveConfiguredFor[A]`, provides:
- `incomplete[P, C, T, R]`: Decoder for case classes with some constructor params pre-applied
- `patch[R, O]`: `Decoder[A => A]` for partial updates

---

## Features (Detailed)

### Feature: Member Name Transformation (`transformMemberNames`)

**Description**: Transforms case class field names in JSON output/input.

**Test cases (from ConfiguredDerivesSuite, SharedConfiguredAutoDerivedSuite)**:
- Snake case: `thisIsAField` -> `this_is_a_field`, `a` -> `a`, `b` -> `b`
- Screaming snake: `thisIsAField` -> `THIS_IS_A_FIELD`, `a` -> `A`, `b` -> `B`
- Kebab case: `thisIsAField` -> `this-is-a-field`
- Pascal case: `thisIsAField` -> `ThisIsAField`, `a` -> `A`, `b` -> `B`

**Corner cases**:
- Single-character field names (`a`, `b`) are transformed too (e.g., screaming snake: `a` -> `A`)
- Already-capitalized names in PascalCase: just ensures first char is upper

**Error scenarios**: If transformed name doesn't match JSON key during decoding, field is treated as missing.

### Feature: Constructor Name Transformation (`transformConstructorNames`)

**Description**: Transforms ADT constructor names in discriminator values or wrapper keys.

**Test cases**:
- Snake case: `ConfigExampleFoo` -> `config_example_foo`
- Screaming snake: `ConfigExampleFoo` -> `CONFIG_EXAMPLE_FOO`
- Kebab case: `ConfigExampleFoo` -> `config-example-foo`
- Pascal case: `configExampleFoo` -> `ConfigExampleFoo` (uppercase first char)

**Corner cases**:
- Tested with discriminator field: `"type" -> "config_example_foo"`
- Tested without discriminator (wrapper): `"config_example_foo" -> { ... }`
- Enum-as-string: `NorthEast` -> `"north_east"` (snake), `"SOUTH_EAST"` (screaming), `"south-west"` (kebab)

**Error scenarios**:
- JSON with original (untransformed) name fails when transforms are active:
  ```scala
  // With withSnakeCaseConstructorNames active:
  {"ConfigExampleFoo": {...}} // FAILS
  // Error: "type ConfigExampleBase has no class/object/case named 'ConfigExampleFoo'."
  ```

### Feature: Default Values (`useDefaults`)

**Description**: When enabled, missing JSON fields use case class default parameter values.

**Test cases**:
- Field `a: Int = 0` missing from JSON -> decoded as `0`
- Encoder still includes default values in output
- Decoding `{"thisIsAField": "x", "b": 2.5}` succeeds when `a` has default `0`

**Corner cases -- Option[T] with defaults (extensively tested)**:
1. `Option[T]` **without** default, null value -> `None`
2. `Option[T]` **without** default, missing key -> `None`
3. `Option[T]` **with** default `Some(0)`, null value -> `None` (null overrides default)
4. `Option[T]` **with** default `Some(0)`, missing key -> `Some(0)` (default used)
5. Non-Option field **with** default, null value -> default value used (e.g., `b: String = "b"`, `null` -> `"b"`)
6. `Option[T]` with default, wrong type in JSON -> `DecodingFailure` (NOT default fallback)
7. Non-Option field with default, wrong type in JSON -> `DecodingFailure` (NOT default fallback)

**Important behavioral distinction**: Null vs. missing key:
- **Missing key**: use default if available, else `None` for `Option`, else error
- **Null value for Option**: always `None` (overrides default)
- **Null value for non-Option with default**: use default value
- **Wrong type**: always error (never falls back to default -- fixed in later versions, was a bug)

**Generic classes with defaults**:
```scala
case class GenericFoo[T](a: List[T] = List.empty, b: String = "b")
// Decoding {} -> GenericFoo(List.empty[Int], "b") -- works correctly
```

### Feature: Discriminator Field (`discriminator`)

**Description**: When set, ADT constructor name is stored as a field in the JSON object instead of as a wrapper key.

**Without discriminator** (default -- wrapper object):
```json
{"ConfigExampleFoo": {"thisIsAField": "x", "a": 0, "b": 2.5}}
```

**With discriminator** `"type"`:
```json
{"type": "ConfigExampleFoo", "thisIsAField": "x", "a": 0, "b": 2.5}
```

**Test cases**:
- Encoding adds discriminator field to the JSON object
- Decoding reads discriminator field to determine constructor
- Works with constructor name transforms: `"type": "config_example_foo"`
- Case objects encode as `{"type": "ConfigExampleBar"}`

**Corner cases**:
- Missing discriminator field -> error: `"ConfigExampleBase: could not find discriminator field 'type' or its null."`
- Null discriminator value -> same error
- Invalid constructor name in discriminator -> error: `"type ConfigExampleBase has no class/object/case named 'invalid-name'."`
- Discriminator field name clashes with case class field name (Issue #239 -- surprising behavior)
- Encoder only adds discriminator when encoder is `Encoder.AsObject` (manual `contramap` encoders silently skip discriminator)

**Error messages tested**:
```scala
// Missing/null discriminator:
"ConfigExampleBase: could not find discriminator field 'type' or its null."
// ops: List(DownField("type"))

// Invalid constructor name (no discriminator):
"type ConfigExampleBase has no class/object/case named 'invalid-name'."
// ops: List(DownField("invalid-name"))
```

### Feature: Strict Decoding (`strictDecoding`)

**Description**: When enabled, rejects JSON with fields not expected by the target type.

**Test cases for sum types (without discriminator)**:
- JSON object with more than one key -> error:
  ```
  "Strict decoding ConfigExampleBase - expected a single key json object
   with one of: ConfigExampleFoo, ConfigExampleBar."
  ```
  ops: `List()` (no cursor movement)

**Test cases for product types**:
- JSON with extra fields -> error:
  ```
  "Strict decoding ConfigExampleFoo - unexpected fields: anotherField;
   valid fields: thisIsAField, a, b."
  ```
  ops: `List(DownField("ConfigExampleFoo"))`

**Error accumulation with strict decoding**:
- `decodeAccumulating` does NOT fail-fast on unexpected fields -- it accumulates both:
  1. Strict decoding failure (unexpected fields)
  2. Field-level decoding failures (e.g., wrong type)
  ```scala
  Validated.invalid(NonEmptyList(
    strictFailure("unexpected fields: invalidField"),
    List(DecodingFailure("Double", List(DownField("b"), DownField("ConfigExampleFoo"))))
  ))
  ```

**Historical issues**:
- Issue #72: `StrictCaseClassConfiguredDecoder` stopped after first unexpected field -- fixed by using `Decoder#ensure` instead of `Decoder#validate`
- The fix allows accumulation of ALL unexpected fields in a single error

### Feature: Enumeration (Enum-as-String) Derivation

**Description**: Derives codecs that encode/decode sealed traits of case objects as plain JSON strings.

**circe-generic-extras** (Scala 2): `deriveEnumerationCodec[CardinalDirection]`
- `North` -> `"North"`, `South` -> `"South"`, etc.

**circe core** (Scala 3): `ConfiguredEnumCodec.derived[IntercardinalDirections]`
- Works with Scala 3 `enum` types
- Works with sealed trait hierarchies of case objects

**Test cases**:
- Round-trip encode/decode for all directions
- Constructor name transforms: `NorthEast` -> `"north_east"` (snake), `"SOUTH_EAST"` (screaming), `"south-west"` (kebab)
- Invalid case name -> error: `"enum IntercardinalDirections does not contain case: NorthNorth"`

**Corner cases**:
- Non-singleton enum cases are **rejected at compile time**:
  ```
  error: Enum "WithNonSingletonCase" contains non singleton case "NonSingletonCase"
  ```
- Hierarchical sealed traits (nested sub-traits, diamond inheritance) work correctly
- `ConfiguredEnumCodec` encodes to JSON string (not object), decodes from JSON string

### Feature: Unwrapped (Value Class) Derivation

**Description**: Derives codecs for value classes that unwrap to the inner type in JSON.

**Test cases** (from `UnwrappedSemiautoDerivedSuite`):
- `case class Foo(value: String)` with `deriveUnwrappedCodec`
- Encoding: `Foo("hello")` -> `"hello"` (JSON string, not object)
- Decoding: `"hello"` -> `Foo("hello")`
- Wrong type error: `42` -> `DecodingFailure` with "string" type expectation

### Feature: Combined Configuration Options

**Description**: Multiple configuration options can be composed.

**Test cases**:
```scala
Configuration.default
  .withSnakeCaseMemberNames
  .withDefaults
  .withDiscriminator("type")
  .withKebabCaseConstructorNames
```
Produces:
```json
{
  "type": "config-example-foo",
  "this_is_a_field": "hello",
  "a": 0,
  "b": 2.5
}
```
Decoding omits `"a"` field (has default), encoding always includes it.

### Feature: Hierarchical Sealed Traits

**Description**: Multi-level sealed trait hierarchies are supported.

**Test cases (from ConfiguredDerivesSuite)**:
1. 2-level hierarchy with discriminator:
   ```scala
   sealed trait GrandParent
   sealed trait Parent extends GrandParent
   case class Child(a: Int, b: String) extends Parent
   ```
   Encodes/decodes correctly with `"type"` discriminator.

2. 3-level hierarchy where a parent's sibling has a field with same name as a child type:
   ```scala
   sealed trait GreatGrandParent
   sealed trait GrandParent extends GreatGrandParent
   case class Uncle(Child: Int) extends GrandParent // field name = "Child"
   sealed trait Parent extends GrandParent
   case class Child(a: Int, b: String) extends Parent
   ```
   Works correctly -- doesn't confuse field name "Child" with type name "Child".

3. Recursive types:
   ```scala
   sealed trait Tree
   case class Branch(l: Tree, r: Tree) extends Tree
   case object Leaf extends Tree
   ```

### Feature: `@ConfiguredJsonCodec` Macro Annotation

**Description**: Auto-generates encoder/decoder for annotated type at compile time.

**Variants**:
- `@ConfiguredJsonCodec` -- both encoder and decoder
- `@ConfiguredJsonCodec(encodeOnly = true)` -- encoder only
- `@ConfiguredJsonCodec(decodeOnly = true)` -- decoder only

**How it works**: Expands to calls to `deriveConfiguredEncoder`, `deriveConfiguredDecoder`, or `deriveConfiguredCodec`.

### Feature: `@JsonKey` Override

**Description**: Override the JSON field name for a specific case class parameter.

**Usage**:
```scala
case class Foo(@JsonKey("myField") b: Double)
```
JSON key becomes `"myField"` regardless of `transformMemberNames` configuration.

**Notes**: Takes precedence over `transformMemberNames`.

### Feature: `@JsonNoDefault` (circe-derivation only)

**Description**: Forces a field to be required even when `useDefaults = true`.

**Usage**:
```scala
case class Foo(@JsonNoDefault a: Int = 0, b: String = "b")
```
When `useDefaults = true`: field `a` is still required in JSON, field `b` uses default.

### Feature: `incomplete` Decoder (DerivationHelper)

**Description**: Derives a decoder for a case class where some constructor parameters are provided upfront.

**Usage pattern**:
```scala
val decoder: Decoder[Int => Foo] = deriveConfiguredFor[Int => Foo].incomplete
```
Creates a decoder that reads partial JSON and returns a function that takes the remaining parameters.

### Feature: `patch` Decoder (DerivationHelper)

**Description**: Derives a decoder that returns `A => A` -- a function that patches an existing instance.

**Usage pattern**:
```scala
val patchDecoder: Decoder[Foo => Foo] = deriveConfiguredFor[Foo].patch
```
Reads JSON patch and returns function that updates matching fields of an existing instance.

---

## ADT Encoding Modes (Without vs With Discriminator)

### Mode 1: Wrapper Object (default, no discriminator)

Sum type encoded as single-key JSON object where key = constructor name:

```json
// sealed trait Foo; case class Bar(x: Int) extends Foo; case object Baz extends Foo
{"Bar": {"x": 42}}     // Bar(42)
{"Baz": {}}             // Baz
```

Decoding: first key is tried as constructor name.

**Non-strict mode**: Extra keys in the outer object are ignored:
```json
{"extraField": true, "Class1": {"int": 3}}  // OK, decoded as Class1(3)
```

**Strict mode**: Only a single key allowed in the outer object.

### Mode 2: Discriminator Field

Sum type encoded as flat object with extra discriminator field:

```json
// with .withDiscriminator("type")
{"type": "Bar", "x": 42}     // Bar(42)
{"type": "Baz"}               // Baz
```

**Important behavioral nuance**: Discriminator is only added when the encoder is `Encoder.AsObject`. If using `Encoder.instance` + `contramap`, the discriminator is silently omitted.

---

## Edge Cases Tested

### 1. Empty case class decoding
- `deriveConfiguredDecoder[EmptyCc]` only accepts JSON objects (not arrays, strings, etc.)
- JSON string -> `DecodingFailure(WrongTypeExpectation("object", json))`

### 2. Non-object JSON for sum types
- JSON string for configured sum type -> `DecodingFailure(WrongTypeExpectation("object", json))`
- JSON string for configured product type -> same error

### 3. ADT decoding with invalid wrapper names
- `{"invalid-name": {...}}` -> `DecodingFailure("type ConfigExampleBase has no class/object/case named 'invalid-name'.")`
- Same error for `decodeAccumulating`

### 4. Superfluous keys in ADT wrapper (non-strict)
- `{"extraField":true,"Class1":{"int":3}}` -> successfully decoded as `Class1(3)`
- `{"Class1":{"int":3},"extraField":true}` -> successfully decoded as `Class1(3)`
- Multiple extra keys work: `{"extraField":true,"extraField2":15,"Class1":{"int":3}}` -> OK

### 5. Option[T] behavior matrix

| JSON value | Has default? | Default value | Result |
|---|---|---|---|
| missing key | no | n/a | `None` |
| missing key | yes | `Some(0)` | `Some(0)` |
| `null` | no | n/a | `None` |
| `null` | yes | `Some(0)` | `None` (null overrides default) |
| wrong type | no | n/a | `DecodingFailure` |
| wrong type | yes | `Some(0)` | `DecodingFailure` (not default fallback) |

### 6. Non-Option field with default behavior matrix

| JSON value | Has default? | Default value | Result |
|---|---|---|---|
| missing key | yes | `"b"` | `"b"` (default used) |
| `null` | yes | `"b"` | `"b"` (default used) |
| wrong type | yes | `"b"` | `DecodingFailure` (not default fallback) |

### 7. Nested case classes that cannot be derived
- `case class Quux(baz: Box[Baz])` where `Baz` has no codec in scope -> compile error
- Nested ADTs with non-companion subtypes -> compile error

### 8. Enum with non-singleton cases
- Compile-time rejection: `Enum "WithNonSingletonCase" contains non singleton case "NonSingletonCase"`

### 9. Hierarchical enums with diamond inheritance
- `case object D extends NestedA, NestedB` -- works correctly with `ConfiguredEnumCodec`

### 10. Long enums and long products
- `DerivesSuite` tests enum with 33 cases and case class with 33 fields -- both work

### 11. Recursive types
- `sealed trait Tree; case class Branch(l: Tree, r: Tree) extends Tree; case object Leaf extends Tree`
- Works with discriminator

### 12. Generic types with defaults
- `case class GenericFoo[T](a: List[T] = List.empty, b: String = "b")` -- defaults work

### 13. Tagged types
- `ProductWithTaggedMember` testing phantom type tagged types -- works

### 14. Constructor name collision with field name
- Case class `Uncle(Child: Int)` where `Child` is also a type in the hierarchy -- resolved correctly

---

## Error Handling / Guards

### Error message format

**Sum type -- invalid constructor (no discriminator)**:
```
"type ConfigExampleBase has no class/object/case named 'invalid-name'."
cursor ops: List(DownField("invalid-name"))
```

**Sum type -- invalid constructor (with discriminator)**:
Same message format but cursor ops include `DownField("type")`.

**Sum type -- missing discriminator**:
```
"ConfigExampleBase: could not find discriminator field 'type' or its null."
cursor ops: List(DownField("type"))
```

**Sum type -- strict decoding, multiple keys**:
```
"Strict decoding ConfigExampleBase - expected a single key json object
 with one of: ConfigExampleFoo, ConfigExampleBar."
cursor ops: List()
```

**Product type -- strict decoding, unexpected fields**:
```
"Strict decoding ConfigExampleFoo - unexpected fields: anotherField;
 valid fields: thisIsAField, a, b."
cursor ops: List(DownField("ConfigExampleFoo"))
```

**Enum -- invalid case**:
```
"enum IntercardinalDirections does not contain case: NorthNorth"
cursor ops: List()
```

**Non-object input**:
```
WrongTypeExpectation("object", <json value>)
cursor ops: List()
```

**Type mismatch**:
```
"Int"  -- for expected Int, got string
"Double" -- for expected Double, got string
WrongTypeExpectation("string", <json value>) -- for expected String, got number
cursor ops: List(DownField("fieldName"))
```

### Error accumulation

Both `decodeAccumulating` and normal `decode` are tested. Key difference:
- `decode`: fails fast on first error
- `decodeAccumulating`: collects ALL errors into `Validated.Invalid(NonEmptyList(...))`
- Strict decoding + type errors accumulated together (not fail-fast)

### Compile-time guards

- Missing codec for nested type: compile error mentioning the missing type
- Non-singleton enum cases: compile error
- Nested case classes without companion codecs: compile error

---

## Known Issues / Workarounds

### 1. Option default value ignored (Issue #878)
**Problem**: With `useDefaults = true`, `Option[T]` with default `Some(x)` returned `None` when key was missing.
**Fix**: Later versions distinguish between missing key and null value. Missing key uses default, null value forces `None`.

### 2. Defaults used on decode failure (Issue #1024)
**Problem**: When `useDefaults = true` and a field has wrong type, the default was silently used instead of reporting an error.
**Fix**: Default values should ONLY be used when the key is missing, not when decoding fails.

### 3. Null-valued fields and defaults (Issue #1142)
**Problem**: In 0.12.0-M1, null-valued fields no longer used defaults.
**Resolution**: This was intentional -- null explicitly means "no value", different from "key not present".

### 4. Discriminator behavior with manual encoders (Issue #239)
**Problem**: Discriminator only added when encoder is `Encoder.AsObject`. Manual `contramap` encoders silently skip discriminator.
**Resolution**: Documented as "working as expected" but confusing. Use `Encoder.AsObject` derivatives.

### 5. StrictCaseClassConfiguredDecoder stops on first error (Issue #72)
**Problem**: Strict decoder used `Decoder#validate` which doesn't accumulate errors.
**Fix**: Changed to `Decoder#ensure` to allow error accumulation.

### 6. dropNullValues not supported in Configuration (Issue #323)
**Problem**: No way to configure encoder to drop null values at derivation time.
**Workaround**: Use `Printer.spaces2.copy(dropNullValues = true)` at serialization time.
**Status**: Open / help wanted.

### 7. Discriminator inconsistent in Scala 3 (Issue #2281)
**Problem**: `Configuration` with discriminator works with `ConfiguredCodec.derived` but not always with `derives ConfiguredCodec` syntax.
**Status**: Known issue.

### 8. `knownDirectSubclasses` ordering issues (Scala 2)
**Problem**: Shapeless macro `knownDirectSubclasses` fails depending on file compilation order.
**Workaround**: Alphabetical file ordering, import enum subclasses before materialization, semi-auto derivation.

### 9. Stack overflow on large structures
**Problem**: Generic derivation can overflow the compiler stack for deeply nested types.
**Workaround**: Increase compiler stack size with `-J-Xss64m`.

### 10. Precision loss in Scala.js
**Problem**: `scalajs.js.JSON` parses numbers into floating-point, losing precision for `BigDecimal`.

---

## Type Hierarchy of Codec Types

### circe-generic-extras
```
Decoder[A]
  |-> ExtrasDecoder[A]     (adds isStrict, decodeStrict, StrictResult)

Codec.AsObject[A]
  |-> ExtrasAsObjectCodec[A]  (= Codec.AsObject[A] with ExtrasDecoder[A])
```

### circe core (Scala 3)
```
Decoder[A]
  |-> ConfiguredDecoder[A]
  |-> ConfiguredEnumDecoder[A]

Encoder[A]
  |-> Encoder.AsObject[A]
    |-> ConfiguredEncoder[A]
  |-> ConfiguredEnumEncoder[A]

Codec[A]
  |-> Codec.AsObject[A]
    |-> ConfiguredCodec[A]  (extends ConfiguredDecoder[A] & ConfiguredEncoder[A])
  |-> ConfiguredEnumCodec[A]
```

### SumOrProduct marker trait
Both `ConfiguredDecoder` and `ConfiguredEncoder` in circe core mix in `SumOrProduct` which provides:
- `isSum: Boolean` -- whether this is a sum type or product type

This is used internally to determine encoding strategy (flat vs wrapped for nested sums).

---

## Feature Comparison: circe-generic-extras vs circe core vs Kindlings

| Feature | circe-generic-extras (Scala 2+3) | circe core (Scala 3 only) | Kindlings (Scala 2+3) |
|---|---|---|---|
| `transformMemberNames` | Yes | Yes | Yes |
| `transformConstructorNames` | Yes | Yes | Yes |
| `useDefaults` | Yes | Yes (needs `-Yretain-trees`) | Yes |
| `discriminator` | Yes | Yes | Yes |
| `strictDecoding` | Yes | Yes | Yes |
| `@JsonKey` / `@fieldName` | Yes (`@JsonKey`) | No | Yes (`@fieldName`) |
| `@JsonNoDefault` / `@transientField` | No (in circe-derivation) | No | Yes (`@transientField`) |
| Enum-as-string | Yes (`deriveEnumerationCodec`) | Yes (`ConfiguredEnumCodec`) | Yes (`enumAsStrings`) |
| Unwrapped value class | Yes (`deriveUnwrappedCodec`) | No (use basic derivation) | ? |
| `incomplete` decoder | Yes (via `DerivationHelper`) | No | No |
| `patch` decoder | Yes (via `DerivationHelper`) | No | No |
| `@ConfiguredJsonCodec` annotation | Yes | No (uses `derives`) | No |
| `dropNullValues` in config | No (use Printer) | No | ? |
| `ExtrasDecoder` (structured strict errors) | Yes | No | ? |

---

## Test Data Models Used

### circe-generic-extras test models

```scala
// ADT
sealed trait ConfigExampleBase
case class ConfigExampleFoo(thisIsAField: String, a: Int = 0, b: Double) extends ConfigExampleBase
case object ConfigExampleBar extends ConfigExampleBase

// Option defaults
case class FooWithDefault(a: Option[Int] = Some(0), b: String = "b")
case class FooNoDefault(a: Option[Int], b: String = "b")

// Inner types
case class InnerExample(a: Double)

// Value class
case class Foo(value: String) extends AnyVal  // for unwrapped

// Enumerations
sealed trait CardinalDirection
case object North extends CardinalDirection
case object South extends CardinalDirection
case object East extends CardinalDirection
case object West extends CardinalDirection

sealed trait Mary
case object HadA extends Mary
case object LittleLamb extends Mary

// Extended enumeration (includes non-singleton)
sealed trait ExtendedCardinalDirection
case object North2 extends ExtendedCardinalDirection
// ...
case class NotACardinalDirectionAtAll(x: String) extends ExtendedCardinalDirection
```

### circe core test models (Scala 3)

```scala
// ADT as Scala 3 enum
enum ConfigExampleBase:
  case ConfigExampleFoo(thisIsAField: String, a: Int = 0, b: Double)
  case ConfigExampleBar

// Enum-as-string
enum IntercardinalDirections:
  case NorthEast, SouthEast, SouthWest, NorthWest

// Hierarchical enum
sealed trait HierarchicalEnum
object HierarchicalEnum:
  sealed trait NestedA extends HierarchicalEnum
  sealed trait NestedB extends HierarchicalEnum
  case object A extends HierarchicalEnum
  case object B extends NestedA
  case object C extends NestedB
  case object D extends NestedA, NestedB  // diamond

// Recursive
sealed trait Tree
case class Branch(l: Tree, r: Tree) extends Tree
case object Leaf extends Tree

// Multi-level hierarchy
sealed trait GrandParent
sealed trait Parent extends GrandParent
case class Child(a: Int, b: String) extends Parent

// Generic with defaults
case class GenericFoo[T](a: List[T] = List.empty, b: String = "b")
```
