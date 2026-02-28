# Circe Gap Analysis: Upstream vs Kindlings

## Summary
- Features implemented: 12/17
- Features missing: 5
  - `incomplete` decoder (DerivationHelper)
  - `patch` decoder (DerivationHelper)
  - `@ConfiguredJsonCodec` macro annotation
  - `dropNullValues` in config
  - `ExtrasDecoder` / structured strict errors (`StrictResult`, `decodeStrict`)
- Test coverage gaps: 15+ specific scenarios (detailed below)

## Feature-by-Feature Comparison

---

### Feature: Member Name Transformation (`transformMemberNames`)
- **Upstream**: Transforms case class field names in JSON output/input. Supports snake_case, SCREAMING_SNAKE_CASE, kebab-case, PascalCase, and custom transforms. Uses regex-based approach for consecutive capitals (e.g., `HTMLParser` -> `html_parser`).
- **Kindlings status**: Implemented
- **Kindlings tests**:
  - `KindlingsEncoderSpec` lines 346-381: snake_case, kebab-case, PascalCase, SCREAMING_SNAKE_CASE for encoding
  - `KindlingsDecoderSpec` lines 397-420: snake_case, kebab-case, PascalCase, SCREAMING_SNAKE_CASE for decoding
  - `RoundTripSpec` lines 215-220: snake_case round-trip
- **Gap**:
  - **Implementation difference**: Kindlings uses a simple char-by-char approach (`Configuration.scala` lines 30-75) instead of upstream's regex-based approach. This means consecutive capitals are handled differently: upstream `HTMLParser` -> `html_parser`, Kindlings `HTMLParser` -> `h_t_m_l_parser`. The upstream research document (line 126) explicitly calls this out.
  - **Missing test**: No test for consecutive capital handling edge cases (e.g., `HTMLParser`, `XMLReader`, `getHTTPSResponse`).
  - **Missing test**: No test for single-character field names (`a`, `b`) with transforms (upstream tests `a` -> `A` for SCREAMING_SNAKE_CASE).
  - **Missing builder**: `withPascalCaseConstructorNames` and `withScreamingSnakeCaseConstructorNames` are missing from Configuration (upstream provides both).

### Feature: Constructor Name Transformation (`transformConstructorNames`)
- **Upstream**: Transforms ADT constructor names in discriminator values or wrapper keys. Tested with snake_case, SCREAMING_SNAKE_CASE, kebab-case, PascalCase. Error when original (untransformed) name is used with transforms active.
- **Kindlings status**: Implemented
- **Kindlings tests**:
  - `KindlingsEncoderSpec` lines 370-381: custom toLowerCase transform, snake_case constructor names
  - `KindlingsDecoderSpec` lines 214-219: custom toLowerCase constructor name transform
  - `CirceScala3Spec` lines 31-36, 60-65: custom toLowerCase for Scala 3 enums
  - `RoundTripSpec` lines 207-213: custom constructor name transform round-trip
- **Gap**:
  - **Missing test**: No test for screaming snake case constructor names (`ConfigExampleFoo` -> `CONFIG_EXAMPLE_FOO`).
  - **Missing test**: No test for kebab-case constructor names.
  - **Missing test**: No test for PascalCase constructor names.
  - **Missing test**: No test that original (untransformed) constructor name fails when transform is active (upstream explicitly tests that `{"ConfigExampleFoo": {...}}` fails when snake_case is on).
  - **Missing test**: No test for constructor name transform with enum-as-string mode (upstream tests `NorthEast` -> `"north_east"` etc.).
  - **Missing builder methods**: `withPascalCaseConstructorNames`, `withScreamingSnakeCaseConstructorNames` not in `Configuration.scala`.

### Feature: Default Values (`useDefaults`)
- **Upstream**: When enabled, missing JSON fields use case class default parameter values. Extensively tests Option[T] with defaults, null vs missing key semantics, and non-Option fields with defaults.
- **Kindlings status**: Implemented
- **Kindlings tests**:
  - `KindlingsDecoderSpec` lines 466-501: basic useDefaults (field missing, provided, all defaults, field without default still required, combined with strictDecoding)
  - `KindlingsDecoderSpec` lines 581-615: Option null vs absent key behavior (6 tests)
- **Gap**:
  - **Missing test**: Non-Option field with default + null value (upstream: `b: String = "b"` with `null` -> uses default `"b"`). Kindlings behavior for this case is not tested.
  - **Missing test**: Option[T] with default + wrong type (upstream: always error, not default fallback). Not tested in Kindlings.
  - **Missing test**: Non-Option field with default + wrong type (upstream: always error, not default fallback). Not tested in Kindlings.
  - **Missing test**: Generic case class with defaults (`GenericFoo[T](a: List[T] = List.empty, b: String = "b")` decoded from `{}`).
  - **Missing test**: Encoder still includes default values in output (upstream explicitly tests that encoding with `useDefaults` still emits the default value).

### Feature: Discriminator Field (`discriminator`)
- **Upstream**: When set, ADT constructor name stored as a field in the JSON object instead of wrapper key. Tested with case classes, case objects, and combined with constructor transforms.
- **Kindlings status**: Implemented
- **Kindlings tests**:
  - `KindlingsEncoderSpec` lines 243-250: discriminator encoding
  - `KindlingsDecoderSpec` lines 127-141: discriminator decoding, unknown discriminator error
  - `RoundTripSpec` lines 53-65: discriminator round-trip for Dog and Cat
  - `CirceScala3Spec` lines 22-29, 51-57: discriminator with Scala 3 enums
- **Gap**:
  - **Missing test**: Case object encoding with discriminator (upstream: `{"type": "ConfigExampleBar"}` for case objects).
  - **Missing test**: Missing discriminator field error (upstream: `"could not find discriminator field 'type' or its null."` with `DownField("type")` ops).
  - **Missing test**: Null discriminator value error.
  - **Missing test**: Invalid constructor name in discriminator field error.
  - **Missing test**: Discriminator field name clashing with case class field name (upstream Issue #239).
  - **Error message difference**: Kindlings uses "Unknown type discriminator: X. Expected one of: ..." while upstream uses "type ConfigExampleBase has no class/object/case named 'X'.". The Kindlings message is arguably clearer but differs from upstream.

### Feature: Strict Decoding (`strictDecoding`)
- **Upstream**: Rejects JSON with unexpected fields. For sum types without discriminator, rejects objects with more than one key. Error accumulation with strict decoding tested (both strict failure and field-level failures accumulated together).
- **Kindlings status**: Implemented (for products only)
- **Kindlings tests**:
  - `KindlingsDecoderSpec` lines 423-462: exact fields pass, unexpected fields fail, no strictDecoding allows extras, empty class strict, name transform with strict
  - `KindlingsDecoderSpec` line 497: strictDecoding combined with useDefaults
  - `RoundTripSpec` line 283: Codec.AsObject with strictDecoding
- **Gap**:
  - **Missing test**: Strict decoding for **sum types** without discriminator (upstream: "expected a single key json object with one of: ..."). Not tested and likely not implemented for wrapper-mode ADTs.
  - **Missing test**: Strict decoding for **sum types** with discriminator (unexpected fields in the inner object).
  - **Missing test**: Error accumulation with strict decoding (`decodeAccumulating` should accumulate BOTH strict decoding failure and field-level failures, not fail-fast).
  - **Missing test**: Multiple unexpected fields listed in error message.
  - **Missing test**: Strict decoding with discriminator field (discriminator field should be in the expected field set).
  - **Error message difference**: Kindlings: "Unexpected field(s): extra". Upstream: "Strict decoding ConfigExampleFoo - unexpected fields: anotherField; valid fields: thisIsAField, a, b." -- upstream message includes the type name and lists valid fields.

### Feature: Enumeration (Enum-as-String) Derivation
- **Upstream**: Derives codecs that encode/decode sealed traits of case objects as plain JSON strings. Supports constructor name transforms. Compile-time rejection of non-singleton cases.
- **Kindlings status**: Implemented (via `enumAsStrings` config flag rather than separate method)
- **Kindlings tests**:
  - `KindlingsEncoderSpec` lines 296-318: enumAsStrings encode for sealed trait, constructor name transform, enumAsStrings=false fallback
  - `KindlingsEncoderSpec` lines 322-338: Scala Enumeration with enumAsStrings
  - `KindlingsDecoderSpec` lines 157-185: enumAsStrings decode, constructor transform, non-string input failure, unknown value failure
  - `KindlingsDecoderSpec` lines 188-208: Scala Enumeration decoder
  - JVM-only: Java enum encode/decode with enumAsStrings
  - `CirceScala3Spec` lines 94-103: Scala 3 parameterless enum encode/decode
- **Gap**:
  - **Missing test**: Compile-time rejection when `enumAsStrings` is used with a sealed trait that has non-singleton cases (e.g., `case class NotACardinalDirectionAtAll(x: String) extends ExtendedCardinalDirection`). Upstream rejects this at compile time.
  - **Missing test**: Constructor name transform with enum-as-string (upstream: `NorthEast` -> `"north_east"` snake, `"SOUTH_EAST"` screaming, `"south-west"` kebab).
  - **Missing test**: Hierarchical sealed traits with enum-as-string (nested sub-traits, diamond inheritance).
  - **Design difference**: Upstream has separate derivation methods (`deriveEnumerationCodec`/`ConfiguredEnumCodec.derived`), Kindlings uses a config flag (`enumAsStrings`). This is arguably better UX but means the type system doesn't prevent mixing `enumAsStrings` with product-type derivation.

### Feature: Unwrapped (Value Class) Derivation
- **Upstream**: Derives codecs for value classes that unwrap to inner type in JSON. `Foo("hello")` -> `"hello"`. Wrong type error tested.
- **Kindlings status**: Implemented (as part of general derivation, not a separate method)
- **Kindlings tests**:
  - `KindlingsEncoderSpec` lines 62-65: value class unwrapped encoding
  - `KindlingsDecoderSpec` lines 103-107: value class unwrapped decoding
  - `RoundTripSpec` lines 31-36: value class round-trip
- **Gap**:
  - **Missing test**: Wrong type error for value class decoding (upstream: `42` -> `DecodingFailure` for `Foo(value: String)`).

### Feature: Combined Configuration Options
- **Upstream**: Multiple configuration options can be composed (e.g., snake_case members + defaults + discriminator + kebab-case constructors).
- **Kindlings status**: Implemented
- **Kindlings tests**:
  - `KindlingsEncoderSpec` lines 462-474: snake_case members + discriminator + constructor transform
  - `KindlingsDecoderSpec` lines 559-578: snake_case + discriminator + constructor transform; useDefaults + strictDecoding + snake_case
- **Gap**:
  - **Missing test**: A 4-way combination test matching upstream's exact scenario (snake_case members + defaults + discriminator + kebab-case constructors producing `{"type": "config-example-foo", "this_is_a_field": "hello", "a": 0, "b": 2.5}`).

### Feature: Hierarchical Sealed Traits
- **Upstream**: Multi-level sealed trait hierarchies tested (2-level, 3-level with field name collision, recursive types).
- **Kindlings status**: Partially implemented (recursive types work, basic 2-level likely works)
- **Kindlings tests**:
  - `KindlingsEncoderSpec` lines 254-270: recursive tree encoding
  - `KindlingsDecoderSpec` lines 364-381: recursive tree decoding
  - `RoundTripSpec` lines 301-308: recursive type round-trip
- **Gap**:
  - **Missing test**: Multi-level sealed trait hierarchy (e.g., `GrandParent` -> `Parent` -> `Child`).
  - **Missing test**: 3-level hierarchy where a parent's sibling has a field with same name as a child type (upstream Issue - `Uncle(Child: Int)` where `Child` is also a type).
  - **Missing test**: Recursive sealed trait types (upstream: `Tree`/`Branch`/`Leaf` as sealed trait, not just recursive case class).

### Feature: `@fieldName` / `@JsonKey` Override
- **Upstream**: Override JSON field name for specific case class parameter. Takes precedence over `transformMemberNames`.
- **Kindlings status**: Implemented (as `@fieldName`)
- **Kindlings tests**:
  - `KindlingsEncoderSpec` lines 479-498: @fieldName encoding, precedence over config, combined with @transientField
  - `KindlingsDecoderSpec` lines 666-691: @fieldName decoding, precedence over config, combined with @transientField
  - UTF-8 field name tests (both encoder and decoder)
- **Gap**: No significant gap. Testing is thorough.

### Feature: `@transientField` / `@JsonNoDefault`
- **Upstream**: `@JsonNoDefault` forces field to be required even when `useDefaults = true`. Different semantics from Kindlings' `@transientField` which excludes the field entirely.
- **Kindlings status**: Implemented (different semantics - `@transientField` excludes field from JSON entirely, using default value during decode)
- **Kindlings tests**:
  - `KindlingsEncoderSpec` lines 490-511: @transientField excludes from encoding, compile error without default
  - `KindlingsDecoderSpec` lines 677-711: @transientField uses default, ignores even if present in JSON, compile error without default
- **Gap**:
  - **Semantic difference**: Upstream's `@JsonNoDefault` makes a field required despite `useDefaults = true`. Kindlings' `@transientField` removes the field from JSON entirely. These are different features.
  - **Missing feature**: No equivalent to upstream's `@JsonNoDefault` that forces a field to be required.

### Feature: `incomplete` Decoder (DerivationHelper)
- **Upstream**: Derives a decoder for a case class where some constructor params are provided upfront, returning `Decoder[Int => Foo]`.
- **Kindlings status**: **Missing**
- **Kindlings tests**: None
- **Gap**: Entire feature is not implemented.

### Feature: `patch` Decoder (DerivationHelper)
- **Upstream**: Derives a `Decoder[A => A]` for partial updates to an existing instance.
- **Kindlings status**: **Missing**
- **Kindlings tests**: None
- **Gap**: Entire feature is not implemented.

### Feature: `@ConfiguredJsonCodec` Macro Annotation
- **Upstream**: Auto-generates encoder/decoder for annotated type at compile time with `encodeOnly`/`decodeOnly` variants.
- **Kindlings status**: **Missing** (by design -- Kindlings uses `KindlingsEncoder.derived` / `KindlingsDecoder.derived` / `KindlingsCodecAsObject.derived` instead)
- **Kindlings tests**: None
- **Gap**: Entire feature is not implemented. This is a design choice, not a bug.

### Feature: `dropNullValues` in Configuration
- **Upstream**: Not supported in Configuration; requires `Printer.spaces2.copy(dropNullValues = true)` at serialization time. Open issue (#323).
- **Kindlings status**: **Missing** (same as upstream)
- **Kindlings tests**: None
- **Gap**: Same gap as upstream. Not a Kindlings-specific issue.

### Feature: `ExtrasDecoder` / Structured Strict Errors
- **Upstream**: `ExtrasDecoder[A]` extends `Decoder[A]` with `isStrict`, `StrictResult[A]`, and `decodeStrict(c: HCursor)` for structured strict decoding results that include extraneous field names.
- **Kindlings status**: **Missing**
- **Kindlings tests**: None
- **Gap**: Kindlings handles strict decoding as a runtime check with error messages but does not expose a structured API for it.

### Feature: Error Accumulation (`decodeAccumulating`)
- **Upstream**: `decodeAccumulating` collects ALL errors. Strict decoding + type errors accumulated together.
- **Kindlings status**: Implemented
- **Kindlings tests**:
  - `KindlingsDecoderSpec` lines 733-799: accumulates errors across fields, returns Valid on correct input, accumulates nested errors, single error gives one error, derived decoder has decodeAccumulating override, empty class accumulating
- **Gap**:
  - **Missing test**: Error accumulation combined with strict decoding (upstream: both strict failure and field decoding failures accumulated together in `decodeAccumulating`).
  - **Missing test**: `decodeAccumulating` for enum/sum types.

### Feature: ADT Encoding Modes (Wrapper Object vs Discriminator)
- **Upstream**: Wrapper object mode (default) and discriminator field mode. Non-strict wrapper mode ignores extra keys. Strict wrapper mode rejects multiple keys.
- **Kindlings status**: Implemented
- **Kindlings tests**: Multiple tests for both modes
- **Gap**:
  - **Missing test**: Superfluous keys in wrapper object (non-strict mode) -- upstream tests `{"extraField":true,"Class1":{"int":3}}` succeeds. Kindlings uses `cursor.keys.flatMap(_.headOption)` in `decodeWrapped` which takes the first key, so extra keys are silently ignored -- but this is not tested.
  - **Missing test**: Strict decoding for wrapper-style ADTs (reject multiple keys in outer object).

---

## Missing Test Cases

These are specific test scenarios from upstream that Kindlings does not have:

### From ConfiguredDerivesSuite / SharedConfiguredAutoDerivedSuite:
1. **Consecutive capital handling**: `HTMLParser` -> snake_case (different algorithm, likely different result)
2. **Single-char field transforms**: `a` -> `A` (SCREAMING_SNAKE_CASE)
3. **Constructor transform error**: original name fails when transform is active
4. **Screaming snake constructor names**: `ConfigExampleFoo` -> `CONFIG_EXAMPLE_FOO`
5. **Kebab constructor names**: `ConfigExampleFoo` -> `config-example-foo`
6. **Missing discriminator field error**: specific error message and cursor ops
7. **Null discriminator value error**: same as missing
8. **Invalid constructor name in discriminator**: specific error message and ops
9. **Case object with discriminator**: `{"type": "ConfigExampleBar"}` (no fields)

### From Option/Default behavior matrix:
10. **Non-Option field null with default**: `b: String = "b"` with JSON `null` -> uses default `"b"`
11. **Option with default + wrong type**: `Option[Int] = Some(0)` with JSON `"string"` -> `DecodingFailure`
12. **Non-Option with default + wrong type**: `b: String = "b"` with JSON `42` -> `DecodingFailure` (not default)
13. **Generic type with defaults**: `GenericFoo[Int](a = List.empty, b = "b")` decoded from `{}`

### From Strict decoding:
14. **Sum type strict decoding (wrapper)**: multiple keys in outer object -> "expected a single key json object with one of: ..."
15. **Sum type strict decoding (discriminator)**: unexpected inner fields
16. **Error accumulation + strict**: both strict error and field errors collected in `decodeAccumulating`
17. **Multiple unexpected fields**: error lists all of them

### From Hierarchical types:
18. **Multi-level sealed hierarchy**: `GrandParent` -> `Parent` -> `Child` with discriminator
19. **3-level hierarchy with name collision**: `Uncle(Child: Int)` field vs `Child` type
20. **Recursive sealed trait**: `Tree` / `Branch` / `Leaf` as sealed trait with discriminator

### From Enum:
21. **Compile-time rejection of non-singleton enum cases**
22. **Diamond inheritance in hierarchical enum**: `case object D extends NestedA, NestedB`

### From Edge cases:
23. **Non-object JSON for sum types**: JSON string for configured sum type -> `WrongTypeExpectation("object", json)`
24. **Non-object JSON for product types**: JSON string for case class -> error (tested for empty class but not for non-empty)
25. **Long enum (33 cases)**: stress test for large enum derivation
26. **Long product (33 fields)**: stress test for large case class derivation
27. **Tagged types / phantom types**: `ProductWithTaggedMember` testing phantom-type-tagged types

---

## Missing Corner Cases

1. **Null-valued fields and defaults**: The behavior matrix for null vs missing key vs wrong type with defaults is only partially tested. Upstream has 6+ specific scenarios (see "Option[T] behavior matrix" and "Non-Option field with default behavior matrix" in upstream research).

2. **Encoder always includes defaults**: Upstream explicitly verifies that encoding always includes fields with default values even when `useDefaults` is enabled. Kindlings does not test this.

3. **Discriminator only added when encoder is `Encoder.AsObject`**: Upstream documents (Issue #239) that `Encoder.instance` + `contramap` silently skips discriminator. This is a subtle behavioral nuance.

4. **`knownDirectSubclasses` ordering**: Upstream documents Scala 2 issues with compilation order affecting subclass discovery. Not clear if Kindlings (using Hearth) has the same issue.

5. **Stack overflow on deeply nested types**: Upstream documents this. No stress test in Kindlings.

6. **Precision loss in Scala.js**: BigDecimal precision loss via `scalajs.js.JSON`. No Kindlings test for this.

---

## Implementation Differences

### 1. Case transformation algorithm
- **Upstream** (circe core `renaming.scala`): Regex-based with two patterns (`([A-Z]+)([A-Z][a-z])` and `([a-z\d])([A-Z])`).
- **Kindlings** (`Configuration.scala` lines 30-75): Simple char-by-char `isUpper` check.
- **Impact**: Different output for consecutive capitals. `HTMLParser`: upstream -> `html_parser`, Kindlings -> `h_t_m_l_parser`.
- **File**: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/circe-derivation/src/main/scala/hearth/kindlings/circederivation/Configuration.scala` lines 30-42

### 2. Error messages
- **Upstream wrapper mode**: "type ConfigExampleBase has no class/object/case named 'invalid-name'." with `ops: List(DownField("invalid-name"))`.
- **Kindlings wrapper mode**: "Unknown type discriminator: Unknown. Expected one of: Circle, Rectangle" with `ops` from cursor history.
- **File**: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/circe-derivation/src/main/scala/hearth/kindlings/circederivation/internal/runtime/CirceDerivationUtils.scala` line 119-123

- **Upstream strict decoding**: "Strict decoding ConfigExampleFoo - unexpected fields: anotherField; valid fields: thisIsAField, a, b."
- **Kindlings strict decoding**: "Unexpected field(s): extra"
- **File**: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/circe-derivation/src/main/scala/hearth/kindlings/circederivation/internal/runtime/CirceDerivationUtils.scala` lines 80-91

- **Upstream missing discriminator**: "ConfigExampleBase: could not find discriminator field 'type' or its null."
- **Kindlings missing discriminator**: Returns generic circe decoding failure from `cursor.downField(field).as[String]`.

### 3. Enum-as-string derivation approach
- **Upstream**: Separate derivation methods (`deriveEnumerationCodec`, `ConfiguredEnumCodec.derived`).
- **Kindlings**: Configuration flag `enumAsStrings: Boolean = false` on the shared `Configuration` class.
- **Impact**: Kindlings approach is simpler but doesn't provide compile-time guarantees that the type is an all-singletons enum when `enumAsStrings = true`.

### 4. Missing `decodeOptionField` null-with-default behavior
- **Upstream**: Distinct handling for Option[T] fields with defaults: null -> `None` (overrides default), missing key -> uses default.
- **Kindlings**: The `decodeCaseClassFields` method in `DecoderMacrosImpl.scala` handles defaults at the field level: if key is missing and `useDefaults`, use default. But the null-for-Option-overrides-default behavior depends on how Option fields are decoded -- needs verification. The `decodeOptionField` runtime helper does handle `null -> None`, but the interaction with `useDefaults` for Option-with-default fields may differ from upstream.

### 5. Wrapper-object decoding strategy
- **Upstream non-strict**: Extra keys in wrapper object are silently ignored (takes first matching key).
- **Kindlings**: `CirceDerivationUtils.decodeWrapped` takes `cursor.keys.flatMap(_.headOption)` -- silently takes first key. Behavior matches upstream non-strict but is not tested.
- **File**: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/circe-derivation/src/main/scala/hearth/kindlings/circederivation/internal/runtime/CirceDerivationUtils.scala` lines 100-111

### 6. Missing Configuration builder methods
- **Upstream**: Provides `withPascalCaseConstructorNames`, `withScreamingSnakeCaseConstructorNames`, `withoutDefaults`, `withoutDiscriminator`, `withoutStrictDecoding`.
- **Kindlings**: Only provides `withSnakeCaseConstructorNames` and `withKebabCaseConstructorNames`. Missing the others.
- **File**: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/circe-derivation/src/main/scala/hearth/kindlings/circederivation/Configuration.scala` lines 11-24

### 7. Additional Kindlings features not in upstream
- **`enumAsStrings`** config flag: Not present in upstream `Configuration`. Upstream uses separate derivation methods.
- **`@transientField`**: Different from upstream's `@JsonNoDefault` -- completely excludes field from JSON rather than forcing it to be required.
- **`KindlingsEncoder.encode`/`KindlingsDecoder.decode` inline methods**: Direct encode/decode without creating an intermediate Encoder/Decoder instance.
- **Named tuple support**: Scala 3.7+ named tuples handled natively.
- **Union type support**: Scala 3 union types (`A | B`) handled as enum-like dispatch.
- **Literal type support**: `42`, `"hello"`, `true` literal types encoded/decoded as their literal values.
- **IArray support**: Scala 3 IArray handled as a collection.
- **Opaque type support**: Scala 3 opaque types handled transparently.
- **Non-String map key derivation**: Built-in key codec derivation for Int, Long, Double, Short, Byte, value types, and enums. Upstream requires user-provided KeyEncoder/KeyDecoder or uses circe's built-in ones.
