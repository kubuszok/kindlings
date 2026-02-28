# YAML Gap Analysis

## Upstream Library
- Library: [VirtusLab scala-yaml](https://github.com/VirtusLab/scala-yaml) v0.3.1
- Relationship to Kindlings: Kindlings **reimplements** derivation using Hearth macros, while depending on scala-yaml's runtime types (`Node`, `YamlEncoder`, `YamlDecoder`, `ConstructError`, `LoadSettings`)
- scala-yaml provides its own derivation (Scala 3 `derives YamlCodec`), but Kindlings replaces it entirely with cross-compiled (Scala 2.13 + 3) macro-based derivation that adds many features scala-yaml's built-in derivation lacks

## Upstream (scala-yaml) Built-in Derivation Capabilities

scala-yaml's native derivation (via `Mirror`-based macros in Scala 3 only):
- Case classes (product types)
- Sealed traits / enums (sum types) -- tries each child decoder sequentially until one succeeds
- Default parameter values
- Primitives: Int, Long, Double, Float, Short, Byte, BigInt, BigDecimal, Boolean, String
- Collections: List, Seq, Set, Map
- Option
- Any (dynamic decoding)

**Not supported** by scala-yaml's built-in derivation:
- No discriminator field configuration
- No field name transformation (snake_case, kebab-case, etc.)
- No annotation-based field renaming
- No transient field support
- No `enumAsStrings` mode
- No constructor name transformation
- No configuration object at all
- Scala 2 derivation not supported (Scala 3 only via `derives`)
- No inline encode/decode methods (must create codec first)
- No syntax extensions (`.toYamlString`, `.fromYamlString`)

## Features Implemented in Kindlings

### Type Support (Encoder + Decoder)
- Primitive types: Int, Long, Double, Float, Short, Byte, Char, Boolean, String (via implicit summoning from scala-yaml)
- Case classes (including empty, single-field, nested, generic with type parameters)
- Sealed traits with wrapper-style encoding (default)
- Sealed traits with discriminator-style encoding (configurable)
- Case objects / singletons
- Scala `Enumeration` (via `enumAsStrings`)
- Java enums (via `enumAsStrings`, JVM-only)
- Scala 3 `enum` with fields
- Value classes (`extends AnyVal`) -- unwrapped encoding/decoding
- `Option[A]` -- null node for None
- Collections: List, Vector, Set (via Hearth's `IsCollection`)
- Maps with String keys (via Hearth's `IsMap`)
- Tuples (as named mappings: `_1`, `_2`, etc.)
- Named tuples (Scala 3.7+)
- Recursive types (self-referential case classes)
- Type aliases
- Higher-kinded types (`F[_]`)
- Opaque types (Scala 3)
- Literal types: String, Int, Long, Double, Boolean, Float, Short, Byte, Char (Scala 3, encoder + decoder with validation)
- Union types (Scala 3, using fully-qualified names)
- IArray (Scala 3)

### Configuration (`YamlConfig`)
- `transformMemberNames: String => String` -- field name transformation
- `transformConstructorNames: String => String` -- sealed trait/enum variant name transformation
- `discriminator: Option[String]` -- discriminator field for sealed traits
- `enumAsStrings: Boolean` -- encode/decode all-singleton enums as scalar strings
- `useDefaults: Boolean` -- use default values for missing fields during decoding
- Convenience methods: `withSnakeCaseMemberNames`, `withKebabCaseMemberNames`, `withPascalCaseMemberNames`, `withScreamingSnakeCaseMemberNames`, `withSnakeCaseConstructorNames`, `withKebabCaseConstructorNames`, `withDiscriminator`, `withEnumAsStrings`, `withUseDefaults`

### Annotations
- `@fieldName("custom_name")` -- override field name (takes priority over config transform)
- `@transientField` -- exclude field from encoding, use default during decoding (requires default value, compile-time error otherwise)
- UTF-8 / non-ASCII field names supported

### API Surface
- `KindlingsYamlEncoder.encode[A](value)` -- inline macro, returns `Node`
- `KindlingsYamlEncoder.toYamlString[A](value)` -- inline macro, returns `String`
- `KindlingsYamlEncoder.derive[A]` -- returns `YamlEncoder[A]`
- `KindlingsYamlEncoder.derived[A]` -- implicit/given, returns `KindlingsYamlEncoder[A]`
- `KindlingsYamlDecoder.decode[A](node)` -- inline macro, returns `Either[ConstructError, A]`
- `KindlingsYamlDecoder.fromYamlString[A](yaml)` -- inline macro, returns `Either[YamlError, A]`
- `KindlingsYamlDecoder.derive[A]` -- returns `YamlDecoder[A]`
- `KindlingsYamlDecoder.derived[A]` -- implicit/given, returns `KindlingsYamlDecoder[A]`
- `KindlingsYamlCodec.derive[A]` -- returns `KindlingsYamlCodec[A]`
- `KindlingsYamlCodec.derived[A]` -- implicit/given, returns `KindlingsYamlCodec[A]`
- `syntax.toYamlString` -- extension method on any `A`
- `syntax.fromYamlString[A]` -- extension method on `String`

### Debugging
- `LogDerivation` sentinel type for encoder and decoder (import or scalac setting)
- Detailed error messages listing which derivation rules were tried and why they failed
- Phantom type parameter inference guard (Nothing/Any detection)
- Compile-time error for unsupported types with actionable hints

### Cross-Compilation
- Full Scala 2.13 + Scala 3 support
- JVM, Scala.js, Scala Native platforms

## Features from Upstream NOT in Kindlings

### BigInt and BigDecimal
- scala-yaml has built-in `YamlDecoder[BigInt]` and `YamlDecoder[BigDecimal]` / `YamlEncoder[BigInt]` and `YamlEncoder[BigDecimal]`
- Kindlings does not test these but they should work via implicit summoning since scala-yaml provides them
- **Status**: Likely works automatically (untested)

### `Any` / Dynamic Decoding
- scala-yaml has `YamlDecoder.forAny` that dynamically decodes YAML nodes to the most precise Scala type
- Kindlings does not provide or test dynamic decoding
- **Status**: Not applicable -- Kindlings is focused on typed derivation

### NaN / Infinity Handling
- scala-yaml's decoder handles special float values like NaN, Infinity, -Infinity
- Kindlings does not explicitly test these
- **Status**: Should work via implicit summoning (untested)

### Precise Float/Double Decoders
- scala-yaml provides `forDoublePrecise` and `forFloatPrecise` variants
- Not exposed through Kindlings
- **Status**: Can be used via explicit implicit, not part of derivation

### `LoadSettings` Support
- scala-yaml's `YamlDecoder.construct` takes an implicit `LoadSettings` parameter
- Kindlings always passes `LoadSettings.empty`
- **Status**: Not configurable through Kindlings derivation

## Test Coverage Analysis

### Tests Present

**Encoder tests** (`KindlingsYamlEncoderSpec.scala` -- 32 tests):
- Primitives: Int, String, Boolean, Double, Long
- Case classes: simple, empty, single-field, nested
- Value classes (unwrapped)
- Options: Some, None
- Collections: List (ints, empty, case classes), Vector, Set (single, empty)
- Maps: Map[String, Int] (single entry, empty)
- Sealed traits: wrapper-style, discriminator-style
- String enums: case objects as strings, with transform, enumAsStrings=false fallback
- Scala Enumeration: encode as string, all values, with transform
- Recursive types: tree structure
- Configuration: custom constructor transform, snake_case, kebab-case, PascalCase, SCREAMING_SNAKE_CASE
- Tuples: 2-tuple, 3-tuple
- Generic case classes: Box[Int], Pair[String, Int]
- Deeply nested: 3-level nesting
- Type aliases
- Higher-kinded types
- Combined configuration: snake_case + discriminator + constructor transform
- Derive/derived API
- Custom implicit priority
- UTF-8 field names
- Annotations: @fieldName, @transientField, both combined, round-trip

**Decoder tests** (`KindlingsYamlDecoderSpec.scala` -- 38 tests):
- Primitives: Int, String, Boolean, Double, Long
- Case classes: simple, empty, single-field, nested, with List
- Options: Some, None from null
- Collections: List (ints, empty), Vector
- Value classes (unwrapped)
- Sealed traits: wrapper-style, discriminator-style, unknown discriminator error
- String enums: decode from string, all cases, with transform, non-scalar error, unknown value error
- Scala Enumeration: decode from string, all values, with transform
- Sets: ints, empty
- Configuration: custom constructor transform, snake_case, kebab-case
- Derive/derived API
- Custom implicit priority
- Maps: Map[String, Int], empty
- Recursive types: tree
- Tuples: 2-tuple, 3-tuple
- Generic case classes: Box[Int], Pair[String, Int]
- Deeply nested: 3-level nesting
- Type aliases
- Combined configuration: snake_case + discriminator + constructor transform
- Empty class with non-mapping input
- Error handling: missing required field, wrong type for field
- Higher-kinded types: List, Option
- UTF-8 field names
- useDefaults: missing field with default, provided overrides default, required still required, without useDefaults is error, all defaults
- Annotations: @fieldName, overrides config, @transientField, ignores if present, combined
- Edge cases: null for non-Option, fields in different order, useDefaults + enumAsStrings

**Round-trip tests** (`RoundTripSpec.scala` -- 15 tests):
- Case classes: simple, empty, single-field
- Value classes
- Sealed traits: Circle, Rectangle, Dog/Cat with discriminator
- Scala Enumeration with enumAsStrings
- Sets
- Tuples: 2, 3
- Generic case classes: Box, Pair
- Deeply nested
- Type aliases
- Configuration: constructor transform, snake_case
- KindlingsYamlCodec: simple, sealed trait with discriminator, derived, with annotations

**Inline methods tests** (`InlineMethodsSpec.scala` -- 10 tests):
- toYamlString: simple case class, custom config
- fromYamlString: success, invalid YAML
- syntax.toYamlString: simple, custom config
- syntax.fromYamlString: success, invalid YAML
- Round-trip: toYamlString then fromYamlString, syntax version
- Compile-time errors: unhandled type (encoder), unhandled type (decoder), Nothing type parameter

**JVM-only tests** (`KindlingsYamlEncoderJvmSpec.scala`, `KindlingsYamlDecoderJvmSpec.scala`, `RoundTripJvmSpec.scala` -- 7 tests):
- Java enum encode/decode/round-trip with enumAsStrings and name transform

**Scala 3-only tests** (`YamlScala3Spec.scala` -- 22 tests):
- Scala 3 enums: encoding (wrapper, discriminator, constructor transform), decoding (wrapper, discriminator, constructor transform)
- Opaque types: standalone encode/decode, case class with opaque field
- Literal types: String/Int/Boolean encoding, decoding, wrong value validation
- Named tuples: encoding (simple, nested, member transform), decoding (simple, nested, member transform)
- IArray round-trip
- Union types: String|Int encoding/decoding, case class union member
- Auto-derivation isolation: encoder/decoder use kindlings derivation (constructor/member transforms applied)

### Missing Test Cases

1. **Non-String map keys**: Kindlings only supports `Map[String, V]` (intentional limitation since YAML keys are strings). No tests verify that non-String key maps are rejected or produce errors.

2. **Mutable collections**: No tests for `ArrayBuffer`, `mutable.Set`, `mutable.Map`, etc. The circe module tests `WithMutableBuffer` but the YAML module does not. These may work via Hearth's `IsCollection` but are untested.

3. **SortedMap / SortedSet / TreeMap / TreeSet**: No tests for ordered collections.

4. **Sealed trait with mixed case class and case object children**: Tests have either all-case-object or all-case-class sealed traits, but no test verifies a sealed trait with both (e.g., `case object None extends Option` + `case class Some(x) extends Option`). The Animal/Shape tests are all case classes. CardinalDirection is all case objects.

5. **Multi-level sealed trait hierarchies**: No tests for sealed traits extending other sealed traits.

6. **Very large case classes**: No tests for case classes with many fields (10+).

7. **Option[Option[A]]**: Not tested (nested optionality).

8. **Map[String, Option[V]]**: Not tested.

9. **Collection of sealed traits**: e.g., `List[Shape]` is not tested directly (though `TeamWithMembers` tests `List[SimplePerson]`).

10. **Empty string field values**: Not tested whether empty strings are preserved vs treated as null.

11. **YAML-specific scalar types**: No tests for YAML anchors/aliases, multi-line strings, or special YAML constructs.

12. **BigInt / BigDecimal**: Available via scala-yaml implicits but untested.

### Missing Corner Cases

1. **Duplicate keys in mapping**: No test for behavior when YAML input has duplicate keys.

2. **Extra fields in input**: No test verifying that extra/unknown fields in the YAML input are silently ignored (or cause errors).

3. **Null node within a collection**: e.g., `[1, null, 3]` decoded as `List[Int]`.

4. **Deeply nested recursive types**: Current recursive test is 3 levels; no stress test for deeper nesting.

5. **Empty Option field in case class**: `case class Foo(x: Option[String])` decoded from `{}` (field entirely missing vs null).

6. **Config interactions**: No test for `useDefaults + discriminator`, `useDefaults + @transientField`, `useDefaults + @fieldName`.

7. **Constructor with multiple parameter lists**: Not tested (though Hearth may handle this).

8. **Case class extending a trait with fields**: Not tested.

9. **Subtype ambiguity in enum decoding**: When two enum branches could match the same YAML structure (wrapper-style, no discriminator).

## Implementation Notes

### Architecture
- Kindlings YAML follows the same architectural pattern as Kindlings Circe and Jsoniter: separate encoder/decoder/codec macros mixing in `MacroCommons`, `StdExtensions`, and `AnnotationSupport`
- The codec (`KindlingsYamlCodec`) is a simple combination: derives encoder and decoder separately, then wraps them into a single trait. In Scala 2 this uses a macro; in Scala 3 it delegates inline to `KindlingsYamlEncoder.derive` + `KindlingsYamlDecoder.derive`
- Runtime helpers live in `YamlDerivationUtils`, which handles node construction/destruction, discriminator logic, collection encoding/decoding, and the codec combiner

### Key Design Decisions
1. **Maps require String keys**: Unlike Circe (which supports `KeyEncoder`/`KeyDecoder`), YAML naturally uses string keys, so only `Map[String, V]` is supported
2. **`LoadSettings.empty` always**: Kindlings does not pass through scala-yaml's `LoadSettings`, using empty settings for all decoding
3. **`summonExprIgnoring` pattern**: To avoid infinite macro expansion / OOM when scala-yaml's own `derives` method is in scope, the derivation explicitly ignores `KindlingsYamlEncoder.derived` / `KindlingsYamlDecoder.derived` during implicit search
4. **Enum encoding strategy**: All-singleton sealed traits (or Scala Enumerations / Java enums) can be encoded as plain strings via `enumAsStrings`; mixed sealed traits always use wrapper or discriminator style
5. **`@transientField` requires defaults**: Enforced at compile time for both encoder and decoder, preventing runtime NPEs
6. **Null handling**: `nodeNull` is created by encoding `None` through scala-yaml's own `YamlEncoder.forOption`, ensuring compatibility with scala-yaml's null representation

### Differences from Other Kindlings Modules
- Unlike circe-derivation, YAML does not have `strictDecoding` configuration (rejecting unknown fields)
- Unlike circe-derivation, YAML does not support non-String map keys (YAML's natural constraint)
- Unlike jsoniter-derivation, YAML does not have streaming/incremental encoding -- everything goes through the `Node` AST
- The YAML codec in Scala 3 uses a simpler approach than jsoniter (direct inline delegation rather than macro-level LambdaBuilder combination), which is possible because YAML's codec is a pure combination of encoder + decoder without shared state
