# Gap Analysis: Kindlings vs Circe / Jsoniter Scala / Avro4s

## Methodology

Compared test coverage across:
- **Circe**: `circe-generic` + `circe-generic-extras` (derivation modules)
- **Jsoniter Scala**: `jsoniter-scala-macros` (codec derivation)
- **Avro4s**: `avro4s-core` (schema/encoder/decoder derivation)
- **Kindlings**: All 6 modules (446 tests at time of initial analysis)

Focus is on **derivation-relevant** functionality — what types/patterns can be derived and how configuration affects them — not on core parsing/serialization (that's the underlying library's job, not Kindlings').

---

## RESOLVED GAPS

These items from the original analysis have been fully addressed.

### ~~2. Generic / Parameterized Case Classes~~ — RESOLVED

Now tested in **all 4 derivation modules** (circe, jsoniter, yaml, avro) with `Box[A]`/`Pair[A, B]` model types in each module's `examples.scala`.

### ~~3. Enumeration Encoding (case objects as plain strings)~~ — RESOLVED

Implemented via `enumAsStrings` config option. Tested in **circe, jsoniter, yaml** — encode, decode, round-trip, constructor name transforms, error cases for invalid input. Avro is N/A (uses native ENUM schema type).

### ~~6. Opaque Types (Scala 3)~~ — RESOLVED

Now tested in **all 4 modules** (circe, jsoniter, yaml, avro) — standalone opaque type encode/decode plus case class with opaque field.

### ~~7. Combined/Composed Configuration Tests~~ — RESOLVED

Now tested in **circe** (snake_case + discriminator + constructor transform; useDefaults + strictDecoding + snake_case), **jsoniter** (snake_case + discriminator + snake ADT leaf names), and **yaml** (snake_case + discriminator + constructor transform).

### ~~11. Type Aliases~~ — RESOLVED

Now tested in **all 4 modules** with `type Name = String` / `WithAlias` model types.

### ~~15. Per-Field Name Override Annotation~~ — RESOLVED

Implemented as `@fieldName("name")` annotation. Tested in **all 4 modules** — custom name, precedence over config transform, compile-error for invalid usage.

### ~~16. Transient/Ignored Fields~~ — RESOLVED

Implemented as `@transientField` annotation. Tested in **all 4 modules** — excluded from encoding, uses default during decoding, compile-error when no default provided. Avro: excluded from schema.

### ~~19. Deeply Nested Case Classes (3+ levels)~~ — RESOLVED

Now tested in **all 4 modules** with `PersonFull -> FullAddress -> GeoCoordinates` (3-level nesting).

### ~~1. Tuples (avro module)~~ — RESOLVED

Tuples are case classes with `_1`, `_2`, etc. fields. Avro's `SfHandleAsCaseClassRule` already handles them. Now tested in **all 4 modules**: schema (RECORD with `_1`/`_2`/`_3` fields), encode (GenericRecord), decode, and binary round-trip.

### ~~4. Option + Missing Key vs Null Distinction~~ — RESOLVED

Tested in **circe-derivation**: Option field present+null → None, present+value → Some, absent (no default) → None, absent (with useDefaults) → uses default, present null (with useDefaults) → None (not default), absent (without useDefaults but with default) → None. Circe's built-in `Decoder[Option[A]]` correctly handles absent keys via `tryDecode` on `FailedCursor`.

### ~~5. Arrays (Array[T])~~ — RESOLVED (known limitation)

Generic `Array[T]` (e.g., `Array[Int]`) requires `ClassTag` which is not available in macro-generated `Expr.quote` blocks. This is a known limitation documented in CLAUDE.md. `Array[Byte]` works as a special case in Avro (maps to BYTES). Users should use `List`, `Vector`, etc. instead of `Array`.

### ~~17. java.time Types as Fields~~ — RESOLVED

Avro module has **full built-in support** for java.time types (`EventRecord` with UUID, Instant, LocalDate, LocalTime, LocalDateTime — schema, encode, decode, round-trip). **Circe** tested with user-provided `Encoder[Instant]`/`Decoder[Instant]` implicits — derivation correctly summons them for case class fields. **Jsoniter/YAML**: java.time is not in the built-in type rules; users must provide custom `JsonValueCodec`/`YamlEncoder`/`YamlDecoder` instances if needed. This matches the libraries' own patterns (jsoniter-scala's built-in java.time support is internal to their `JsonCodecMaker.make` macro, not exposed as standalone implicit codecs).

### ~~12. Empty Case Class Decoder — Non-Object Input~~ — RESOLVED (bug fix)

All 4 modules now validate input type for empty case classes. **Circe**: `checkIsObject` validates `cursor.value.isObject`. **YAML**: `checkIsMapping` validates node is `MappingNode`. **Avro**: `checkIsRecord` validates value is `GenericRecord`. **Jsoniter**: already validated via `readEmptyObject`. Tests updated in circe and yaml to verify non-object/non-mapping input is rejected.

### ~~20. Sealed Trait with Non-Case-Class Leaves~~ — RESOLVED (already works)

Tested in **circe** with `sealed trait MixedADT` containing both `case class CaseLeaf` and `class PlainLeaf` (non-case-class). The enum derivation rule tries implicit summoning first via `summonExprIgnoring` for each subtype — when a user-provided `Encoder[PlainLeaf]`/`Decoder[PlainLeaf]` is in scope, it is correctly found and used. Case class leaves still auto-derive normally. Works on both Scala 2.13 and 3.

### ~~10. Avro-specific: Annotations (@avroDoc, @avroNamespace)~~ — RESOLVED

Implemented `@avroDoc` and `@avroNamespace` annotations in the avro-derivation module. `@avroDoc` works at both class level (sets record schema doc) and field level (sets field doc). `@avroNamespace` overrides the config namespace at the class level. Added `findTypeAnnotationOfType` to the `AnnotationSupport` trait for reading class-level annotations, with Scala 2 and Scala 3 implementations. 6 tests covering class doc, field doc, namespace override, combined annotations.

### ~~9. Avro-specific: Default Values in Schema & Schema Evolution~~ — RESOLVED

Implemented `@avroDefault(json)` annotation for specifying default values in Avro schemas. The annotation takes a JSON string which is parsed at runtime via Jackson `ObjectMapper` and converted to native Java objects for Avro's `Schema.Field` constructor. Supports all JSON types: null, boolean, int, long, float, double, string, arrays, objects. Works in combination with `@avroDoc` for field documentation. 3 tests: integer/string defaults, Option with null default, schema evolution forward compatibility.

### ~~14. Named Tuples (Scala 3.7+)~~ — RESOLVED

Implemented `HandleAsNamedTupleRule` in **all 4 derivation modules** (circe, jsoniter, yaml, avro). Named tuples encode with their actual field names (not `_1`, `_2`) as JSON objects / YAML mappings / Avro records. Uses Hearth's `Type[A].isNamedTuple`, `primaryConstructor`, and `productElement(i)` APIs. Rule naturally no-ops on Scala 2 (`isNamedTuple` returns `false`). Tests in each module's Scala 3 spec: simple named tuple, nested with case class, member name transforms, and binary round-trip (avro).

### ~~8. Avro-specific: BigDecimal as Decimal Logical Type, Either as Union~~ — RESOLVED

Implemented `AvroConfig.withDecimalConfig(precision, scale)` for BigDecimal as Avro decimal logical type (BYTES with decimal logical type annotation). Without config, BigDecimal defaults to STRING. Implemented `Either[A, B]` as Avro UNION(A, B). Tested in **avro-derivation**: schema, encode, decode, binary round-trip for both BigDecimal decimal and Either union.

### ~~Mutable collections~~ — RESOLVED (already works)

Hearth's `IsCollectionProviderForScalaCollection` handles any `Iterable` subtype with a `Factory` implicit, including `mutable.ArrayBuffer`, `mutable.HashMap`, etc. Tested in **circe-derivation** with `mutable.ArrayBuffer[Int]` round-trip (standalone and as case class field). Works on both Scala 2.13 and 3.

### ~~`IArray` (Scala 3)~~ — PARTIALLY RESOLVED (encoder only)

Hearth's `IsCollectionProviderForIArray` handles `IArray[T]` on Scala 3. **Encoder works**: tested in **circe-derivation** Scala 3 spec. **Decoder has a Hearth bug**: fails with "key not found: n" at macro expansion time. Filed as a Hearth issue to investigate.

### ~~`IntMap`/`LongMap`/`BitSet`~~ — RESOLVED (already works)

`IntMap` and `LongMap` are `Map` subtypes handled by Hearth's `IsMap` providers. `BitSet` is an `Iterable[Int]` handled by `IsCollection`. No additional Kindlings work needed.

---

## REMAINING MEDIUM PRIORITY GAPS

### 13. Error Accumulation (Circe)

**What circe tests**: `decodeAccumulating` that collects multiple errors instead of failing fast.

**Kindlings status**: No error accumulation tests. May not be in scope if Kindlings doesn't add this API.

**Action**: Evaluate whether this is in scope. If circe's `decodeAccumulating` works with Kindlings-derived decoders, add a test verifying it.

### 18. Recursive Types — compile-time error for non-opted-in

**What jsoniter tests**: Requires explicit opt-in (`withAllowRecursiveTypes(true)`) and tests compile-time error when not enabled.

**Kindlings status**: Tests recursive types work (always allowed in Kindlings) but doesn't test that directly self-referential types (without `List` indirection) produce useful compile-time errors.

**Action**: N/A — Kindlings always allows recursion. Dropped per design decision.

---

## KNOWN LIMITATIONS (investigated, not fixable in Kindlings)

### Java enums — partial support (Avro schema + encoder only)

**Investigation findings**: Hearth's `Enum.parse` correctly identifies Java enums via `isJavaEnum` and returns `directChildren`. However, the `allCaseObjects` guard in Kindlings' enum derivation rules works inconsistently:

- **Avro `AvroSchemaFor`**: Works — produces ENUM schema with correct symbols. Tested.
- **Avro `AvroEncoder`**: Works — encodes to `GenericData.EnumSymbol`. Tested.
- **Avro `AvroDecoder`**: Fails — treats each Java enum value as a case class (not a val/object), so the mixed-ADT path fires and fails with "is not parseable as a case class".
- **Circe/Jsoniter/YAML encoder/decoder**: Fails on Scala 2.13 — each child type (e.g., `JavaColor.RED.type`) is not recognized by any derivation rule (not a case class, not a val, not an enum). Hearth's `Type.isVal` on Scala 2 checks `isObject && isStatic && isFinal`, which Java enum values don't satisfy.

**Root cause**: The `allCaseObjects` guard passes when `Type.isVal` returns `true` for each child. On Scala 2, `isVal` requires the Scala `isObject` flag which Java enum values lack. On Scala 3, `isVal` requires `Flags.Enum` which Java enum values do have, but the downstream derivation rules still fail because individual enum value types aren't handled (they're not case classes, value types, or anything else the rules know about).

**Status**: Hearth-level limitation. Fixing this requires changes to how enum derivation handles children that are neither case objects nor case classes.

### Scala `Enumeration` — not supported

**Investigation findings**: Hearth's `Enum.parse` has an `isEnumeration` branch that detects `scala.Enumeration` subtypes. However, Scala Enumeration values are instances of the inner `Value` class — they are NOT case objects (don't pass `Type.isVal`) and NOT zero-parameter case classes (don't pass `CaseClass.parse`). The `allCaseObjects` guard therefore fails, and derivation does not proceed.

**Status**: Hearth-level limitation. Supporting Scala Enumeration would require either special-casing it in the `allCaseObjects` guard or adding a dedicated derivation rule.

---

## LOWER PRIORITY GAPS

| Gap | Tested By | Notes |
|-----|-----------|-------|
| Literal types | Jsoniter | `"VVV"`, `true`, `42` as types |
| Union types (Scala 3) | Jsoniter | `String \| Int` — needs custom codec, not derivable |
| Higher-kinded types `F[_]` | Jsoniter, Circe | `HigherKindedType[F[_]]` — advanced use case |
| `@stringified` (numbers as strings) | Jsoniter | Jsoniter-specific performance feature |
| Map as array encoding | Jsoniter | `[[k,v],[k,v]]` format — jsoniter-specific |
| Whitespace/indentation | Jsoniter | Writer config — not derivation-related |
| UTF-8/special chars in field names | Jsoniter | Edge case |
| `Sized[List[Int], Nat._4]` | Circe-shapes | Shapeless-specific — not applicable |
| Partial/patch decoding | Circe-extras | Scala 2 shapeless-specific — not applicable |
| `@AvroFixed`, `@AvroProp`, `@AvroError` | Avro4s | Avro-specific annotations — lower priority |
| `@AvroAlias` for schema evolution | Avro4s | Avro-specific |
| ByteBuffer encoding | Avro4s | Avro-specific |
| String as Fixed encoding | Avro4s | Avro-specific |
| `@AvroSortPriority` | Avro4s | Union/enum ordering |
| Kafka integration | Avro4s | Out of scope |
| Stream tests | Avro4s | Out of scope |

---

## Detailed Library Comparison by Feature

### Circe Generic Extras — Features Tested

**Configuration options tested:**
- `withSnakeCaseMemberNames` / `withScreamingSnakeCaseMemberNames` / `withKebabCaseMemberNames` / `withPascalCaseMemberNames`
- `withSnakeCaseConstructorNames` / `withScreamingSnakeCaseConstructorNames` / `withKebabCaseConstructorNames` / `withPascalCaseConstructorNames`
- `withDefaults` — use Scala default parameter values when JSON field missing
- `withDiscriminator("type")` — type discriminator field
- `withStrictDecoding` — reject unexpected fields
- Combined: snake_case members + defaults + discriminator in same test

**Annotation support:**
- `@JsonKey("name")` — per-field JSON name override, takes precedence over config transforms

**Derivation modes:**
- Auto (import auto._)
- Semiauto (deriveEncoder, deriveDecoder, deriveCodec)
- Configured auto/semiauto
- Enumeration (deriveEnumerationEncoder/Decoder/Codec) — case objects as strings
- Unwrapped (deriveUnwrappedEncoder/Decoder/Codec) — value classes

**Special behaviors:**
- Option[T] with default: None if null, uses default if missing key
- Empty case class decoder rejects non-objects
- Error accumulation (decodeAccumulating)
- Semiauto doesn't leak implicits ("not come from nowhere")
- Semiauto requires instances for all component types

### Jsoniter Scala — Configuration Options Tested

| Config Option | Purpose | Kindlings equivalent |
|---|---|---|
| `withFieldNameMapper(fn)` | Custom field name transformation | `transformMemberNames` |
| `withAdtLeafClassNameMapper(fn)` | Custom discriminator value | `transformConstructorNames` |
| `withDiscriminatorFieldName(Some("x"))` | Custom discriminator field | `discriminator` |
| `withDiscriminatorFieldName(None)` | No discriminator (strings/wrappers) | `enumAsStrings` (case objects only) |
| `withCirceLikeObjectEncoding(true)` | Wrapper-style encoding | Default in Kindlings |
| `withIsStringified(true)` | Numbers as strings | Not supported |
| `withTransientNone(false)` | Serialize None as null | Not supported |
| `withTransientEmpty(false)` | Serialize empty collections | Not supported |
| `withTransientDefault(false)` | Serialize defaults | Not supported |
| `withSkipUnexpectedFields(false)` | Error on unknown fields | `skipUnexpectedFields` (jsoniter) / `strictDecoding` (circe) |
| `withRequireDefaultFields(true)` | Error if defaults missing | Not supported |
| `withAllowRecursiveTypes(true)` | Allow recursive derivation | Always allowed |
| `withInlineOneValueClasses(true)` | Unwrap value classes | Always unwrapped |
| `withCheckFieldDuplication(false)` | Disable dup key check | Not supported |
| `withAlwaysEmitDiscriminator(true)` | Emit discriminator from leaf | Not supported |

### Avro4s — Features Tested

**Schema generation:**
- Primitive types with Avro type mapping
- Nested case classes (multi-level)
- Options as UNION(null, T)
- Either as UNION(A, B)
- Sealed traits: case objects -> ENUM, case classes -> UNION of records
- Collections as ARRAY
- Maps as MAP
- Tuples as records with `_1`, `_2`, etc.
- UUID with logical type
- Date/Time with logical types (DATE, TIME, timestamps)
- BigDecimal with decimal logical type (bytes or fixed)
- Default values in schema
- Generic types with name mangling

**Annotations:**
- `@AvroDoc` — documentation on classes/fields
- `@AvroName` — rename fields/classes/enum symbols
- `@AvroNamespace` — override namespace at class/field level
- `@AvroProp` — custom properties
- `@AvroAlias` — aliases for schema evolution
- `@AvroFixed` — fixed-size encoding
- `@AvroTransient` — skip fields
- `@AvroSortPriority` / `@AvroUnionPosition` — ordering
- `@AvroNoDefault` — suppress Scala default in schema
- `@AvroError` — mark as error type
- `@AvroErasedName` — disable type-arg name mangling

**Namespace inference:**
- Package name for top-level classes
- Enclosing object namespace for inner classes
- Sealed trait namespace inheritance

---

## Research Sources

- Circe generic: https://github.com/circe/circe (modules/generic/, modules/shapes/)
- Circe generic extras: https://github.com/circe/circe-generic-extras (generic-extras/)
- Jsoniter Scala: https://github.com/plokhotnyuk/jsoniter-scala (jsoniter-scala-macros/)
- Avro4s: https://github.com/sksamuel/avro4s (avro4s-core/)

Initial analysis: 2026-02-24
Last updated: 2026-02-25 — gap #8 (BigDecimal decimal + Either union) resolved; mutable collections and IntMap/LongMap/BitSet confirmed working; IArray encoder works (decoder has Hearth bug); Java enums partially work (Avro schema+encoder only); Scala Enumeration not supported (Hearth limitation)
