# Gap Analysis: Kindlings vs Circe / Jsoniter Scala / Avro4s

## Methodology

Compared test coverage across:
- **Circe**: `circe-generic` + `circe-generic-extras` (derivation modules)
- **Jsoniter Scala**: `jsoniter-scala-macros` (codec derivation)
- **Avro4s**: `avro4s-core` (schema/encoder/decoder derivation)
- **Kindlings**: All 6 modules (446 tests at time of initial analysis, 1454 at last update)

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

### ~~Java enums~~ — RESOLVED

Java enum types are now fully supported across all 4 derivation modules. Hearth's `Enum.parse` identifies Java enums via `isJavaEnum` and returns `directChildren`. The `allCaseObjects` guard includes `Type[A].isJavaEnum`. Encoder skips recursive derivation for Java enum children. Decoder uses `singletonOf` to construct enum values.

Tested in **all 4 modules** (circe, yaml, jsoniter, avro): encoder (3 tests each), decoder (3 tests each for circe/yaml/avro), codec round-trip (jsoniter), binary round-trip (avro). Works on both Scala 2.13 and 3.

Required Hearth `0.2.0-233+` for `isJavaEnum` fix on Scala 3 and `singletonOf` fix for Java enum values on Scala 2.

### ~~Scala `Enumeration`~~ — RESOLVED

Scala `Enumeration` types (`object Foo extends Enumeration`) are now fully supported across all 4 derivation modules. The `allCaseObjects` guard includes `Type[A].isEnumeration`. Encoder skips recursive derivation for Enumeration children. A `Type[EnumCase].shortName` issue (returns "Value" instead of "Red") is worked around by looking up child names from `Enum.directChildren` via subtype checks.

Tested in **all 4 modules** (circe, yaml, jsoniter, avro): encoder (3 tests each), decoder (3 tests each for circe/yaml/avro), codec round-trip (jsoniter), binary round-trip (avro). Constructor name transforms work. Works on both Scala 2.13 and 3.

Required Hearth `0.2.0-233+` for `singletonOf` fix for Enumeration values on Scala 2 and `shortName` fix on both Scala versions.

### ~~Mutable collections~~ — RESOLVED (already works)

Hearth's `IsCollectionProviderForScalaCollection` handles any `Iterable` subtype with a `Factory` implicit, including `mutable.ArrayBuffer`, `mutable.HashMap`, etc. Tested in **circe-derivation** with `mutable.ArrayBuffer[Int]` round-trip (standalone and as case class field). Works on both Scala 2.13 and 3.

### ~~`IArray` (Scala 3)~~ — RESOLVED

Hearth's `IsCollectionProviderForIArray` handles `IArray[T]` on Scala 3. Encoder and decoder both work. Tested in **circe-derivation** Scala 3 spec: encode, decode, and round-trip.

**Workaround applied**: `IArray` is an opaque type in Scala 3, so Hearth's `IsValueTypeProviderForOpaque` matches it before `IsCollectionProviderForIArray`. All 9 `IsValueType` match sites across all modules have a `case _ if Type[A].isIArray =>` guard that skips the value type rule, allowing the collection rule to handle it correctly. The underlying Hearth bug should still be fixed in Hearth.

### ~~`IntMap`/`LongMap`/`BitSet`~~ — RESOLVED (already works)

`IntMap` and `LongMap` are `Map` subtypes handled by Hearth's `IsMap` providers. `BitSet` is an `Iterable[Int]` handled by `IsCollection`. No additional Kindlings work needed.

### ~~13. Error Accumulation (Circe)~~ — RESOLVED

Kindlings-derived `KindlingsDecoder[A]` now overrides `decodeAccumulating` with a macro-generated body that accumulates errors across case class fields. The approach uses a single cached def with a `failFast: Boolean` parameter — when `true`, fields fail fast with `Either`; when `false`, all fields are decoded and errors collected into `ValidatedNel`. Nested case classes accumulate recursively when the inner decoder also has `decodeAccumulating` (e.g., summoned implicits or `KindlingsDecoder.derived`). Non-case-class types (enums, collections, options, value types) wrap `apply` with `Validated.fromEither` for the accumulating path.

Runtime helpers added to `CirceDerivationUtils`: `sequenceDecodeResultsAccumulating`, `checkStrictDecodingAccumulating`, `checkIsObjectAccumulating`, `decodeFieldAccumulating`, `decodeFieldWithDefaultAccumulating`, `decoderFromFnWithAcc`.

Tested in **circe-derivation** (6 tests): multi-field error accumulation, valid input, nested case classes, single-field error, `derived` implicit with override, empty case class. Works on both Scala 2.13 and 3.

### ~~18. Recursive Types~~ — RESOLVED

Always allowed by design. Kindlings has no `allowRecursiveTypes` configuration — recursion is always permitted. Jsoniter Scala requires explicit opt-in (`withAllowRecursiveTypes(true)`) and produces a compile-time error when not enabled; Kindlings intentionally does not impose this restriction.

### ~~Higher-kinded types `F[_]`~~ — RESOLVED (already works)

Higher-kinded type case classes like `case class HigherKindedType[F[_]](value: F[Int])` work out of the box with all derivation modules. When instantiated with a concrete type constructor (e.g., `HigherKindedType[List]`, `HigherKindedType[Option]`), the macro sees fully applied types and derives normally. Tested in **circe** (encoder + decoder), **jsoniter** (round-trip), and **yaml** (encoder + decoder). Works on both Scala 2.13 and 3.

### ~~Sized, Partial/Patch decoding~~ — NOT APPLICABLE

`Sized[List[Int], Nat._4]` (circe-shapes) and partial/patch decoding (circe-generic-extras) are Shapeless-specific features with no equivalent in Hearth's macro-agnostic API.

### ~~Whitespace/Indentation (Jsoniter)~~ — NOT APPLICABLE

Writer configuration is handled by the jsoniter-scala library itself, not by derivation macros.

### ~~Kafka integration, Stream tests (Avro4s)~~ — OUT OF SCOPE

Not derivation-related.

---

---

## REMAINING LOWER PRIORITY GAPS

### Literal types — **Medium** (was estimated Low)

**Source:** Jsoniter Scala supports `"VVV"`, `true`, `42` as literal types in codecs.

**Analysis:** Literal type fields (e.g., `case class Tagged(tag: "hello", count: 42)`) do NOT work today. The derivation pipeline tries to summon `Encoder["hello"]` / `Decoder[42]` — these don't exist as implicits in circe or yaml. Jsoniter-scala's `JsonCodecMaker.make` handles them internally but those codecs aren't exposed as standalone implicits for Kindlings to summon.

**What's needed:**
- New derivation rule `HandleAsLiteralTypeRule` (or per-primitive variants) in each module
- Use Hearth's `TypeCodec` API to extract the constant value at compile time (`Type.valueOfConstant[A]`)
- Encoder: emit the constant value directly (ignore the runtime value — it's always the same)
- Decoder: read the value, validate it matches the literal, reject otherwise
- Scala 3 only (`LiteralCodec` is Scala 3; Scala 2 has no literal type syntax for case class fields)

**Difficulty revised to Medium:** Requires a new derivation rule (not just tests), and the rule needs cross-module implementation. Encoder is simple but decoder needs validation logic.

### Union types (Scala 3) — **Hard** (blocked on Hearth)

**Source:** Jsoniter Scala supports `String | Int` union type codecs.

**Analysis:** Hearth exposes NO union type introspection APIs — no `isUnion`, no `unionMembers`. Scala 3's `OrType` pattern match on `TypeRepr` is internal `scala.tasty.reflect` API, not the public `scala.quoted` API.

**What's needed:**
1. **Hearth upstream:** Add `isUnion[A: Type]: Boolean` and `unionMembers[A: Type]: List[Type[?]]`
2. **Kindlings:** New `HandleAsUnionRule` similar to sealed trait dispatch but without discriminator (try each member type, first success wins)
3. Scala 2: no-op (no union syntax)

**Blockers:** Requires Hearth changes first. Runtime erasure means no `Class`-based dispatch — would need try-parse fallback. Recommend filing Hearth issue before attempting.

### `@stringified` (numbers as strings) — **Medium**

**Source:** Jsoniter Scala's `withIsStringified(true)`.

**Analysis:** Would encode numeric fields as JSON strings (`42` → `"42"`) and decode strings back to numbers. Useful for APIs that transmit numbers as strings for precision.

**What's needed:**
- New annotation: `@stringified` (field-level, jsoniter-derivation only)
- Modify `EncHandleAsBuiltInRule` in `CodecMacrosImpl.scala`: when `@stringified` present on a numeric field, write via `writeVal(number.toString)` instead of `writeVal(number)`
- Modify corresponding decoder rule: read as string, parse to numeric type
- Add runtime helpers in `JsoniterDerivationUtils.scala`
- Read annotation via existing `AnnotationSupport.getAnnotationStringArg` pattern (but as marker, no args)

### Map as array encoding — **Medium**

**Source:** Jsoniter Scala's `withMapAsArray(true)`.

**Analysis:** Encode maps as `[[k1,v1],[k2,v2]]` instead of `{"k1":v1,"k2":v2}`. Useful for non-string keys (JSON objects require string keys).

**What's needed:**
- Config option: `JsoniterConfig.mapAsArray: Boolean`
- Modify `EncHandleAsMapRule` (~L720): conditional `writeArrayStart` + nested `[key, value]` arrays instead of `writeObjectStart`
- Modify `DecHandleAsMapRule` (~L1429): detect array input, parse pairs
- New runtime helpers in `JsoniterDerivationUtils.scala`

### UTF-8/special chars in field names — **Low** (just tests)

**Source:** Jsoniter Scala edge case testing.

**Analysis:** Almost certainly works already. Field names pass through `JsonWriter.writeKey(String)` which handles UTF-8 transparently. `JsonReader.readKeyAsString()` also handles UTF-8. The `@fieldName` annotation extracts string literals at compile time with no character restrictions.

**What's needed:** Just add tests with Unicode field names, emoji, CJK characters via `@fieldName`. No code changes expected.

### `@AvroFixed` — **High**

**Source:** Avro4s `@AvroFixed(size)`.

**Analysis:** Changes schema type from `BYTES` to `FIXED(size)`. Unlike other Avro annotations that mutate schema properties after construction, this changes which **type rule** is selected. Requires intercepting the type derivation logic itself.

**What's needed:**
- New annotation with integer parameter (extends `AnnotationSupport` to extract int literals, not just strings)
- Schema: `Schema.createFixed(name, doc, namespace, size)` instead of `Schema.create(Schema.Type.BYTES)`
- Encoder: validate byte array length matches fixed size
- Decoder: read fixed-size bytes
- Affects all 3 avro macro impls

### `@AvroProp` — **Medium**

**Source:** Avro4s `@AvroProp(key, value)`.

**Analysis:** Adds custom key-value metadata to schema or field. Pure schema mutation — no type rules affected.

**What's needed:**
- Annotation with two string parameters
- Extend `AnnotationSupport` to extract string pairs
- Call `schema.addProp(key, value)` / `field.addProp(key, value)` after construction
- Support both class-level and field-level
- Schema-only change (no encoder/decoder modifications)

### `@AvroError` — **Low**

**Source:** Avro4s `@AvroError`.

**Analysis:** Marker annotation that sets `isError=true` on RECORD schema (for Avro RPC protocol error types). One branch point in schema generation.

**What's needed:**
- Marker annotation (no parameters)
- Check `hasAnnotationType[avroError]` on the type
- Pass `isError=true` to `Schema.createRecord(name, doc, namespace, isError)` — currently hardcoded to `false`
- Schema-only change

### `@AvroAlias` — **Medium**

**Source:** Avro4s `@AvroAlias(aliases*)`.

**Analysis:** Adds old names as aliases for schema evolution (readers using old field/record names can still read new schemas). Needs varargs extraction.

**What's needed:**
- Annotation with varargs string parameter
- Extend `AnnotationSupport` to extract string arrays/varargs (currently only extracts single strings)
- Call `field.addAlias(alias)` / `schema.addAlias(alias)` after construction
- Support both field-level and type-level
- Schema-only change

### `@AvroSortPriority` — **Low**

**Source:** Avro4s `@AvroSortPriority(priority)`.

**Analysis:** Controls ordering of types in union schemas. Just a sort key.

**What's needed:**
- Annotation with integer parameter (same `AnnotationSupport` extension as `@AvroFixed`)
- Read priority from sealed trait children annotations
- Sort children by priority before creating union schema
- Schema-only change

### ByteBuffer encoding — **Low**

**Source:** Avro4s supports `java.nio.ByteBuffer` as a built-in type mapping to Avro BYTES.

**What's needed:**
- Add `java.nio.ByteBuffer` type check to the 3 avro type rule files (schema, encoder, decoder)
- Schema: `Schema.create(Schema.Type.BYTES)`
- Encoder: `ByteBuffer.array()` → bytes
- Decoder: bytes → `ByteBuffer.wrap(bytes)`
- Pure type rule extension, no annotations

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
Last updated: 2026-02-25 — Higher-kinded types confirmed working (all modules). Cache operation logging added. Error testing strengthened (avro compile-time, yaml/jsoniter runtime message validation). Gap #18 (recursive types) resolved. Difficulty estimates added for remaining low-priority gaps. IArray workaround still needed (Hearth bug not yet fully fixed).
