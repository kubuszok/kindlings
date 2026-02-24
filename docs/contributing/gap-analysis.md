# Gap Analysis: Kindlings vs Circe / Jsoniter Scala / Avro4s

## Methodology

Compared test coverage across:
- **Circe**: `circe-generic` + `circe-generic-extras` (derivation modules)
- **Jsoniter Scala**: `jsoniter-scala-macros` (codec derivation)
- **Avro4s**: `avro4s-core` (schema/encoder/decoder derivation)
- **Kindlings**: All 6 modules (446 tests at time of analysis)

Focus is on **derivation-relevant** functionality — what types/patterns can be derived and how configuration affects them — not on core parsing/serialization (that's the underlying library's job, not Kindlings').

## Current Kindlings Test Coverage Summary

| Module | Tests | Categories Covered |
|--------|-------|--------------------|
| Circe Derivation | 107 | Primitives, case classes, value classes, options, collections, maps, sealed traits (wrapper + discriminator), recursive, sets, singletons, config (4 member name transforms + constructor names), derive/derived API, custom implicit priority, compile errors, Scala 3 enums/opaque types, useDefaults, strictDecoding, error handling |
| Jsoniter Derivation | 59 | Case classes, value classes, options, collections, maps, sealed traits (wrapper + discriminator), recursive, sets, config (4 member + constructor names + skipUnexpectedFields), derive/derived, custom implicit priority, inline methods/syntax, compile errors, Scala 3 enums, codec extensions (map/mapDecode) |
| YAML Derivation | 99 | Same pattern as Circe — primitives, case classes, value classes, options, collections, maps, sealed traits, recursive, sets, config, derive/derived, custom implicit priority, inline methods/syntax, compile errors, Scala 3 enums, auto-derivation isolation |
| Avro Derivation | 90 | Primitives (11 types including Byte, Short, Char, Array[Byte], BigDecimal), case classes, value classes, options, collections, maps, sealed traits (case objects as ENUM, case classes as UNION), config (namespace, field name transforms), derive, sets, Scala 3 enums (simple + parameterized), binary + JSON round-trip |
| FastShowPretty | 57 | Primitives (12), value types, case classes, collections, maps, options, custom instances, edge cases, derive, compile errors, sealed traits, tuples, Seq, SortedMap, WrappedString, recursive, Scala 3 named tuples + enums |
| Jsoniter JSON | 34 | JSON AST round-trips, type checks, fold, JsonObject, JsonNumber, optics (field/index/composed/each), JsonPath |

---

## HIGH PRIORITY GAPS

### 1. Tuples (all JSON/YAML/Avro modules)

**What they test**: Circe and jsoniter both encode tuples as JSON arrays (`(1, "hello")` -> `[1, "hello"]`). Jsoniter tests Tuple2-Tuple22+. Avro4s tests Tuple2-Tuple5.

**Kindlings status**: FastShowPretty tests Tuple2/Tuple3. **No tuple tests in circe-derivation, jsoniter-derivation, yaml-derivation, or avro-derivation.**

**Action**: Add tuple encode/decode/round-trip tests to all 4 derivation modules.

### 2. Generic / Parameterized Case Classes

**What they test**: Circe tests `Box[A](a: A)`, `Qux[A](i: Int, a: A, j: Int)`, `Typed1[A]`, `Typed2[A, B]`. Jsoniter tests higher-kinded types `F[_]`, first-order types with type parameters, and generic ADTs. Avro4s tests generics with name mangling.

**Kindlings status**: **No tests for parameterized case classes anywhere.** This is a very common pattern.

**Action**: Add `case class Box[A](a: A)` and similar test types + tests across all modules.

### 3. Enumeration Encoding (case objects as plain strings)

**What they test**: Circe-generic-extras has `deriveEnumerationEncoder/Decoder` that encodes `case object Red` as `"Red"` (a plain string), not `{"Red": {}}`. Jsoniter supports `withDiscriminatorFieldName(None)` for case objects -> plain strings. Avro4s encodes sealed traits of case objects as ENUM (string symbols).

**Kindlings status**: Kindlings encodes case object singletons as `{"Red": {}}` in circe/yaml/jsoniter. **No option to encode them as plain strings.** This is a behavioral difference users will notice.

**Action**: This needs **implementation work** (not just tests). Consider adding a config option like `withEnumerationValues(true)` or `withStringEnums(true)` that encodes parameterless sealed trait children as plain strings. Then add tests.

### 4. Option + Missing Key vs Null Distinction

**What they test**: Circe-generic-extras specifically tests:
- `Option[T]` without default -> `None` if null, `None` if missing key
- `Option[T]` with default -> `None` if null, **uses default** if missing key
- Non-option with default -> **uses default** if null

**Kindlings status**: Tests `useDefaults` but doesn't test the null vs missing key distinction for Option fields with defaults.

**Action**: Add tests to circe-derivation decoder spec covering these combinations.

### 5. Arrays (Array[T])

**What they test**: Jsoniter extensively tests `Array[Int]`, `Array[Array[Int]]`, `Array[BigInt]`. Avro4s tests `Array[Byte]`, arrays of records.

**Kindlings status**: FastShowPretty plan mentions Array but **no Array tests in any JSON/YAML/Avro derivation module.**

**Action**: Add `Array[Int]` encode/decode tests to all derivation modules. Note: Array needs ClassTag — may need special macro handling.

### 6. Opaque Types (Scala 3) — jsoniter/yaml/avro modules

**What they test**: Jsoniter-scala tests opaque types (`Gram`, `Meter`, `Year`), arrays of opaque types, maps with opaque keys.

**Kindlings status**: Circe module tests opaque types. **Jsoniter, YAML, and Avro modules have no opaque type tests.**

**Action**: Add opaque type tests to jsoniter-derivation, yaml-derivation, and avro-derivation Scala 3 spec files.

### 7. Combined/Composed Configuration Tests

**What they test**: Circe-generic-extras tests snake_case members + defaults + discriminator + snake_case constructors all combined. Jsoniter tests various multi-config combinations.

**Kindlings status**: Tests each config option individually but **doesn't test configurations composed together** (e.g., useDefaults + strictDecoding + snake_case + discriminator all at once).

**Action**: Add combined configuration tests to circe and jsoniter modules.

### 8. Avro-specific: UUID, Date/Time Logical Types, BigDecimal as Decimal

**What avro4s tests**:
- UUID with logical type (schema, encode as Utf8 string, decode)
- LocalDate as DATE, LocalTime as TIME, LocalDateTime/Instant/Timestamp as various timestamp logical types
- BigDecimal as decimal logical type (encoded as bytes, not string)
- Either as union type

**Kindlings status**: Avro module maps BigDecimal to STRING. **No UUID, no date/time logical types, no Either-as-union, no BigDecimal-as-decimal-bytes.**

**Action**: This needs **implementation work**. Add primitive handlers for UUID, java.time types, and BigDecimal-as-decimal in the Avro macro. Then add tests.

### 9. Avro-specific: Default Values in Schema & Schema Evolution

**What avro4s tests**: Default values in Avro schema (so consumers with the schema can handle missing fields). Schema evolution tests (missing field uses Scala default or schema default).

**Kindlings status**: **No default value support in schema generation, no schema evolution tests.**

**Action**: Implementation work needed in AvroSchemaFor macro to emit default values. Then add tests.

### 10. Avro-specific: Annotations (@AvroDoc, @AvroName, @AvroNamespace)

**What avro4s tests**: Rich annotation support for documentation (`@AvroDoc`), name overrides (`@AvroName`), namespace control (`@AvroNamespace`), aliases (`@AvroAlias`), transient fields (`@AvroTransient`).

**Kindlings status**: Only tests `namespace` in AvroConfig. **No annotation support for doc, per-field name overrides, aliases, transient fields.**

**Action**: Implementation work needed. Define Kindlings annotations and read them in macros. Consider which are essential (probably @AvroName, @AvroNamespace, @AvroDoc at minimum).

---

## MEDIUM PRIORITY GAPS

### 11. Type Aliases

**What they test**: Jsoniter tests `type UserId = String` aliases used in case class fields, collection type arguments, and top-level codecs.

**Kindlings status**: No type alias tests in any module.

**Action**: Add test types using type aliases and verify derivation handles them.

### 12. Empty Case Class Decoder Rejects Non-Objects

**What they test**: Both circe and circe-generic-extras verify that `Decoder[EmptyClass]` rejects non-object JSON (e.g., `true`, `42`, `[]`).

**Kindlings status**: Tests `EmptyClass` encode/decode round-trip but doesn't verify rejection of non-object input.

**Action**: Add negative decode tests for empty case classes with non-object input.

### 13. Error Accumulation (Circe)

**What circe tests**: `decodeAccumulating` that collects multiple errors instead of failing fast.

**Kindlings status**: No error accumulation tests. May not be in scope if Kindlings doesn't add this API.

**Action**: Evaluate whether this is in scope. If circe's `decodeAccumulating` works with Kindlings-derived decoders, add a test verifying it.

### 14. Named Tuples (Scala 3) — JSON/YAML/Avro modules

**Kindlings status**: FastShowPretty tests named tuples. **No named tuple tests in JSON/YAML/Avro modules.**

**Action**: Add named tuple tests to Scala 3 spec files for all derivation modules.

### 15. Per-Field Name Override Annotation

**What they test**: Circe has `@JsonKey("myField")`. Jsoniter has `@named("myField")`. Both override the config-level name transform for specific fields.

**Kindlings status**: **No per-field name override support.** Configuration only applies globally.

**Action**: Implementation work needed. Define a `@fieldName("name")` or similar annotation, read it in macros. This is important for real-world usage where one field needs a different name.

### 16. Transient/Ignored Fields

**What they test**: Jsoniter has `@transient` to skip fields. Avro4s has `@AvroTransient`. Both require the field to have a default value.

**Kindlings status**: **No transient field support in any module.**

**Action**: Implementation work needed. Define a `@transient` annotation (or reuse `scala.transient`), read it in macros, skip field during encode/decode (require default value).

### 17. java.time Types as Fields in Derived Codecs

**What jsoniter tests**: Duration, Instant, LocalDate, LocalDateTime, LocalTime, MonthDay, OffsetDateTime, OffsetTime, Period, Year, YearMonth, ZonedDateTime, ZoneId, ZoneOffset — all with dedicated codecs.

**Kindlings status**: These are typically provided by the underlying library (jsoniter-scala-core), not derivation. **But Kindlings should verify they work as fields in derived codecs.**

**Action**: Add a test case class with `java.time.Instant` field and verify it round-trips through derivation (the underlying library provides the codec, but derivation needs to find it).

### 18. Recursive Types — compile-time error for non-opted-in

**What jsoniter tests**: Requires explicit opt-in (`withAllowRecursiveTypes(true)`) and tests compile-time error when not enabled.

**Kindlings status**: Tests recursive types work but doesn't test that they produce useful compile-time errors when problematic.

**Action**: Add `compileErrors` test for recursive types without opt-in (if applicable to Kindlings' design).

### 19. Deeply Nested Case Classes (3+ levels)

**What they test**: Avro4s tests Level1->Level2->Level3->Level4. Jsoniter tests multi-level ADT hierarchies.

**Kindlings status**: Tests 2-level nesting (PersonWithAddress). No 3+ level tests.

**Action**: Add 3-level nesting test (Person -> Address -> GeoCoordinates).

### 20. Sealed Trait with Non-Case-Class Leaves

**What they test**: Jsoniter tests sealed traits where some subtypes are plain classes (not case classes) with explicit codec.

**Kindlings status**: All sealed trait tests use case classes/objects only.

**Action**: Low priority. Kindlings may intentionally only support case classes as ADT leaves.

---

## LOWER PRIORITY GAPS

| Gap | Tested By | Notes |
|-----|-----------|-------|
| Java enums | Jsoniter, Avro4s | JVM-only; may not be in scope for cross-compiled library |
| Scala `Enumeration` | Jsoniter | Legacy type; low priority |
| Mutable collections | Jsoniter | `mutable.HashMap`, `ArrayBuffer`, etc. — usually not used in data models |
| `IntMap`/`LongMap`/`BitSet` | Jsoniter | Specialized collections |
| Literal types | Jsoniter | `"VVV"`, `true`, `42` as types |
| Union types (Scala 3) | Jsoniter | `String \| Int` — needs custom codec, not derivable |
| `IArray` (Scala 3) | Jsoniter | Immutable array |
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
| `withDiscriminatorFieldName(None)` | No discriminator (strings/wrappers) | Not supported |
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

Analysis performed: 2026-02-24
