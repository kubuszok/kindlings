# Avro Gap Analysis: Upstream avro4s vs Kindlings avro-derivation

## Summary

| Category | Upstream avro4s | Kindlings Implemented | Coverage |
|---|---|---|---|
| Annotations (Scala) | 13 | 12 | 92% |
| Annotations (Java enum) | 4 | 0 | 0% |
| Primitive types | 9 | 11 (adds Char, Array[Byte]) | 100%+ |
| Logical types | 7 | 5 | 71% |
| Collection types | 5+ | 4 (List, Vector, Set, Array) | ~80% |
| Map support | Yes | Yes | 100% |
| Option support | Yes | Yes | ~80% |
| Either support | Yes | Yes | 100% |
| Sealed trait (enum) | Yes | Yes | 100% |
| Sealed trait (union) | Yes | Yes | ~90% |
| Value classes | Yes | Yes | 100% |
| Tuples | 2-5 | 2-3 tested | ~80% |
| Generic types | Yes (with name encoding) | Yes (erased names only) | Partial |
| Recursive types | 5 patterns | 1 defined, 0 tested | 20% |
| Shapeless Coproducts | Yes | No | 0% |
| Java enums | Yes | Yes | 100% |
| Scala Enumeration | Yes | Yes | 100% |
| Scala 3 enums | Partial | Via sealed trait | Partial |
| BigDecimal | 3 modes | 2 modes (string + decimal) | 67% |
| Schema evolution | Tested | Partially tested | ~30% |
| Stream I/O | 3 formats | Binary + JSON | 67% |
| Functional combinators | contramap/map/forType | None | 0% |
| Codec (combined) | Yes | No | 0% |
| Schema merge | Yes | No | 0% |
| TypeGuardedDecoding | Yes | No | 0% |
| Field mappers (named) | 4 named mappers | 3 via config (snake/kebab/custom) | ~75% |
| Error hierarchy | 4 exception types | 1 generic type | 25% |
| ToRecord/FromRecord | Yes | No (uses AvroIO) | Different design |

### Key Gaps

1. **No `@AvroUnionPosition` annotation** -- union ordering only via `@avroSortPriority`
2. **No `@AvroScalePrecision` annotation** -- decimal config is global via `AvroConfig`, not per-field
3. **No `OffsetDateTime` or `java.sql.Date`/`java.sql.Timestamp` support**
4. **No timestamp precision variants** (micros, nanos) -- only millis
5. **No recursive type tests** -- `RecursiveTree` is defined but never tested
6. **No Shapeless Coproduct support** (design choice -- Kindlings targets Scala 2.13+3)
7. **No `Codec[T]` combined typeclass**
8. **No functional combinators** (`contramap`, `map`, `forType`)
9. **No schema merge functionality**
10. **No `TypeGuardedDecoding` pattern**
11. **No `Data` format (embedded schema) I/O**
12. **Generic type names always erased** -- no type parameter encoding in schema names
13. **No Scala default value pickup** -- defaults only via `@avroDefault` annotation (JSON string)
14. **No field-level `@avroNamespace`** -- only type-level

---

## Annotation-by-Annotation Comparison

### `@AvroName` (upstream) vs `@fieldName` (Kindlings)

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Annotation name** | `@AvroName(name)` | `@fieldName(name)` | Different naming convention |
| **Target: fields** | Yes | Yes | None |
| **Target: classes** | Yes (overrides record name) | No | **Missing**: class-level name override |
| **Target: enum symbols** | Yes (overrides symbol name) | No | **Missing**: enum symbol name override |
| **Overrides field mapper** | Yes (Issue #396) | Yes (tested in `AvroSchemaForSpec` line 316) | None |
| **Implementation** | `AnnotationSupport.getAnnotationStringArg[fieldName]` | Same pattern | None |
| **Tests** | `AvroNameSchemaTest`, `AvroNameEncoderTest`, `AvroNameDecoderTest` | Schema/Encoder/Decoder/RoundTrip tested | Good coverage |

**File references:**
- Annotation: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/fieldName.scala`
- Schema usage: `SchemaForMacrosImpl.scala` line 715
- Encoder usage: `EncoderMacrosImpl.scala` line 823
- Decoder usage: `DecoderMacrosImpl.scala` line 892

**Missing test cases vs upstream:**
- Class-level `@AvroName` to rename the record itself
- `@AvroName` on enum case objects to rename symbols
- `@AvroName` name clash avoidance (when name equals enumeration name)

### `@AvroNamespace`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Target: classes** | Yes | Yes | None |
| **Target: fields** | Yes (overrides nested schema namespace) | No | **Missing** |
| **Field-level precedence** | Field overrides class | N/A | **Missing** |
| **Overrides config** | Yes | Yes (tested in `AvroSchemaForSpec` line 382) | None |

**File references:**
- Annotation: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/avroNamespace.scala`
- Schema usage: `SchemaForMacrosImpl.scala` line 661

**Missing test cases vs upstream:**
- Field-level `@avroNamespace` overriding nested record namespace
- Recursive namespace override in deeply nested schemas
- `@avroNamespace` on sealed trait subtypes in Either unions

### `@AvroDoc`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Target: classes** | Yes | Yes | None |
| **Target: fields** | Yes | Yes | None |
| **Tests** | `AvroDocSchemaTest` | `AvroSchemaForSpec` group "@avroDoc annotation" (line 356) | Good |

**File references:**
- Annotation: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/avroDoc.scala`

No significant gaps.

### `@AvroAlias` / `@avroAlias`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Target: classes** | Yes | Yes | None |
| **Target: fields** | Yes | Yes | None |
| **Multiple (stackable)** | Yes | Yes (tested in `AvroSchemaForSpec` line 666) | None |
| **Tests** | `AvroAliasSchemaTest` | Schema tests only | **Missing**: encoder/decoder/round-trip tests |

**File references:**
- Annotation: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/avroAlias.scala`

**Missing test cases:**
- Round-trip test using alias for schema evolution (reader with alias reads writer with old name)
- Alias on enum symbols

### `@AvroProp` / `@avroProp`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Target: classes** | Yes | Yes | None |
| **Target: fields** | Yes | Yes | None |
| **Multiple (stackable)** | Yes | Yes | None |
| **JSON value support** | Yes (`String | JsonNode`) | No (String only) | **Missing**: JSON node values |
| **Tests** | `AvroPropSchemaTest` | `AvroSchemaForSpec` group "@avroProp" (line 635) | Schema only |

**File references:**
- Annotation: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/avroProp.scala`

**Missing test cases:**
- JSON node value support in `@avroProp`
- Round-trip test preserving custom properties

### `@AvroTransient` / `@transientField`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Excludes from schema** | Yes | Yes | None |
| **Excludes from encoding** | Yes | Yes | None |
| **Uses default during decoding** | Yes | Yes | None |
| **Requires default value** | Implied | Yes (compile error if missing) | Better validation |
| **Tests** | `TransientSchemaTest`, `AvroTransientEncoderTest`, `TransientDecoderTest` | All three + round-trip tested | Good |

No significant gaps. Kindlings provides stricter validation.

### `@AvroNoDefault` / `@avroNoDefault`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Suppresses default** | Yes | Yes | None |
| **Conflict with @avroDefault** | Not documented | Compile error (tested line 427) | Better validation |
| **Tests** | `AvroNoDefaultTest` | Schema + round-trip tested | Good |

No significant gaps.

### `@AvroFixed` / `@avroFixed`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Target: fields** | Yes | Yes (Array[Byte] only) | None |
| **Target: value classes** | Yes | No | **Missing** |
| **Schema generation** | FIXED schema | FIXED schema | None |
| **Encode validation** | Not documented | Runtime size check (`AvroDerivationUtils.wrapByteArrayAsFixed`) | Better |
| **Non-Array[Byte] error** | Not documented | Compile error (tested line 567) | Better validation |
| **Tests** | `AvroFixedSchemaTest`, `FixedEncoderTest`, `FixedDecoderTest` | Schema + encode + decode + round-trip | Good |

**File references:**
- Annotation: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/avroFixed.scala`

**Missing:**
- `@avroFixed` on value class types (top-level fixed-length value types)

### `@AvroError` / `@avroError`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Marks schema as error** | Yes | Yes | None |
| **Tests** | `AvroErrorSchemaTest` | Schema test + round-trip | Good |

No significant gaps.

### `@AvroSortPriority` / `@avroSortPriority`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Enum symbol ordering** | Yes (higher = earlier) | Yes (lower = earlier, reversed convention) | **Different semantics** |
| **Union member ordering** | Yes (higher = earlier) | Yes (lower = earlier) | **Different semantics** |
| **Type: Int** | Float in upstream | Int in Kindlings | Minor difference |
| **Tests** | `AvroSortPrioritySchemaTest` | `AvroSchemaForSpec` group "@avroSortPriority" (line 610) | Schema only |

**Important semantic difference:** Upstream uses higher priority = earlier position; Kindlings sorts ascending (lower = earlier). The test at line 611-618 shows `PBeta(1)` first, `PGamma(2)` second, `PAlpha(3)` third, confirming ascending sort.

**Missing test cases:**
- Sort priority on union subtypes with mixed annotated/unannotated members
- Encoder and decoder tests for priority-ordered enums/unions

### `@AvroEnumDefault` / `@avroEnumDefault`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Sets default enum value** | Yes | Yes | None |
| **Tests** | `EnumSchemaCompatibilityTest` | Schema test + round-trip | Partial |

**File references:**
- Annotation: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/avroEnumDefault.scala`

**Missing test cases:**
- Schema evolution test: unknown enum symbol decoded using default (forward/backward compatibility)

### `@AvroErasedName` / `@avroErasedName`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Disables type param in name** | Yes | Annotation exists but is no-op (erased by default) | Inverted default |
| **Tests** | `GenericSchemaTest` | `ErasedBox` defined in examples but not tested | **Missing tests** |

**Important design difference:** Upstream encodes type parameters in generic names by default (e.g., `Generic__String`) and `@AvroErasedName` opts out. Kindlings always uses erased names (e.g., `Box` not `Box__Int`), so `@avroErasedName` is a documentation-only annotation.

**File references:**
- Annotation: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/avroErasedName.scala` (line 8 explicitly documents this)

### `@AvroDefault` / `@avroDefault`

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Source of defaults** | Scala constructor defaults (automatic) | Annotation only (`@avroDefault("json")`) | **Major difference** |
| **Format** | Scala values converted by `DefaultResolver` | Raw JSON string parsed at runtime | Different approach |
| **Tests** | `DefaultValueSchemaTest`, `DefaultValueRecordTest` | `AvroSchemaForSpec` group "@avroDefault" (line 400) | Partial |

**Major gap:** Upstream automatically picks up Scala default parameter values and converts them to Avro defaults via `DefaultResolver`. Kindlings requires explicit `@avroDefault("json")` annotation with the JSON representation. This means:
- Users must manually specify defaults as JSON strings
- No automatic `None -> null` default for Option fields
- No automatic conversion of Scala literals to Avro defaults
- No support for complex default values (sealed trait instances, BigDecimal, Instant, etc.)

**Missing test cases:**
- Automatic Scala default pickup (fundamentally different design)
- Nested defaults (`Outer(b: Inner = Inner())`)
- Default `None` for optional fields
- Complex defaults (temporal types, sealed trait case objects as defaults)

### `@AvroUnionPosition` -- **NOT IMPLEMENTED**

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Explicit position in union** | Yes | No | **Completely missing** |
| **Schema evolution support** | Critical for forward/backward compat | N/A | **Major gap** |
| **Tests** | `AvroUnionPositionSchemaTest`, `Github587` | N/A | N/A |

This is a significant gap for schema evolution scenarios involving sealed trait unions.

### `@AvroScalePrecision` -- **NOT IMPLEMENTED**

| Aspect | Upstream | Kindlings | Gap |
|---|---|---|---|
| **Per-field decimal config** | `@AvroScalePrecision(scale, precision)` | Global `AvroConfig.withDecimalConfig(precision, scale)` | **Different granularity** |
| **Tests** | `BigDecimalSchemaTest` | `AvroSchemaForSpec` group "BigDecimal as decimal logical type" (line 450) | Partial |

Kindlings uses a global `DecimalConfig` in `AvroConfig` rather than per-field annotation, which means all `BigDecimal` fields in a record share the same scale/precision.

### Java Enum Annotations -- **NOT IMPLEMENTED**

| Annotation | Upstream | Kindlings | Gap |
|---|---|---|---|
| `@AvroJavaName` | Yes | No | **Missing** |
| `@AvroJavaNamespace` | Yes | No | **Missing** |
| `@AvroJavaProp` | Yes | No | **Missing** |
| `@AvroJavaEnumDefault` | Yes | No | **Missing** |

Kindlings supports Java enums for schema/encode/decode but has no Java-side annotations for customization. The Java enum `JavaColor.java` is a plain enum without annotations.

---

## Type Support Comparison

### Primitives

| Type | Upstream | Kindlings | Gap |
|---|---|---|---|
| `Boolean` | int -> boolean | int -> boolean | None |
| `Int` | int | int | None |
| `Long` | long | long | None |
| `Float` | float | float | None |
| `Double` | double | double | None |
| `Byte` | int | int | None |
| `Short` | int | int | None |
| `String` | string (+ JavaString variant) | string only | **Missing**: `JavaStringSchemaFor` variant |
| `Char` | Not mentioned | string | Kindlings extra |
| `ByteBuffer` | bytes | bytes | None |
| `Array[Byte]` | bytes | bytes | None |

**Missing:**
- `JavaStringSchemaFor` (string with `"avro.java.string": "String"` property)
- `fixedStringSchemaFor(name, size)` (FIXED schema for strings)
- `Utf8` and `CharSequence` as standalone types

**Decoder type widening (upstream):** Long decoder accepts Byte/Short/Int/Long. Kindlings decoders use strict `asInstanceOf` casts -- no widening. This may cause runtime `ClassCastException` when Avro delivers a narrower type than expected.

### Collections

| Type | Upstream | Kindlings | Gap |
|---|---|---|---|
| `List[T]` | array | array | None |
| `Seq[T]` | array | array (via IsCollection) | None |
| `Set[T]` | array | array | None |
| `Vector[T]` | array | array | None |
| `Array[T]` (T != Byte) | array | array (via IsCollection) | None |

**Tested in Kindlings:** `List[Int]`, `Vector[String]`, `Set[Int]`, `List[SimplePerson]` (nested records)

**Missing test cases vs upstream:**
- `Seq[T]` explicitly tested
- `Array[T]` for non-Byte types
- Collections of tuples
- Collections of maps
- Nested collections (`List[List[Int]]`)
- Collections containing `Option[T]`

### Byte Collections

| Type | Upstream Schema | Kindlings | Gap |
|---|---|---|---|
| `Array[Byte]` | bytes | bytes | None |
| `List[Byte]` | bytes | **array of int** | **Different**: upstream special-cases, Kindlings treats as normal collection |
| `Seq[Byte]` | bytes | **array of int** | **Different** |
| `Vector[Byte]` | bytes | **array of int** | **Different** |
| `ByteBuffer` | bytes | bytes | None |

**Significant gap:** Upstream maps `List[Byte]`, `Seq[Byte]`, `Vector[Byte]` to Avro `bytes` type (same as `Array[Byte]`). Kindlings only special-cases `Array[Byte]` and `ByteBuffer`; other byte collections are treated as normal arrays of int.

### Maps

| Type | Upstream | Kindlings | Gap |
|---|---|---|---|
| `Map[String, V]` | map | map | None |
| Unicode keys | Tested | Not tested | **Missing test** |

**Missing test cases vs upstream:**
- `Map[String, Nested]` (record values)
- `Map[String, Option[Boolean]]` (nullable values)
- `Map[String, Seq[String]]` (nested collections in values)
- `Map[String, Seq[Nested]]` (nested records in collections in values)
- Unicode keys

### Options

| Pattern | Upstream | Kindlings | Gap |
|---|---|---|---|
| `Option[T]` -> `["null", T]` | Yes | Yes | None |
| `Option[Either[A,B]]` flatten | Yes | Not tested | **Missing test** |
| `Option[SealedTrait]` | Yes (flattened) | Not tested | **Missing test** |
| `None.type` -> `"null"` | Yes | Not tested | **Missing test** |
| Default `Some(x)` ordering | Yes (default type first) | N/A (no auto defaults) | **Gap** |
| Issue #883 (schema mutation) | Tested | Not tested | **Missing test** |
| Issue #885 (single subtype) | Tested | Not tested | **Missing test** |
| Issue #890 (nested None defaults) | Tested | Not tested | **Missing test** |

**Missing test cases:**
- `Option[SimplePerson]` (option of record)
- `Option[Color]` (option of enum)
- `Option[Either[A,B]]` flattened union
- Nested option defaults

### Either

| Pattern | Upstream | Kindlings | Gap |
|---|---|---|---|
| `Either[A, B]` -> union | Yes | Yes | None |
| `Either[Record, Record]` | Yes | Yes (tested) | None |
| `Either[String, Option[Int]]` flatten | Yes (null first) | Not tested | **Missing test** |
| `@AvroNamespace` on subtypes | Yes | Not tested | **Missing test** |

### Sealed Traits

#### Enum Mode (all case objects)

| Feature | Upstream | Kindlings | Gap |
|---|---|---|---|
| Basic enum schema | Yes | Yes | None |
| `@AvroName` on symbols | Yes | No | **Missing** |
| `@AvroNamespace` on trait | Yes | Yes (via config/annotation) | None |
| `@AvroEnumDefault` | Yes | Yes | None |
| `@AvroSortPriority` | Yes (Float, higher=earlier) | Yes (Int, lower=earlier) | **Different semantics** |
| Nested in records | Yes | Tested (EventRecord etc.) | None |
| Top-level encode/decode | Yes | Yes | None |

#### Union Mode (case classes)

| Feature | Upstream | Kindlings | Gap |
|---|---|---|---|
| Basic union schema | Yes | Yes | None |
| `@AvroUnionPosition` | Yes | No | **Missing** |
| `@AvroSortPriority` on subtypes | Yes | Yes | None |
| Subtype encode by type match | Yes | Yes | None |
| Subtype decode by schema name | Yes | Yes | None |
| Mixed (objects + classes) | Yes | Yes (objects encoded as empty records) | None |
| Unknown subtype error | `Avro4sDecodingException` | `IllegalArgumentException` | Different exception type |
| Same field names across subtypes | Tested | Not tested | **Missing test** |

### Value Classes (AnyVal)

| Feature | Upstream | Kindlings | Gap |
|---|---|---|---|
| Unwraps to inner schema | Yes | Yes | None |
| `@AvroFixed` on value class | Yes | No | **Missing** |
| `Option[ValueType]` (Issue #191) | Tested | Not tested | **Missing test** |
| Nested in records | Yes | Yes | None |
| Top-level round-trip | Yes | Yes | None |

### Tuples

| Feature | Upstream | Kindlings | Gap |
|---|---|---|---|
| Tuple2 | Yes (namespace `"scala"`) | Yes (no fixed namespace) | **Different namespace** |
| Tuple3 | Yes | Yes | None |
| Tuple4-5 | Yes | Not tested | **Missing tests** |
| Tuple6 bug | Known bug | N/A | N/A |
| Namespace | `"scala"` | Config namespace or `""` | **Different** |

### Generic Types

| Feature | Upstream | Kindlings | Gap |
|---|---|---|---|
| `Generic[String]` schema | `Generic__String` | `Box` (erased) | **Different naming** |
| `@AvroErasedName` | Yes (opt-in) | Default behavior | Inverted |
| Type param encoding | Default on | Never on | **Missing feature** |
| Different instantiations | Different schemas | Same schema name | **Potential collision** |
| Tested | `GenericSchemaTest` | `Box[Int]`, `Pair[String, Int]` | Partial |

### Recursive Types

| Pattern | Upstream | Kindlings | Gap |
|---|---|---|---|
| Self-recursive via List | Tested | Defined (`RecursiveTree`) but **not tested** | **Missing tests** |
| Self-recursive via Map | Tested | Not defined | **Missing** |
| Self-recursive via Option | Tested | Not defined | **Missing** |
| Mutual recursion | Tested | Not defined | **Missing** |
| Either-based tree | Tested | Not defined | **Missing** |

**File reference:** `RecursiveTree` defined at `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/avro-derivation/src/test/scala/hearth/kindlings/avroderivation/examples.scala` line 24, but no test references it.

### Shapeless Coproducts -- **NOT IMPLEMENTED**

Upstream supports `Int :+: String :+: Boolean :+: CNil` as Avro UNION. Kindlings does not support Shapeless coproducts. This is a deliberate design choice since Kindlings targets both Scala 2.13 and 3, and Shapeless coproducts are largely replaced by sealed traits and union types in Scala 3.

---

## Logical Types Comparison

| Logical Type | Scala/Java Type | Upstream | Kindlings | Gap |
|---|---|---|---|---|
| `uuid` | `UUID` | string + uuid | string + uuid | None |
| `date` | `LocalDate` | int + date | int + date | None |
| `date` | `java.sql.Date` | int + date | No | **Missing** |
| `time-micros` | `LocalTime` | long + time-micros | long + time-micros | None |
| `timestamp-millis` | `Instant` | long + timestamp-millis | long + timestamp-millis | None |
| `timestamp-millis` | `java.sql.Timestamp` | long + timestamp-millis | No | **Missing** |
| `timestamp-nanos` | `LocalDateTime` | long + custom timestamp-nanos | long + timestamp-millis (!) | **Different logical type** |
| `datetime-with-offset` | `OffsetDateTime` | string + custom logical type | No | **Missing** |
| `decimal` | `BigDecimal` | bytes + decimal (configurable) | bytes + decimal or string | Partial |

### Timestamp Precision -- **NOT IMPLEMENTED**

Upstream supports three precision variants for timestamps:
- `timestamp-millis`: epoch milliseconds
- `timestamp-micros`: epoch microseconds
- `timestamp-nanos`: epoch nanoseconds

Kindlings only supports `timestamp-millis` for all temporal types. Even `LocalDateTime` uses `timestamp-millis` (upstream uses `timestamp-nanos` for `LocalDateTime`).

### Missing Temporal Types

- `OffsetDateTime` -- no schema, encoder, or decoder
- `java.sql.Date` -- no schema, encoder, or decoder
- `java.sql.Timestamp` -- no schema, encoder, or decoder

### Instant Edge Cases (upstream)

Upstream handles `Instant.MAX -> Long.MaxValue` and `Instant.MIN -> Long.MinValue`. Kindlings does not special-case these.

---

## Schema Evolution Comparison

| Scenario | Upstream | Kindlings | Gap |
|---|---|---|---|
| New field with default | Tested (`SchemaEvolutionTest`) | Schema defaults via `@avroDefault` only | **Partial** |
| Schema-level defaults | Tested | Only via `@avroDefault` | **Partial** |
| New Optional field -> None | Tested | Not tested | **Missing test** |
| Union evolution with position | Tested (`Github587`) | No `@AvroUnionPosition` | **Missing** |
| Enum evolution with defaults | Tested (`EnumSchemaCompatibilityTest`) | `@avroEnumDefault` exists but no evolution test | **Missing test** |
| Forward/backward compat | Tested extensively | Not tested | **Missing tests** |

**Major gap:** Upstream has dedicated schema evolution tests covering forward/backward compatibility with reader/writer schema resolution. Kindlings has no schema evolution tests. The `@avroDefault` annotation enables placing defaults in the schema, but no test verifies the actual evolution scenario (reader reads data written with a different schema).

---

## Scala 3 Feature Comparison

| Feature | Upstream | Kindlings | Gap |
|---|---|---|---|
| Scala 3 enums (simple) | Via Magnolia | Via Hearth `Enum.parse` | None (both work) |
| Scala 3 enums (parameterized) | Tested | Not tested | **Missing test** |
| `given`/`using` syntax | Yes | Yes | None |
| `inline def` + macros | Yes | Yes | None |
| Union types (`A \| B`) | Not supported | Not supported | Same |
| Opaque types | Not supported | Not supported | Same |
| Named tuples | Not mentioned | Via `NamedTuple.parse` | Kindlings extra |

---

## Missing Test Cases

### Tests Present in Upstream But Missing in Kindlings

#### Schema Tests Missing

1. `Seq[T]` collection schema (only `List`/`Vector`/`Set` tested)
2. `Array[T]` for non-Byte types
3. `List[Byte]`/`Seq[Byte]`/`Vector[Byte]` as bytes schema
4. `Option[SealedTrait]` schema (union flattening)
5. `Option[Either[A,B]]` schema (union flattening with null)
6. `None.type` schema
7. Recursive type schemas
8. Tuple4+ schemas
9. Multiple `@fieldName` combined with `@avroNamespace`
10. `@AvroUnionPosition` schema ordering
11. `@AvroScalePrecision` per-field configuration
12. Generic type parameter encoding in schema names
13. `SchemaFor` typeclass override tests
14. String schema variants (JavaString, Fixed-length)
15. Tuple namespace (`"scala"`)

#### Encoder Tests Missing

1. Collections of nested types (maps, tuples)
2. `Option[Record]` encoding
3. `Option[SealedTrait]` encoding
4. Recursive type encoding
5. `BigDecimal` rounding behavior tests
6. `Instant.MAX`/`Instant.MIN` edge cases
7. `@avroSortPriority` effect on encoding (priority-ordered enums)
8. Value class within `Option` encoding (Issue #191)

#### Decoder Tests Missing

1. Decoder type widening (Long from Int, Short from Int, etc.)
2. Missing fields with defaults
3. Missing `Option[T]` fields -> `None`
4. Field reordering (schema field order != case class parameter order)
5. Recursive type decoding
6. `Instant.MAX`/`Instant.MIN` edge cases
7. Unknown enum symbol handling with default
8. Union decode failure with descriptive error

#### Round-Trip Tests Missing

1. Recursive types
2. `Map[String, Record]`
3. `Map[String, Option[Boolean]]`
4. `Option[Record]` round-trip
5. `Option[SealedTrait]` round-trip (enum and union)
6. Mixed sealed trait (case objects + case classes)
7. Value class in `Option`
8. Tuple4+
9. Schema evolution round-trip (reader schema != writer schema)
10. Enum evolution round-trip (unknown symbol -> default)

---

## Missing Corner Cases

### GitHub Issues Not Covered

| Issue | Description | Upstream Status | Kindlings Status |
|---|---|---|---|
| #69 | Generic type schema `Message[MyRecord]` | Tested | **Not tested** (generic naming differs) |
| #110 | Default value in `FromRecord` | Tested | **Not applicable** (no `FromRecord`) |
| #191 | AnyVal in `Option` | Tested | **Not tested** |
| #260 | Schema generation determinism | Commented out | Not tested |
| #292 | ADT with type-parametrized values | Tested | **Not tested** |
| #318 | Coproduct containing ADT | Tested | **Not applicable** (no coproducts) |
| #396 | `@AvroName` overrides FieldMapper | Tested | Tested (`@fieldName` overrides config) |
| #484 | Serializable enum decoder | Tested | **Not tested** |
| #587 | Union evolution with `@AvroUnionPosition` | Tested extensively | **Not applicable** (no `@AvroUnionPosition`) |
| #883 | Option schema must not mutate original | Tested | **Not tested** |
| #885 | `Option[SealedTrait]` with single subtype | Tested | **Not tested** |
| #890 | Nested default `None` values | Tested | **Not tested** |

### Runtime Safety Gaps

1. **No decoder type widening**: Upstream decoders gracefully accept narrower numeric types (e.g., Long decoder accepts Int). Kindlings uses strict `asInstanceOf` which will fail with `ClassCastException` if Avro delivers an unexpected type.

2. **No `TypeGuardedDecoding`**: Upstream uses `PartialFunction`-based type guards for safe union dispatch. Kindlings uses `GenericData.get().resolveUnion()` for Either and schema name matching for sealed traits, but has no general type guard mechanism.

3. **No `Avro4sDecodingException` with value capture**: Upstream captures the problematic value in decoding exceptions. Kindlings throws generic `IllegalArgumentException`.

### Compile-Time Safety Gaps

1. **No validation of `@avroErasedName` with multiple type instantiations**: Upstream warns that erased names cause collisions. Kindlings always erases, so two `Box[Int]` and `Box[String]` in the same record would collide.

2. **No validation of enum default value**: `@avroEnumDefault("NonExistent")` would produce an invalid schema at runtime. No compile-time check that the default is a valid symbol.

---

## Architectural Differences

| Aspect | Upstream avro4s | Kindlings avro-derivation |
|---|---|---|
| **Derivation engine** | Magnolia 1 | Hearth (macro-agnostic) |
| **Scala versions** | 3 only (v5) | 2.13 + 3 |
| **Typeclass design** | `SchemaFor[T]` / `Encoder[T]` / `Decoder[T]` separate | `AvroSchemaFor[A]` / `AvroEncoder[A] extends AvroSchemaFor[A]` / `AvroDecoder[A] extends AvroSchemaFor[A]` |
| **Schema ownership** | Separate `SchemaFor` | Embedded in encoder/decoder (each carries schema) |
| **Schema-driven encode/decode** | `encode(schema): T => AnyRef` (curried on schema) | `encode(value: A): Any` (schema is instance field) |
| **Configuration** | `FieldMapper` + annotations | `AvroConfig` (runtime config) + annotations |
| **Default values** | Automatic from Scala defaults | Explicit via `@avroDefault("json")` annotation |
| **Entry points** | `AvroSchema[T]`, `ToRecord[T]`, `FromRecord[T]`, `AvroOutputStream/InputStream` | `AvroSchemaFor.schemaOf[T]`, `AvroEncoder.encode[T]`, `AvroDecoder.decode[T]`, `AvroIO.toBinary/fromBinary/toJson/fromJson` |
| **Record type** | `ImmutableRecord` (custom) | `GenericData.Record` (standard Avro) |
| **Output formats** | Binary, JSON, Data (with embedded schema) | Binary, JSON |
