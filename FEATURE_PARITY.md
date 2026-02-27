# Feature Parity: Kindlings vs Original Libraries

This document tracks feature parity between Kindlings derivation modules and the original libraries they replace, as well as UX improvements Kindlings provides.

Legend: **Parity** = feature matches original, **Improvement** = Kindlings does it better, **Gap** = feature not yet implemented.

---

## circe-derivation

**Replaces:** `circe-generic-extras` (Scala 2 only, community-maintained) + circe's Scala 3 `ConfiguredEncoder`/`ConfiguredDecoder`/`ConfiguredCodec`

### Configuration

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Field name transforms (snake/kebab/pascal/screaming) | Yes | Yes | Parity |
| Constructor name transforms | Yes | Yes | Parity |
| `useDefaults` | Yes (Scala 3 needs `-Yretain-trees`) | Yes | Parity |
| Discriminator field | Yes | Yes | Parity |
| Strict decoding | Yes | Yes | Parity |
| `enumAsStrings` config flag | Scala 2: `deriveEnumeration*`; Scala 3: `EnumEncoder`/`EnumDecoder` (parameterless only) | Yes (unified config flag, both Scala 2+3) | Improvement |

### Type classes

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| `Encoder[A]` | Yes | Yes | Parity |
| `Encoder.AsObject[A]` | Yes | Yes (`deriveAsObject`) | Parity |
| `Decoder[A]` | Yes | Yes | Parity |
| `Codec.AsObject[A]` | Scala 3 only | No separate Codec type | Gap (minor) |

### Annotations

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Per-field rename | `@JsonKey` (Scala 2 only! No Scala 3 equivalent) | `@fieldName` (both Scala 2+3) | Improvement |
| Transient field | No built-in annotation | `@transientField` (requires default value) | Improvement |

### Type support

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Value class unwrapping | `deriveUnwrapped*` (Scala 2 only! No Scala 3 equivalent) | Automatic (both Scala 2+3) | Improvement |
| Recursive types | Scala 2: needs Shapeless `Lazy`; Scala 3: broken with auto-derivation ([#1980](https://github.com/circe/circe/issues/1980)) | Works without wrappers | Improvement |
| Non-string map keys | Manual `KeyEncoder`/`KeyDecoder` | Derived (Int, Long, Double, Short, Byte, value classes) | Improvement |
| `Option` null vs absent | No distinction | Yes | Improvement |
| Scala 3 enums | Parameterless only (`EnumEncoder`/`EnumDecoder`) | Full support via `enumAsStrings` | Improvement |

### Cross-compilation

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Unified Scala 2+3 API | No — different modules, different APIs, feature gaps on Scala 3 | Yes | Improvement |
| Cross-platform (JVM/JS/Native) | JVM focus (Shapeless-based on Scala 2) | JVM + JS + Native | Improvement |
| Compile-time performance | Shapeless = slow, stack overflows on deep types | Hearth macros | Improvement |
| Error messages | "could not find Lazy implicit value..." | Clear messages from Hearth | Improvement |

### Not ported

| Feature | Notes |
|---|---|
| `@ConfiguredJsonCodec` macro annotation | Minor — `derives` / explicit derivation covers the same ground |

---

## jsoniter-derivation

**Replaces:** `com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker`

### Configuration

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Field name mapper | Yes | Yes | Parity |
| ADT leaf class name mapper | Yes | Yes | Parity |
| Discriminator field | Yes (default `Some("type")`) | Yes | Parity |
| Skip unexpected fields | Yes (default `true`) | Yes | Parity |
| Enum as strings | Yes | Yes | Parity |
| Map as array | Yes | Yes | Parity |
| `transientDefault` | Yes | No | Gap |
| `transientEmpty` | Yes | No | Gap |
| `transientNone` | Yes | No | Gap |
| `isStringified` (global) | Yes | No (per-field `@stringified` only) | Gap |
| `decodingOnly` / `encodingOnly` | Yes | No | Gap |
| `requireCollectionFields` | Yes | No | Gap |
| `requireDefaultFields` | Yes | No | Gap |
| `requireDiscriminatorFirst` | Yes | No | Gap |
| `circeLikeObjectEncoding` | Yes | No | Gap |
| `checkFieldDuplication` | Yes | No | Gap |
| `bigDecimalPrecision`/`bigDecimalScaleLimit`/`bigDecimalDigitsLimit` | Yes (DoS protection) | No | Gap |
| `mapMaxInsertNumber` / `setMaxInsertNumber` | Yes (DoS protection) | No | Gap |
| `useScalaEnumValueId` | Yes | No | Gap |

### Type classes

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| `JsonValueCodec[A]` | Yes | Yes | Parity |
| `JsonCodec[A]` | Yes | Yes | Parity |
| `JsonKeyCodec[A]` | Yes | Yes (`deriveKeyCodec`) | Parity |

### Annotations

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Per-field rename (`@named` / `@fieldName`) | Yes | Yes | Parity |
| Transient field (`@transient` / `@transientField`) | Yes | Yes (requires default value) | Parity |
| `@stringified` | Yes | Yes | Parity |

### Type support

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Non-string map keys | Yes (extensive) | Yes (numeric + value types + BigDecimal/BigInt) | Parity |
| Value class unwrapping | Yes (+ `inlineOneValueClasses`) | Automatic (always) | Parity |
| Recursive types | Yes (`allowRecursiveTypes = true`) | Yes (no flag needed) | Improvement |
| java.time built-in codecs | Yes (ISO-8601) | No (user-provided) | Gap |

### Cross-compilation

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Unified Scala 2+3 API | Separate macro implementations | Unified API | Improvement |
| Cross-platform | JVM + JS + Native | JVM + JS + Native | Parity |

### Not ported

| Feature | Notes |
|---|---|
| `transientDefault`/`transientEmpty`/`transientNone` | Fine-grained control over omitting fields during encoding |
| DoS protection limits | `bigDecimalPrecision`, `mapMaxInsertNumber`, `setMaxInsertNumber`, etc. |
| `decodingOnly` / `encodingOnly` | Restricts codec to one direction |
| `circeLikeObjectEncoding` | Alternative ADT encoding style |
| java.time codecs | Built-in ISO-8601 temporal codecs |
| Convenience factories (`makeCirceLike`, etc.) | Pre-configured codec makers |

---

## yaml-derivation

**Replaces:** VirtusLab `scala-yaml` built-in `derives YamlEncoder`/`YamlDecoder`/`YamlCodec`

### Configuration

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| Field name transforms (snake/kebab/pascal/screaming) | No | Yes | Improvement |
| Constructor name transforms | No | Yes | Improvement |
| Discriminator config | No | Yes | Improvement |
| `enumAsStrings` | No (buggy for enums) | Yes | Improvement |

### Type classes

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| `YamlEncoder[A]` | Yes (Scala 3 only!) | Yes (Scala 2+3) | Improvement |
| `YamlDecoder[A]` | Yes (Scala 3 only!) | Yes (Scala 2+3) | Improvement |
| `YamlCodec[A]` | Yes (Scala 3 only!) | No combined type | Gap (minor) |

### Annotations

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| Per-field rename | None | `@fieldName` | Improvement |
| Transient field | None | `@transientField` | Improvement |

### Type support

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| Sealed trait handling | Brute-force try-each (ambiguous, O(n)) | Discriminator-based | Improvement |
| Enum support | Buggy ([#363](https://github.com/VirtusLab/scala-yaml/issues/363)) | Works | Improvement |
| Option encoding | Buggy (documented) | Works | Improvement |
| Recursive types | Undocumented | Yes | Improvement |
| Value classes | Not documented | Automatic unwrapping | Improvement |

### Cross-compilation

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| Scala 2 support | No (Scala 3 only) | Yes | Improvement |
| Scala 3 Native | No ([#324](https://github.com/VirtusLab/scala-yaml/issues/324)) | Yes | Improvement |
| Cross-platform | JVM + JS (Scala 2 Native only) | JVM + JS + Native | Improvement |

### Not ported

| Feature | Notes |
|---|---|
| `useDefaults` | scala-yaml uses runtime reflection for defaults; Kindlings does not support this yet |

---

## avro-derivation

**Replaces:** `com.sksamuel.avro4s` (v4 for Scala 2, v5 for Scala 3 — separate incompatible versions)

### Configuration

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| Namespace config | Yes | Yes | Parity |
| Field name transforms (`FieldMapper`) | Yes (`SnakeCase`, custom) | Yes (4 built-in strategies) | Parity |
| Constructor name transforms | Not built-in | Yes | Improvement |
| Decimal config (precision/scale) | `@AvroScalePrecision` or implicit `ScalePrecision` | `AvroConfig.decimalConfig` | Parity (different API) |

### Type classes

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| `SchemaFor[A]` | Yes | Yes (`AvroSchemaFor`) | Parity |
| `Encoder[A]` | Yes | Yes (`AvroEncoder`) | Parity |
| `Decoder[A]` | Yes | Yes (`AvroDecoder`) | Parity |

### Annotations

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| Per-field rename (`@AvroName` / `@fieldName`) | Yes | Yes | Parity |
| Documentation (`@AvroDoc` / `@avroDoc`) | Yes | Yes | Parity |
| Namespace (`@AvroNamespace` / `@avroNamespace`) | Yes | Yes | Parity |
| Custom properties (`@AvroProp` / `@avroProp`) | Yes | Yes (stackable) | Parity |
| Aliases (`@AvroAlias` / `@avroAlias`) | Yes | Yes (stackable) | Parity |
| Fixed size (`@AvroFixed` / `@avroFixed`) | Yes | Yes | Parity |
| Error type (`@AvroError` / `@avroError`) | Yes | Yes | Parity |
| Transient field (`@AvroTransient` / `@transientField`) | Yes | Yes | Parity |
| Default values (`@AvroDefault`) | From constructor defaults (automatic) | `@avroDefault(json)` (explicit JSON string) | Parity (different API) |
| Subtype ordering (`@AvroUnionPosition` / `@avroSortPriority`) | Yes | Yes | Parity |
| `@AvroNoDefault` (suppress defaults) | Yes | No | Gap |
| `@AvroErasedName` (disable generic name encoding) | Yes | No | Gap |
| `@AvroEnumDefault` | Yes | No | Gap |

### Type support

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| UUID | Yes | Yes | Parity |
| Instant | Yes | Yes | Parity |
| LocalDate | Yes | Yes | Parity |
| LocalTime | Yes | Yes | Parity |
| LocalDateTime | Yes | Yes | Parity |
| BigDecimal | BYTES/FIXED with decimal logical type | BYTES with decimal logical type (when DecimalConfig provided); STRING fallback | Parity |
| Value classes | Unwrapped | Unwrapped | Parity |
| Case classes → RECORD | Yes | Yes | Parity |
| Sealed case objects → ENUM | Yes | Yes | Parity |
| Sealed traits → UNION | Yes | Yes | Parity |
| `Either[A,B]` → UNION | Yes | Yes | Parity |
| `Option[T]` → UNION(null,T) | Yes | Yes | Parity |
| Recursive types | Yes (with caveats) | Yes | Parity |
| Shapeless Coproduct → UNION | Yes | No | Gap |

### Cross-compilation

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| Unified Scala 2+3 API | No (v4 ≠ v5, different APIs) | Yes | Improvement |
| Cross-platform | JVM only | JVM only (Avro dependency is JVM-only) | Parity |

### Not ported

| Feature | Notes |
|---|---|
| `@AvroNoDefault` | Suppress default value in schema |
| `@AvroErasedName` | Disable generic type parameter name encoding |
| `@AvroEnumDefault` | Default value for enum type |
| Shapeless Coproduct → UNION | avro4s can map `A :+: B :+: CNil` to UNION |
| Cats integration | `avro4s-cats` module for `NonEmptyList`, etc. |
| Refined types | `avro4s-refined` module |
| Kafka `GenericSerde` | `avro4s-kafka` module |

---

## tapir-schema-derivation

**Replaces:** Tapir's built-in `Schema.derived` + `sttp.tapir.generic.auto.*`

### Annotations

| Feature | Tapir built-in | Kindlings | Status |
|---|---|---|---|
| `@description` | Yes | Yes | Parity |
| `@title` | Yes | Yes | Parity |
| `@encodedName` | Yes | Yes | Parity |
| `@format` | Yes | Yes | Parity |
| `@hidden` | Yes (was broken on Scala 3) | Yes | Parity |
| `@deprecated` | Yes | Yes | Parity |
| `@validate` | Yes | Yes | Parity |
| `@validateEach` | Yes | Yes | Parity |
| `@default` | Yes | Yes | Parity |
| `@encodedExample` | Yes | Yes | Parity |
| `@customise` | Yes | Yes | Parity |

### Configuration

| Feature | Tapir built-in | Kindlings | Status |
|---|---|---|---|
| Field naming | Independent `sttp.tapir.generic.Configuration` (must manually match JSON lib) | Discovers JSON lib config at compile time | Improvement |
| Discriminator | Independent (must manually match JSON lib) | Discovers JSON lib config at compile time | Improvement |
| JSON config consistency | **No** — schema and codec configs are separate, drift is common | **Yes** — automatically reads Circe/Jsoniter config via `JsonSchemaConfigExtension` | Improvement |
| Recursive types on Scala 3 | Must use `implicit def` (not `given`), risk of deadlocks | Works | Improvement |

### Not ported

| Feature | Notes |
|---|---|
| `Schema.derivedEnumeration` | Specific enum schema derivation |
| `Schema.oneOfWrapped` | Manual union schema builder |
| `.modify(_.path)` post-derivation | Tapir core feature, not a derivation concern |

---

## fast-show-pretty

**Original type class** — no replacement target. Provides configurable pretty-printing with indentation support for case classes, collections, maps, and primitives. Cross-compiled for Scala 2.13 + 3, JVM + JS + Native.
