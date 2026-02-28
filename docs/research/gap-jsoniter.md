# Jsoniter Gap Analysis: Upstream vs Kindlings

> Analysis date: 2026-02-28
> Upstream: jsoniter-scala 2.38.x (31 config fields, 3 annotations)
> Kindlings: jsoniter-derivation module (23 config fields, 3 annotations)

## Summary

- **Config fields implemented**: 23/31 (8 missing)
- **Config fields missing**: `bigIntDigitsLimit`, `bitSetValueLimit`, `allowRecursiveTypes`, `skipNestedOptionValues`, `scalaTransientSupport`, `inlineOneValueClasses`, `alwaysEmitDiscriminator`, `transientNull`
- **Config fields with different defaults**: `discriminatorFieldName` (Kindlings: `None`, upstream: `Some("type")`), `transientDefault` (Kindlings: `false`, upstream: `true`), `transientEmpty` (Kindlings: `false`, upstream: `true`), `transientNone` (Kindlings: `false`, upstream: `true`), `checkFieldDuplication` (Kindlings: `false`, upstream: `true`), `mapMaxInsertNumber` (Kindlings: `Int.MaxValue`, upstream: `1024`), `setMaxInsertNumber` (Kindlings: `Int.MaxValue`, upstream: `1024`)
- **Config fields with Kindlings additions**: `enumAsStrings` (not in upstream -- Kindlings-specific flag for case-object-only sealed traits)
- **Annotations**: 3/3 implemented (with different names: `@fieldName` vs `@named`, `@transientField` vs `@transient`)
- **Type support gaps**: `java.util.UUID`, 8 of 16 `java.time.*` types, `BitSet`, `Iterator`, boxed Java primitives, most specific collection subtypes (relies on Hearth's generic IsCollection/IsMap), `Either`, `Unit`, `Byte`/`Short`/`Char` as map keys
- **Test coverage gaps**: No tests for `requireDiscriminatorFirst`, no deeply recursive ADT tests, no nested option tests, no `@fieldName` on ADT leaf classes (discriminator override), limited numeric boundary tests, no DoS protection corner case tests
- **Architecture difference**: Kindlings config is runtime (case class), upstream config is compile-time (macro parameter). Kindlings checks config values at runtime in the generated codec; upstream bakes config values into generated code at compile time.

---

## CodecMakerConfig Field-by-Field Comparison

### Field 1: `fieldNameMapper`
- **Upstream behavior**: `PartialFunction[String, String]`, default `partialIdentity`. Predefined mappers: `enforce_snake_case`, `enforce_snake_case2`, `enforce-kebab-case`, `enforce-kebab-case2`, `enforceCamelCase`, `EnforcePascalCase`.
- **Kindlings status**: Implemented
- **Kindlings name**: `fieldNameMapper: String => String`, default `identity`
- **Kindlings tests**: snake_case, kebab-case, PascalCase, SCREAMING_SNAKE_CASE field names; combined with other config options; combined with `@fieldName` annotation
- **Gap**:
  - Kindlings uses `String => String` instead of `PartialFunction[String, String]` (minor API difference, functionally equivalent)
  - Missing `enforceCamelCase` predefined mapper (upstream converts snake/kebab to camelCase)
  - Missing variant mappers (`enforce_snake_case2`, `enforce-kebab-case2`) that handle consecutive uppercase differently
  - Kindlings adds `SCREAMING_SNAKE_CASE` which upstream does not have as a predefined mapper

### Field 2: `javaEnumValueNameMapper`
- **Upstream behavior**: `PartialFunction[String, String]`, default `partialIdentity`. Maps Java enum value names to JSON strings.
- **Kindlings status**: **Missing**
- **Kindlings tests**: None
- **Gap**: No support for custom Java enum value name mapping. Java enums use their `name()` directly.

### Field 3: `adtLeafClassNameMapper`
- **Upstream behavior**: `PartialFunction[String, String]` or `String => String`, default `simpleClassName` (strips package prefix).
- **Kindlings status**: Implemented
- **Kindlings name**: `adtLeafClassNameMapper: String => String`, default `identity`
- **Kindlings tests**: snake_case, kebab-case, toLowerCase mapper; combined with discriminator; combined with `enumAsStrings`
- **Gap**:
  - Default differs: Kindlings uses `identity` (no stripping), upstream uses `simpleClassName` (strips package prefix). In practice, Kindlings always passes the simple class name (extracted from Hearth's `Enum.directChildren`), so the effective behavior is similar.
  - Missing `simpleClassName` as a predefined utility function

### Field 4: `discriminatorFieldName`
- **Upstream behavior**: `Option[String]`, default `Some("type")`. `None` = wrapper-object encoding.
- **Kindlings status**: Implemented
- **Kindlings name**: `discriminatorFieldName: Option[String]`, default `None`
- **Kindlings tests**: Discriminator with `Some("type")`, combined with name mappers, wrapper-style (default)
- **Gap**:
  - Default differs: Kindlings defaults to `None` (wrapper-style), upstream defaults to `Some("type")` (discriminator-style)
  - This is a deliberate design choice, not a bug

### Field 5: `isStringified`
- **Upstream behavior**: `Boolean`, default `false`. Stringify number/boolean values of collections, options, and value classes.
- **Kindlings status**: Implemented
- **Kindlings name**: `isStringified: Boolean`, default `false`
- **Kindlings tests**: Global isStringified encodes/decodes numeric fields as strings; interaction with `@stringified` annotation
- **Gap**:
  - Kindlings applies stringification to all numeric fields in case classes (not just collections/options/value classes as upstream specifies). This is arguably broader coverage.
  - No tests for stringified Boolean values
  - No tests for stringified values inside collections or options

### Field 6: `mapAsArray`
- **Upstream behavior**: `Boolean`, default `false`. Serialize/parse maps as `[[k,v],...]` instead of `{k:v,...}`.
- **Kindlings status**: Implemented
- **Kindlings name**: `mapAsArray: Boolean`, default `false`
- **Kindlings tests**: String-keyed maps, Int-keyed maps, empty maps, case class with map field, combined with mapMaxInsertNumber DoS limit
- **Gap**: No significant gap. Well tested.

### Field 7: `skipUnexpectedFields`
- **Upstream behavior**: `Boolean`, default `true`. Skip unexpected fields during parsing. When `false`, throws on unknown fields.
- **Kindlings status**: Implemented
- **Kindlings name**: `skipUnexpectedFields: Boolean`, default `true`
- **Kindlings tests**: Default skips extra fields; `false` rejects extra fields with error message containing field name
- **Gap**: No significant gap.

### Field 8: `transientDefault`
- **Upstream behavior**: `Boolean`, default `true`. Skip serialization of fields whose values equal their constructor defaults.
- **Kindlings status**: Implemented
- **Kindlings name**: `transientDefault: Boolean`, default `false`
- **Kindlings tests**: Fields with default values omitted when equal; non-default values written; round-trip; interaction with `requireDefaultFields`
- **Gap**:
  - Default differs: Kindlings `false`, upstream `true`
  - No tests for default values with complex types (e.g., case class defaults, collection defaults)

### Field 9: `transientEmpty`
- **Upstream behavior**: `Boolean`, default `true`. Skip serialization of fields with empty arrays/collections.
- **Kindlings status**: Implemented
- **Kindlings name**: `transientEmpty: Boolean`, default `false`
- **Kindlings tests**: Empty collections omitted; non-empty written; round-trip; interaction with `requireCollectionFields`
- **Gap**:
  - Default differs: Kindlings `false`, upstream `true`
  - No tests for empty `Set`, `Array`, or `Map` separately

### Field 10: `transientNone`
- **Upstream behavior**: `Boolean`, default `true`. Skip serialization of `None` option fields.
- **Kindlings status**: Implemented
- **Kindlings name**: `transientNone: Boolean`, default `false`
- **Kindlings tests**: None fields omitted; Some fields written; round-trip; interaction with `@fieldName`
- **Gap**:
  - Default differs: Kindlings `false`, upstream `true`

### Field 11: `requireCollectionFields`
- **Upstream behavior**: `Boolean`, default `false`. Require collection/array fields in JSON input.
- **Kindlings status**: Implemented
- **Kindlings name**: `requireCollectionFields: Boolean`, default `false`
- **Kindlings tests**: Accepts complete JSON; throws when collection field missing; round-trip; interaction with `transientEmpty`
- **Gap**: No significant gap.

### Field 12: `bigDecimalPrecision`
- **Upstream behavior**: `Int`, default `34`. Precision for BigDecimal values.
- **Kindlings status**: Implemented
- **Kindlings name**: `bigDecimalPrecision: Int`, default `34`
- **Kindlings tests**: Normal values accepted; custom precision rejects exceeding values; accepts within custom precision
- **Gap**: No significant gap.

### Field 13: `bigDecimalScaleLimit`
- **Upstream behavior**: `Int`, default `6178`. Exclusive limit for accepted scale.
- **Kindlings status**: Implemented
- **Kindlings name**: `bigDecimalScaleLimit: Int`, default `6178`
- **Kindlings tests**: Validated via `JsoniterDerivationUtils.validateBigDecimal`
- **Gap**: No dedicated test for scale limit violations.

### Field 14: `bigDecimalDigitsLimit`
- **Upstream behavior**: `Int`, default `308`. Exclusive limit for accepted mantissa digits.
- **Kindlings status**: Implemented
- **Kindlings name**: `bigDecimalDigitsLimit: Int`, default `308`
- **Kindlings tests**: Used for both BigDecimal and BigInt validation (BigInt reuses this field)
- **Gap**: BigInt uses `bigDecimalDigitsLimit` instead of having its own `bigIntDigitsLimit` field.

### Field 15: `bigIntDigitsLimit`
- **Upstream behavior**: `Int`, default `308`. Exclusive limit for BigInt decimal digits.
- **Kindlings status**: **Missing** (reuses `bigDecimalDigitsLimit` for BigInt validation)
- **Kindlings tests**: BigInt rejection test uses `bigDecimalDigitsLimit` at line 1329 of test spec
- **Gap**: No separate `bigIntDigitsLimit` config field. BigInt validation delegates to `bigDecimalDigitsLimit`. This is a minor API difference; the validation logic itself is present.

### Field 16: `bitSetValueLimit`
- **Upstream behavior**: `Int`, default `1024`. Exclusive limit for numeric values in bit sets.
- **Kindlings status**: **Missing**
- **Kindlings tests**: None
- **Gap**: No `BitSet` support at all (neither `immutable.BitSet` nor `mutable.BitSet`). No `bitSetValueLimit` config field.

### Field 17: `mapMaxInsertNumber`
- **Upstream behavior**: `Int`, default `1024`. Max entries in deserialized maps.
- **Kindlings status**: Implemented
- **Kindlings name**: `mapMaxInsertNumber: Int`, default `Int.MaxValue`
- **Kindlings tests**: Accepts within limit; rejects exceeding limit; combined with `mapAsArray`
- **Gap**:
  - Default differs: Kindlings `Int.MaxValue` (no limit by default), upstream `1024`
  - This means Kindlings is less secure by default against map-based DoS attacks

### Field 18: `setMaxInsertNumber`
- **Upstream behavior**: `Int`, default `1024`. Max entries in deserialized sets (excluding BitSet).
- **Kindlings status**: Implemented (as collection size limit)
- **Kindlings name**: `setMaxInsertNumber: Int`, default `Int.MaxValue`
- **Kindlings tests**: Accepts within limit; rejects exceeding limit (tested on List, not Set)
- **Gap**:
  - Default differs: Kindlings `Int.MaxValue`, upstream `1024`
  - Kindlings applies this to all collections (List, Vector, etc.), not just sets. This is broader but uses the same config name.
  - Less secure by default

### Field 19: `allowRecursiveTypes`
- **Upstream behavior**: `Boolean`, default `false`. Prevents stack overflow with untrusted input.
- **Kindlings status**: **Missing**
- **Kindlings tests**: Recursive types are tested and work without any flag (`RecursiveTree` round-trip test)
- **Gap**: Kindlings always allows recursive types. No compile-time check or config flag to prevent them. This means no protection against stack overflow from malicious deeply-nested recursive JSON input.

### Field 20: `requireDiscriminatorFirst`
- **Upstream behavior**: `Boolean`, default `true`. Require discriminator field as first field.
- **Kindlings status**: Partially implemented
- **Kindlings name**: `requireDiscriminatorFirst: Boolean`, default `true` (in config)
- **Kindlings tests**: **No tests at all**
- **Gap**: The config field exists but is **never read in the macro implementation**. The decoder always requires the discriminator as the first field (hardcoded in `readWithDiscriminator` at `JsoniterDerivationUtils.scala` line 236). Setting `requireDiscriminatorFirst = false` has no effect.

### Field 21: `useScalaEnumValueId`
- **Upstream behavior**: `Boolean`, default `false`. Use numeric IDs instead of names for Scala Enumeration values.
- **Kindlings status**: Implemented
- **Kindlings name**: `useScalaEnumValueId: Boolean`, default `false`
- **Kindlings tests**: Encode as id; decode from id; round-trip all values
- **Gap**: No significant gap.

### Field 22: `skipNestedOptionValues`
- **Upstream behavior**: `Boolean`, default `false`. Skip some values for options nested more than 2 times.
- **Kindlings status**: **Missing**
- **Kindlings tests**: None
- **Gap**: No support for `Option[Option[A]]` nesting behavior configuration. No nested option tests at all.

### Field 23: `circeLikeObjectEncoding`
- **Upstream behavior**: `Boolean`, default `false`. Case objects as `{"ClassName":{}}` instead of `"ClassName"` in ADTs.
- **Kindlings status**: Implemented (but with different semantics)
- **Kindlings name**: `circeLikeObjectEncoding: Boolean`, default `false`
- **Kindlings tests**: Case objects encode as bare strings; case classes as wrapped objects; round-trip for both
- **Gap**:
  - Kindlings encodes case objects as bare strings `"Pending"` when `circeLikeObjectEncoding=true`, but upstream encodes them as `{"Pending":{}}` (empty object wrapper). The Kindlings behavior matches the **default wrapper mode** for case objects, not circe-like.
  - Upstream's circe-like encoding wraps ALL leaves (including case objects) in `{"Name": {...}}` format. Kindlings' implementation treats case objects as strings even with `circeLikeObjectEncoding=true`.

### Field 24: `decodingOnly`
- **Upstream behavior**: `Boolean`, default `false`. Generate only decoding implementation.
- **Kindlings status**: Implemented
- **Kindlings name**: `decodingOnly: Boolean`, default `false`
- **Kindlings tests**: Decodes normally; throws on encode; combined with `requireDefaultFields`
- **Gap**:
  - Kindlings implements this as a runtime check (`throw UnsupportedOperationException`) rather than compile-time code elimination. The encoder code is still generated but guarded by a runtime `if`.

### Field 25: `encodingOnly`
- **Upstream behavior**: `Boolean`, default `false`. Generate only encoding implementation.
- **Kindlings status**: Implemented
- **Kindlings name**: `encodingOnly: Boolean`, default `false`
- **Kindlings tests**: Encodes normally; throws on decode; combined with `transientDefault`
- **Gap**:
  - Same as `decodingOnly`: runtime check rather than compile-time elimination.

### Field 26: `requireDefaultFields`
- **Upstream behavior**: `Boolean`, default `false`. Require fields with default values in JSON input.
- **Kindlings status**: Implemented
- **Kindlings name**: `requireDefaultFields: Boolean`, default `false`
- **Kindlings tests**: Accepts complete JSON; throws when default field missing; round-trip; interaction with `transientDefault` and `decodingOnly`
- **Gap**: No significant gap.

### Field 27: `checkFieldDuplication`
- **Upstream behavior**: `Boolean`, default `true`. Check for duplicated field names.
- **Kindlings status**: Implemented
- **Kindlings name**: `checkFieldDuplication: Boolean`, default `false`
- **Kindlings tests**: Accepts without duplicates; throws on duplicate; allows when off (last value wins); combined with field name transforms
- **Gap**:
  - Default differs: Kindlings `false`, upstream `true`
  - Less secure by default

### Field 28: `scalaTransientSupport`
- **Upstream behavior**: `Boolean`, default `false`. Support Scala's `@transient` annotation.
- **Kindlings status**: **Missing**
- **Kindlings tests**: None
- **Gap**: Kindlings uses its own `@transientField` annotation instead. Scala's built-in `@transient` is not recognized.

### Field 29: `inlineOneValueClasses`
- **Upstream behavior**: `Boolean`, default `false`. Inline non-value classes with single constructor argument.
- **Kindlings status**: **Missing**
- **Kindlings tests**: None
- **Gap**: Only `AnyVal` subclasses (true value classes) are unwrapped. Regular single-field case classes are always encoded as JSON objects.

### Field 30: `alwaysEmitDiscriminator`
- **Upstream behavior**: `Boolean`, default `false`. Always emit discriminator field even when inferrable.
- **Kindlings status**: **Missing**
- **Kindlings tests**: None
- **Gap**: No config field or implementation.

### Field 31: `transientNull`
- **Upstream behavior**: `Boolean`, default `false`. Skip serialization of fields with null values (for Scala 3 union types with `Null`).
- **Kindlings status**: **Missing**
- **Kindlings tests**: None
- **Gap**: No support for null-skipping in union types.

---

## Annotation Comparison

### `@named` (upstream) vs `@fieldName` (Kindlings)
- **Upstream**: `@named(name: String)` -- targets both fields and ADT leaf classes
- **Kindlings**: `@fieldName(name: String)` -- targets fields only
- **Kindlings file**: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/main/scala/hearth/kindlings/jsoniterderivation/annotations/fieldName.scala`
- **Kindlings tests**: Custom field name encoding/decoding; overrides config fieldNameMapper; combined with config transforms; UTF-8 field names
- **Gap**:
  - **No class-level `@fieldName` for ADT discriminator override**: Upstream's `@named` on a case class overrides the discriminator value. Kindlings does not support `@fieldName` on ADT leaf classes to override the discriminator. Only `adtLeafClassNameMapper` can customize discriminator values.
  - Different annotation name (`@fieldName` vs `@named`)

### `@transient` (upstream) vs `@transientField` (Kindlings)
- **Upstream**: `@transient` -- excludes field from both parsing and serialization; requires default value
- **Kindlings**: `@transientField` -- same semantics
- **Kindlings file**: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/main/scala/hearth/kindlings/jsoniterderivation/annotations/transientField.scala`
- **Kindlings tests**: Excludes from encoding; decodes without field; compile error without default; combined with `@fieldName`
- **Gap**:
  - Different annotation name (`@transientField` vs `@transient`)
  - No gap in functionality

### `@stringified` (both)
- **Upstream**: `@stringified` -- read/write numeric/boolean field values as strings
- **Kindlings**: `@stringified` -- same name, same purpose
- **Kindlings file**: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/main/scala/hearth/kindlings/jsoniterderivation/annotations/stringified.scala`
- **Kindlings tests**: Int, Long, BigDecimal as strings; mixed fields; compile error on non-numeric; combined with `@fieldName`; combined with global `isStringified`
- **Gap**:
  - No tests for `@stringified` on `Boolean` fields
  - No tests for `@stringified` on `Float`/`Double`/`Short`/`Byte` fields
  - Compile error correctly rejects non-numeric types

---

## Type Support Comparison

### Primitives and Boxed

| Type | Upstream | Kindlings | Test |
|------|----------|-----------|------|
| `Int` | Yes | Yes | Yes |
| `Long` | Yes | Yes | Yes |
| `Double` | Yes | Yes | Yes |
| `Float` | Yes | Yes | Indirectly (via Map key) |
| `Boolean` | Yes | Yes | Yes |
| `String` | Yes | Yes | Yes |
| `Byte` | Yes | Yes | No dedicated test |
| `Short` | Yes | Yes | No dedicated test |
| `Char` | Yes | Yes | No dedicated test |
| `java.lang.Integer` etc. (boxed) | Yes | **No** | N/A |

**Gap**: No support for boxed Java primitives (`java.lang.Integer`, `java.lang.Boolean`, etc.). Kindlings only handles Scala primitives.

### Numeric Types

| Type | Upstream | Kindlings | Test |
|------|----------|-----------|------|
| `BigDecimal` | Yes (with limits) | Yes (with limits) | Yes (precision test) |
| `BigInt` | Yes (with limits) | Yes (with limits) | Yes (digits limit test) |

**Gap**: BigInt validation uses `bigDecimalDigitsLimit` instead of a separate `bigIntDigitsLimit`.

### Standard Library Types

| Type | Upstream | Kindlings | Test |
|------|----------|-----------|------|
| `Option[A]` | Yes | Yes | Yes (Some/None round-trip) |
| `Either[L, R]` | Custom codec required | **No** | N/A |
| `Unit` | Yes | **No** | N/A |

### Java Types

| Type | Upstream | Kindlings | Test |
|------|----------|-----------|------|
| `java.util.UUID` | Yes | **No** | N/A |
| `java.time.Instant` | Yes | Yes | Yes (JVM test) |
| `java.time.LocalDate` | Yes | Yes | Yes (JVM test) |
| `java.time.LocalDateTime` | Yes | Yes | Yes (JVM test) |
| `java.time.LocalTime` | Yes | Yes | Yes (JVM test) |
| `java.time.OffsetDateTime` | Yes | Yes | Yes (JVM test) |
| `java.time.ZonedDateTime` | Yes | Yes | Yes (JVM test) |
| `java.time.Duration` | Yes | Yes | Yes (JVM test) |
| `java.time.Period` | Yes | Yes | Yes (JVM test) |
| `java.time.MonthDay` | Yes | **No** | N/A |
| `java.time.OffsetTime` | Yes | **No** | N/A |
| `java.time.Year` | Yes | **No** | N/A |
| `java.time.YearMonth` | Yes | **No** | N/A |
| `java.time.ZoneId` | Yes | **No** | N/A |
| `java.time.ZoneOffset` | Yes | **No** | N/A |

**Gap**: 8 of 16 `java.time.*` types missing: `MonthDay`, `OffsetTime`, `Year`, `YearMonth`, `ZoneId`, `ZoneOffset`, plus `java.util.UUID`. These are handled by jsoniter-scala-core's `JsonReader`/`JsonWriter` natively, so adding support requires only extending `EncHandleAsBuiltInRule` and `DecHandleAsBuiltInRule` with additional type checks.

### Tuples

| Type | Upstream | Kindlings | Test |
|------|----------|-----------|------|
| Standard tuples (Tuple1..Tuple22) | Yes | **Partial** | Yes (2-tuple, 3-tuple) |
| Scala 3 tuples > 22 | Yes | **Unknown** (depends on Hearth) | No |
| Named tuples (Scala 3.7+) | Yes | Yes | Yes (Scala 3 test) |

**Gap**: Tuples are handled via Hearth's case class derivation (tuples are product types). Only Tuple2 and Tuple3 are explicitly tested. Named tuples have dedicated handling via `EncHandleAsNamedTupleRule`/`DecHandleAsNamedTupleRule`.

### Collections

Kindlings uses Hearth's `IsCollection` and `IsMap` type class instances, which provides generic collection support through `scala.collection.Factory`. This means any collection type that Hearth's `IsCollection` recognizes is supported. The following are explicitly tested:

| Type | Kindlings Test |
|------|---------------|
| `List[A]` | Yes |
| `Vector[A]` | Yes |
| `Set[A]` | Yes |
| `Map[String, A]` | Yes |
| `Map[Int, A]` | Yes |
| `Map[Long, A]` | Yes |

**Not tested** (but may work via Hearth's generic collection support):
- `Seq[A]`, `IndexedSeq[A]`
- `SortedSet[A]`, `TreeSet[A]`, `ListSet[A]`, `HashSet[A]`
- `SortedMap[K, V]`, `TreeMap[K, V]`, `ListMap[K, V]`, `HashMap[K, V]`
- `IntMap[V]`, `LongMap[V]`
- `BitSet` (likely **not supported** -- requires special handling)
- `Stream[A]` / `LazyList[A]`
- `Queue[A]`
- All mutable collections
- `Iterator[A]` (likely **not supported** -- requires special serialization)
- `Array[A]` (depends on ClassTag availability)
- `IArray[A]` (Scala 3) -- tested and works

**Gap**: Collection coverage relies on Hearth's `IsCollection`/`IsMap` abstractions. While this provides broad coverage, the following are specifically missing:
- `BitSet` (requires special numeric-value handling + `bitSetValueLimit`)
- `Iterator[A]` (requires lazy serialization semantics)
- `::[A]` (non-empty list with empty-array rejection)
- Specific mutable collection types are untested

### Map Key Types

| Key Type | Upstream | Kindlings | Test |
|----------|----------|-----------|------|
| `String` | Yes | Yes | Yes |
| `Int` | Yes | Yes | Yes |
| `Long` | Yes | Yes | Yes |
| `Double` | Yes | Yes | Yes |
| `Float` | Yes | Yes | Yes |
| `Short` | Yes | Yes | Yes |
| `Boolean` | Yes | Yes | Yes |
| `BigDecimal` | Yes | Yes | Yes |
| `BigInt` | Yes | Yes | Yes |
| `java.util.UUID` | Yes | **No** | N/A |
| `java.time.*` | Yes | **No** | N/A |
| Value classes | Yes | Yes | Yes (UserId) |
| Enum (sealed case objects) | Yes | Yes | Yes (CardinalDirection) |
| Java enums | Yes | **Unknown** | No |
| Literal types | Yes | **Unknown** | No |

**Gap**: Missing UUID and java.time key types. java.time key encoding/decoding would require extending `deriveKeyEncoding`/`deriveKeyDecoding` with the same built-in types.

### Value Classes

| Feature | Upstream | Kindlings | Test |
|---------|----------|-----------|------|
| AnyVal subclasses | Yes | Yes | Yes (WrappedInt, UserId) |
| As map keys | Yes | Yes | Yes (UserId key) |
| In collections | Yes | Yes | Implicit (via generic collection) |
| `inlineOneValueClasses` | Yes | **No** | N/A |

### ADT (Sealed Hierarchies)

| Feature | Upstream | Kindlings | Test |
|---------|----------|-----------|------|
| Sealed traits + case classes | Yes | Yes | Yes |
| Case objects | Yes | Yes | Yes |
| Nested ADTs | Yes | **Unknown** | No |
| GADTs | Yes | **Unknown** | No |
| Polymorphic sealed traits | Yes | **Unknown** | No |
| Recursive ADTs | Yes (with flag) | Yes (always) | Yes (RecursiveTree) |

### Scala 3 Specific Types

| Feature | Upstream | Kindlings | Test |
|---------|----------|-----------|------|
| Scala 3 enums | Yes | Yes | Yes (Fruit enum) |
| Opaque types | Yes | Yes | Yes (UserId) |
| Union types | Custom codec | Yes (auto-derived) | Yes (String\|Int, Parrot\|Hamster) |
| Named tuples | Yes | Yes | Yes |
| Literal types | Yes | Yes | Yes (String, Int, Boolean) |
| `IArray` | Yes | Yes | Yes |
| `derives` keyword | Yes | **Partial** | No `derives` test |

**Note**: Kindlings auto-derives union types, which upstream does NOT support (requires custom codec). This is a feature *advantage* for Kindlings.

### Scala Enumerations

| Feature | Upstream | Kindlings | Test |
|---------|----------|-----------|------|
| By name | Yes | Yes | Yes |
| By id (`useScalaEnumValueId`) | Yes | Yes | Yes |
| Java enums | Yes | Yes | Yes (JVM-only test) |

---

## ADT Encoding Style Comparison

| Style | Upstream | Kindlings | Test |
|-------|----------|-----------|------|
| Discriminator field (default upstream) | Yes | Yes | Yes |
| Wrapper object (default Kindlings) | Yes | Yes | Yes |
| Circe-like object encoding | Yes | **Partial** | Yes (but wrong semantics for case objects) |
| `@named` on leaf for discriminator override | Yes | **No** | No |

**Gap detail for circe-like encoding**: Upstream encodes case objects as `{"Name":{}}` (object with empty value object). Kindlings encodes case objects as `"Name"` (bare string). This is the same as wrapper-mode case object encoding, not circe-like. The distinction only matters for case objects -- case classes are wrapped correctly as `{"Name":{...}}` in both.

---

## DoS Protection Comparison

| Protection | Upstream | Kindlings | Default |
|------------|----------|-----------|---------|
| `mapMaxInsertNumber` | 1024 | `Int.MaxValue` | **Weaker** |
| `setMaxInsertNumber` | 1024 | `Int.MaxValue` | **Weaker** |
| `bitSetValueLimit` | 1024 | **Missing** | **Missing** |
| `bigDecimalPrecision` | 34 | 34 | Same |
| `bigDecimalScaleLimit` | 6178 | 6178 | Same |
| `bigDecimalDigitsLimit` | 308 | 308 | Same |
| `bigIntDigitsLimit` | 308 | Uses `bigDecimalDigitsLimit` | Functionally same |
| `allowRecursiveTypes` | false | **Always allowed** | **Weaker** |
| Runtime buffer limits | Yes (ReaderConfig) | N/A (uses upstream core) | Same |

**Gap summary**: Kindlings provides weaker DoS protection by default:
1. No map/set insert limits by default (`Int.MaxValue` vs `1024`)
2. No recursive type protection (always allowed)
3. No BitSet value limit
4. Runtime buffer limits are inherited from jsoniter-scala-core's `ReaderConfig`/`WriterConfig`, which Kindlings does not wrap or configure.

---

## Error Handling Comparison

### Compile-Time Errors

| Error Case | Upstream | Kindlings | Test |
|------------|----------|-----------|------|
| Non-sealed trait as ADT base | Yes | Yes (via Hearth) | No |
| Missing codec for type | Yes | Yes | Yes |
| @transient without default | Yes | Yes | Yes |
| Duplicate discriminator values | Yes | **No** (runtime check) | No |
| Recursive types without flag | Yes | **No** (always allowed) | N/A |
| @stringified on non-numeric | Yes | Yes | Yes |
| Type inferred as Nothing/Any | N/A | Yes | Yes |

### Runtime Errors

| Error Case | Upstream | Kindlings | Test |
|------------|----------|-----------|------|
| Missing required field | Yes | Yes (via `throwMissingField`) | Yes |
| Duplicate field | Yes | Yes (via `throwDuplicateField`) | Yes |
| Unexpected field | Yes | Yes | Yes |
| Map/Set insert limit | Yes | Yes | Yes |
| BigDecimal precision | Yes | Yes | Yes |
| BigInt digits | Yes | Yes | Yes |
| Invalid JSON structure | Yes (via core) | Yes (via core) | Implicit |
| Null for non-Option | Yes (via core) | Yes (via core) | Yes |

---

## Missing Test Cases

### Tests that upstream has but Kindlings lacks

1. **`requireDiscriminatorFirst` = false**: Kindlings has the config field but no implementation and no tests
2. **Nested options** (`Option[Option[A]]`): No tests for distinguishing null from missing
3. **`@fieldName` on ADT leaf classes**: No tests for overriding discriminator values per-leaf
4. **`@stringified` on Boolean**: Not tested
5. **`@stringified` inside collections**: Not tested (e.g., `List[@stringified Int]`)
6. **Empty constructor case class with defaults**: Only `AllOptionalWithDefaults` tested with `transientDefault`
7. **BitSet types**: Not supported, not tested
8. **Iterator serialization**: Not supported, not tested
9. **Non-empty list (`::[A]`)**: Not supported, not tested
10. **`UUID` encoding**: Not supported, not tested
11. **Mutable collection types**: Not tested
12. **`Array[A]` types**: Not tested
13. **Deeply recursive ADT stack overflow**: No test for stack overflow behavior
14. **Java enum name mapping**: No `javaEnumValueNameMapper` support
15. **`inlineOneValueClasses`**: Not supported, not tested
16. **`alwaysEmitDiscriminator`**: Not supported, not tested
17. **Compile-time duplicate discriminator detection**: Not tested
18. **Scala 3 `derives` keyword**: No test
19. **Generic case classes with bounded type parameters**: Not tested
20. **Case class with `var` fields or non-case classes**: Not tested

---

## Missing Corner Cases

### Numeric Boundaries
- **Tested**: `Int.MaxValue`, `Int.MinValue`, `Long.MaxValue`, `Long.MinValue`
- **Missing**: `Byte.MinValue`/`MaxValue`, `Short.MinValue`/`MaxValue`, `Float` precision edge cases, very large `BigInt`/`BigDecimal` (100K+ digits)

### Unicode and Encoding
- **Tested**: Unicode emoji in field values, UTF-8 field names with `@fieldName`
- **Missing**: Surrogate pairs in keys, escaped characters (`\n`, `\t`, `\\`), illegal UTF-8 sequences, non-ASCII in discriminator values

### Collection Edge Cases
- **Tested**: Empty list, empty map, empty set
- **Missing**: `null` JSON deserialized as empty collection, nested collections (`List[List[Int]]`), collections exceeding size limits with exact boundary values

### Field Edge Cases
- **Tested**: Fields in different order, missing optional fields, duplicate fields
- **Missing**: Field names with special JSON characters (quotes, backslashes), field names that collide after name mapping

### Option Edge Cases
- **Tested**: `Some`, `None`, `Option[Int]`
- **Missing**: `Option[Option[A]]`, `Option[List[A]]`, `Option` as collection element

### ADT Edge Cases
- **Tested**: Discriminator as first field, wrapper-object encoding, case objects, recursive ADT
- **Missing**: Discriminator not first, unknown discriminator value (error message test), deeply nested sealed hierarchies, GADT with type parameters

### Value Class Edge Cases
- **Tested**: `WrappedInt`, `UserId`, value class as map key
- **Missing**: Value class wrapping String, value class wrapping other value class, value class in Option

---

## Kindlings-Specific Features (Not in Upstream)

1. **`enumAsStrings` config field**: Kindlings-specific convenience flag for encoding case-object-only sealed traits as bare strings. Upstream achieves this through custom codec or `ConfiguredJsonValueCodec`.

2. **Union type auto-derivation** (Scala 3): Kindlings auto-derives codecs for union types (`A | B`) using Hearth's `directChildren`. Upstream requires a custom `JsonValueCodec` implementation with backtracking.

3. **`KindlingsJsonValueCodec.writeToString` / `readFromString` inline methods**: Macro-powered one-shot encode/decode without needing to store a codec instance. These derive encoder-only or decoder-only codecs inline.

4. **`syntax.toJsonString` / `fromJsonString` extension methods**: Ergonomic syntax for one-shot JSON conversion.

5. **`JsonValueCodecExtensions.map` / `mapDecode`**: Composable codec transformations for building codecs from existing ones.

6. **`KindlingsJsonCodec.deriveKeyCodec`**: Standalone `JsonKeyCodec[A]` derivation.

7. **`SCREAMING_SNAKE_CASE` name mapper**: Predefined mapper not available in upstream.

---

## Key File References

### Kindlings Implementation Files
- Config: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/main/scala/hearth/kindlings/jsoniterderivation/JsoniterConfig.scala`
- Macro impl: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/main/scala/hearth/kindlings/jsoniterderivation/internal/compiletime/CodecMacrosImpl.scala`
- Runtime helpers: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/main/scala/hearth/kindlings/jsoniterderivation/internal/runtime/JsoniterDerivationUtils.scala`
- Annotations: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/main/scala/hearth/kindlings/jsoniterderivation/annotations/` (fieldName.scala, stringified.scala, transientField.scala)
- Scala 3 companion: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/main/scala-3/hearth/kindlings/jsoniterderivation/KindlingsJsonValueCodecCompanionCompat.scala`

### Kindlings Test Files
- Main spec: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/test/scala/hearth/kindlings/jsoniterderivation/KindlingsJsonValueCodecSpec.scala`
- Inline methods spec: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/test/scala/hearth/kindlings/jsoniterderivation/InlineMethodsSpec.scala`
- JVM spec: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/test/scalajvm/hearth/kindlings/jsoniterderivation/KindlingsJsonValueCodecJvmSpec.scala`
- Scala 3 spec: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/test/scala-3/hearth/kindlings/jsoniterderivation/JsoniterScala3Spec.scala`
- Test examples: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/test/scala/hearth/kindlings/jsoniterderivation/examples.scala`
- Scala 3 examples: `/Users/dev/Workspaces/GitHub/kindlings/.claude/worktrees/crystalline-inventing-cerf/jsoniter-derivation/src/test/scala-3/hearth/kindlings/jsoniterderivation/scala3examples.scala`
