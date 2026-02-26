# Remaining Gaps — Implementation Plan

**Replaces:** `gap-analysis.md` and `type-class-gap-analysis.md` (safe to delete).

**Last updated:** 2026-02-26

This document contains all the information needed to implement each remaining gap. Each section is self-contained — an agent with cleared context can pick up any section and implement it.

---

## Status Overview

| # | Gap | Module | Priority | Difficulty | Status |
|---|-----|--------|----------|------------|--------|
| 1 | `Encoder.AsObject` | Circe | **High** | Medium | **Done** |
| 2 | Literal types | All 4 | Medium | Medium | **Done** |
| 3 | `@stringified` | Jsoniter | Medium | Medium | **Done** |
| 4 | Map as array encoding | Jsoniter | Medium | Medium | **Done** |
| 5 | `@AvroFixed` | Avro | High | Medium | **Done** |
| 6 | `@AvroProp` | Avro | Medium | Low | **Done** |
| 7 | `@AvroAlias` | Avro | Medium | Medium | **Done** |
| 8 | `@AvroError` | Avro | Low | Low | **Done** |
| 9 | `@AvroSortPriority` | Avro | Low | Low | **Done** |
| 10 | `ByteBuffer` encoding | Avro | Low | Low | **Done** |
| 11 | UTF-8 field names | All | Low | Low (tests only) | **Done** |
| 12 | Union types (Scala 3) | All | Low | Hard | **Done** |
| 13 | `Codec.AsObject` | Circe | Low | Low | **Done** |
| 14 | `JsonCodec` (combined) | Jsoniter | Low | Low | **Done** |

### Already Completed (for reference)

- Circe `Encoder.AsObject` — `KindlingsEncoder.deriveAsObject[A]` returns `Encoder.AsObject[A]` for case classes and sealed traits (2026-02-26)
- Circe `KeyEncoder`/`KeyDecoder` — built-in types inlined + user implicit summoning (2026-02-26)
- Jsoniter non-String map keys — built-in types + `JsonKeyCodec[K]` summoning (2026-02-26)
- Avro `@avroFixed(size)` — field-level annotation changing `Array[Byte]` fields from BYTES to FIXED schema, with encoder/decoder support and compile-time validation (2026-02-26)
- Avro `@avroError` — marker annotation setting `isError=true` on RECORD schemas for Avro RPC error types (2026-02-26)
- Avro `ByteBuffer` — built-in type support for `java.nio.ByteBuffer` mapping to Avro BYTES in schema, encoder, and decoder (2026-02-26)
- Avro `@avroSortPriority(priority)` — controls ordering of subtypes in ENUM and UNION schemas (2026-02-26)
- Avro `@avroProp(key, value)` — adds custom key-value properties to schemas and fields, supports multiple annotations on same target (2026-02-26)
- Avro `@avroAlias(alias)` — adds schema evolution aliases to records and fields, supports multiple annotations on same target (2026-02-26)
- Jsoniter `@stringified` — per-field annotation encoding numeric fields as JSON strings, with compile-time validation (2026-02-26)
- Jsoniter `mapAsArray` — config option encoding maps as `[[k1,v1],[k2,v2]]` arrays instead of JSON objects (2026-02-26)
- Circe `Codec.AsObject` — `KindlingsCodecAsObject.derive[A]` combining `Encoder.AsObject[A]` + `Decoder[A]` with runtime combiner for Scala 3 splice isolation (2026-02-26)
- Jsoniter `JsonCodec` (combined) — `KindlingsJsonCodec.derive[A]` combining `JsonValueCodec[A]` + `JsonKeyCodec[A]` with standalone `deriveKeyCodec[A]` for primitives, value types, and enums (2026-02-26)
- Literal types — `HandleAsLiteralTypeRule` in all 4 modules: `case class Foo(tag: "hello", code: 42)` encodes/decodes the constant value, validates on decode (2026-02-26)
- UTF-8 field names — Tests verifying `@fieldName` with non-ASCII characters across all 4 modules (2026-02-26)
- Union types (Scala 3) — `String | Int`, `Parrot | Hamster` via Hearth's updated `Enum.parse`. Fixed Jsoniter/Avro decoder dispatch for built-in types and FQN name matching (2026-02-26)
- All items listed as RESOLVED in the former `gap-analysis.md` (generics, enums, opaque types, named tuples, java enums, Scala Enumeration, error accumulation, recursive types, HKTs, mutable collections, IArray, IntMap/LongMap/BitSet, etc.)

---

## Gap 1: Circe `Encoder.AsObject` — DONE

### What Was Implemented

**Approach:** Added a separate `deriveAsObject[A]` entry point (not modifying `derive`/`derived`). Reuses the existing rule chain (which produces `Expr[Json]`) and extracts `JsonObject` via `.asObject.get` — safe because case classes, named tuples, and sealed traits always produce JSON objects.

**Files changed:**
- `KindlingsEncoder.scala` — Added `KindlingsEncoderAsObject[A]` trait extending `KindlingsEncoder[A] with Encoder.AsObject[A]`
- `EncoderMacrosImpl.scala` — Added `deriveEncoderAsObjectTypeClass[A]` method with compile-time validation (rejects non-case-class/non-enum/non-named-tuple types) and runtime guard for `enumAsStrings` edge case. Added `KindlingsEncoderAsObject` and `JsonObject` to `Types` object.
- `EncoderMacros.scala` (Scala 2 & 3) — Added `deriveEncoderAsObjectImpl` bridge methods
- `KindlingsEncoderCompanionCompat.scala` (Scala 2 & 3) — Added `deriveAsObject[A]` public entry point
- `KindlingsEncoderSpec.scala` — 7 new tests: case class, same output as derive, mapJsonObject, empty case class, sealed trait, discriminator config, configuration

**Design notes:**
- `derive[A]` and `derived[A]` are unchanged — fully backwards compatible
- Value types (e.g. `WrappedInt`) compile with `deriveAsObject` (they ARE case classes) but throw `IllegalStateException` at runtime since they produce non-object JSON
- `enumAsStrings=true` with all-case-object sealed traits also throws at runtime (produces string, not object)

---

## Gap 2: Literal Types — DONE

### What Was Implemented

Added `HandleAsLiteralTypeRule` (encoder and decoder) to all 4 modules. Scala 3 only — on Scala 2, `TypeCodec.fromType` returns `None` for non-literal types, making the rule a no-op.

**Approach:** Uses Hearth's `TypeCodec[U].fromType(Type[A])` to extract compile-time constants from literal types (String, Int, Long, Double, Float, Boolean, Short, Byte, Char). Encoder emits the constant value directly. Decoder reads the underlying type, validates against the constant, and errors on mismatch.

**Files changed per module:**
- Circe: `EncoderMacrosImpl.scala` (new `EncHandleAsLiteralTypeRule`), `DecoderMacrosImpl.scala` (new `DecHandleAsLiteralTypeRule`)
- Jsoniter: `CodecMacrosImpl.scala` (new `EncHandleAsLiteralTypeRule`, `DecHandleAsLiteralTypeRule`; added `Boolean` to `CTypes`; added `Type[U]` param to `decodeLiteral`)
- YAML: `EncoderMacrosImpl.scala`, `DecoderMacrosImpl.scala` (new rules; added `Int/Long/Double/Boolean` to `DTypes`)
- Avro: `SchemaForMacrosImpl.scala`, `EncoderMacrosImpl.scala`, `DecoderMacrosImpl.scala` (new rules; added `Int/Long/Double/Boolean` to `DecTypes`)
- All 4 modules: `scala3examples.scala` (test types), Scala 3 spec files (tests)

**Test types:** `WithLiteralString(tag: "hello", name: String)`, `WithLiteralInt(code: 42, name: String)`, `WithLiteralBoolean(flag: true, name: String)`

**Key pitfalls:**
- On Scala 2, `TypeCodec.fromType` return value is path-dependent; must assign with explicit type `val v: String = e.value` for `ExprCodec` resolution
- On Scala 2, `orElse` chains of `Option[Expr[SubType]]` need explicit upcast (e.g., `(Node.ScalarNode(...): Node)`)
- On Scala 3, `decodeLiteral` methods need `Type[U]` in implicit scope for staging

---

## Gap 3: `@stringified` (Jsoniter) — DONE

### What Was Implemented

Per-field `@stringified` annotation that encodes numeric fields as JSON strings and decodes them back. Supports Int, Long, Double, Float, Short, Byte, BigDecimal, BigInt. Compile-time error on non-numeric types. Modified `encodeCaseClassFieldsOnly`, `decodeCaseClassFields`, and `decodeCaseClassFieldsInline` in `CodecMacrosImpl.scala`. Runtime helpers in `JsoniterDerivationUtils.scala` (2026-02-26).

### Problem

Jsoniter Scala's `withIsStringified(true)` encodes numeric fields as JSON strings (`42` → `"42"`). Useful for APIs that transmit numbers as strings for precision (e.g., large Int64 values in JavaScript).

### Implementation Plan

**Step 1 — Add annotation:**

File: `jsoniter-derivation/src/main/scala/hearth/kindlings/jsoniterderivation/annotations/stringified.scala`

```scala
package hearth.kindlings.jsoniterderivation.annotations
import scala.annotation.StaticAnnotation
final class stringified extends StaticAnnotation
```

**Step 2 — Register in CTypes:**

File: `CodecMacrosImpl.scala`, `CTypes` object (line ~2688)

```scala
val Stringified: Type[stringified] = Type.of[stringified]
```

**Step 3 — Modify encoder case class rule:**

In `EncHandleAsCaseClassRule` (starts at line ~974 of `CodecMacrosImpl.scala`), when processing each field, check `hasAnnotationType[stringified](param)`. If present and the field type is numeric (Int, Long, Double, Float, Short, Byte, BigDecimal, BigInt), wrap the write call: `out.writeVal(value.toString)` instead of `out.writeVal(value)`.

**Step 4 — Modify decoder case class rule:**

In `DecHandleAsCaseClassRule` (starts at line ~1857), when `@stringified` is present, read as string and parse: `in.readString(null).toInt` (etc. for each numeric type).

**Step 5 — Add runtime helpers:**

File: `jsoniter-derivation/src/main/scala/hearth/kindlings/jsoniterderivation/internal/runtime/JsoniterDerivationUtils.scala`

```scala
def readStringifiedInt(in: JsonReader): Int = in.readString(null).toInt
def readStringifiedLong(in: JsonReader): Long = in.readString(null).toLong
// etc.
```

### Tests

File: `KindlingsJsonValueCodecSpec.scala`, new group "@stringified":
- `case class Stringified(value: Int)` with `@stringified` on value → `{"value":"42"}`
- Round-trip with Int, Long, Double, BigDecimal
- Mixed: some fields stringified, some not
- Compile error: `@stringified` on non-numeric field

---

## Gap 4: Map as Array Encoding (Jsoniter) — DONE

### What Was Implemented

`mapAsArray` config option (`JsoniterConfig.withMapAsArray`) that encodes maps as `[[k1,v1],[k2,v2]]` at runtime. Modified `EncHandleAsMapRule` and `DecHandleAsMapRule` in `CodecMacrosImpl.scala` to derive both object-style and array-style encoders/decoders, with runtime branching on `config.mapAsArray`. Added `writeMapAsArray` and `readMapAsArray` helpers in `JsoniterDerivationUtils.scala`. Also handles non-String key types that lack key encoding — in mapAsArray mode they use value-level encoding, with a runtime error in non-mapAsArray mode (2026-02-26).

### Problem

Jsoniter Scala's `withMapAsArray(true)` encodes maps as `[[k1,v1],[k2,v2]]` instead of `{"k1":v1,"k2":v2}`. Useful for non-string keys in JSON (JSON objects require string keys).

### Implementation Plan

**Step 1 — Add config option:**

File: `jsoniter-derivation/src/main/scala/hearth/kindlings/jsoniterderivation/JsoniterConfig.scala`

Add field: `mapAsArray: Boolean = false`
Add builder: `def withMapAsArray: JsoniterConfig = copy(mapAsArray = true)`

**Step 2 — Modify encoder map rule:**

File: `CodecMacrosImpl.scala`, `EncHandleAsMapRule` (starts at line ~759)

When `config.mapAsArray` is true:
- `writer.writeArrayStart()`
- For each entry: `writer.writeArrayStart(); encodeKey(k, writer); encodeValue(v, writer); writer.writeArrayEnd()`
- `writer.writeArrayEnd()`

**Step 3 — Modify decoder map rule:**

File: `CodecMacrosImpl.scala`, `DecHandleAsMapRule` (starts at line ~1599)

When `config.mapAsArray` is true:
- `reader.isNextToken('[')` → read array of pairs
- Each pair: `reader.isNextToken('['); val k = decodeKey(reader); val v = decodeValue(reader); reader.isNextToken(']')`
- `reader.isNextToken(']')`

**Step 4 — Add runtime helpers:**

File: `JsoniterDerivationUtils.scala`

```scala
def writeMapAsArray[K, V](writer: JsonWriter, entries: Iterable[(K, V)], encodeKey: ..., encodeValue: ...): Unit
def readMapAsArray[K, V, M](reader: JsonReader, decodeKey: ..., decodeValue: ..., factory: Factory): M
```

### Tests

File: `KindlingsJsonValueCodecSpec.scala`, new group "map as array":
- `Map[Int, String]` with `mapAsArray=true` → `[[1,"a"],[2,"b"]]`
- `Map[String, Int]` with `mapAsArray=true` → `[["a",1],["b",2]]`
- Round-trip for both
- Empty map → `[]`
- `mapAsArray=false` (default) still uses object style

---

## Gap 5: `@AvroFixed` — DONE

### What Was Implemented

**Approach:** Field-level annotation checked in the case class rules of all 3 derivation phases (schema, encoder, decoder). When `@avroFixed(size)` is present on an `Array[Byte]` field, overrides normal derivation with fixed-specific logic. Compile error if used on non-`Array[Byte]` fields. Extended `AnnotationSupport` with `extractIntLiteralFromAnnotation` / `getAnnotationIntArg` (reusable by Gap 9's `@avroSortPriority`).

**Files changed:**
- `annotations/avroFixed.scala` — New `@avroFixed(size: Int)` annotation
- `AnnotationSupport.scala` — Added `extractIntLiteralFromAnnotation` abstract method and `getAnnotationIntArg` convenience method
- `AnnotationSupportScala2.scala` — Scala 2 impl: `Literal(Constant(value: Int))`
- `AnnotationSupportScala3.scala` — Scala 3 impl: `Literal(IntConstant(value))`
- `AvroDerivationUtils.scala` — Added `createFixed(name, namespace, size)`, `wrapByteArrayAsFixed(bytes, expectedSize)` (with size validation), and `decodeFixed(value)` runtime helpers
- `SchemaForMacrosImpl.scala` — In `SfHandleAsCaseClassRule`, `@avroFixed` fields produce FIXED schema instead of recursively deriving; added `AvroFixedOnNonByteArray` error case
- `EncoderMacrosImpl.scala` — In `EncHandleAsCaseClassRule`, `@avroFixed` fields encode as `GenericData.Fixed` via `wrapByteArrayAsFixed` instead of `ByteBuffer`
- `DecoderMacrosImpl.scala` — In `DecHandleAsCaseClassRule`, `@avroFixed` fields decode via `decodeFixed` (extracts bytes from `GenericFixed`) bypassing `deriveFieldDecoder`
- `examples.scala` — Added `WithFixedBytes` and `WithFixedAndRegularBytes` test types
- `AvroSchemaForSpec.scala` — 4 tests: FIXED type/size, name matching, mixed FIXED+BYTES, compile error on non-Array[Byte]
- `AvroEncoderSpec.scala` — 2 tests: correct-length encode, wrong-length throws AvroRuntimeException
- `AvroDecoderSpec.scala` — 1 test: decode from encoded FIXED data
- `AvroRoundTripSpec.scala` — 2 tests: binary round-trips for both test types

**Design notes:**
- The FIXED schema name uses the field name (or `@fieldName` override if present)
- Runtime size validation throws `AvroRuntimeException` with a clear message
- Decoder clones the byte array from `GenericFixed` to avoid aliasing

---

## Gap 6: `@AvroProp`

### Problem

Avro4s's `@AvroProp(key, value)` adds custom key-value metadata to schema or field. Schema-only change.

### Implementation Plan

**Step 1 — Add annotation:**

File: `avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/avroProp.scala`

```scala
final class avroProp(val key: String, val value: String) extends StaticAnnotation
```

**Step 2 — Extend `AnnotationSupport` for two-string-arg extraction:**

Add `extractTwoStringLiterals(annotation: UntypedExpr): Option[(String, String)]` — match on `Apply(_, List(Literal(StringConstant(k)), Literal(StringConstant(v))))`.

Or reuse `findAnnotationOfType` and extract both args.

**Step 3 — Schema modification:**

After constructing a field's schema or a record's schema, check for `@avroProp` annotations. For each, call `schema.addProp(key, value)` or `field.addProp(key, value)`.

Support both class-level (`findTypeAnnotationOfType`) and field-level (`findAnnotationOfType`).

### Tests

- `@avroProp("custom-key", "custom-value")` on class → schema has prop
- `@avroProp("key", "val")` on field → field has prop
- Multiple props on same target

---

## Gap 7: `@AvroAlias`

### Problem

Avro4s's `@AvroAlias(aliases*)` adds old names as aliases for schema evolution.

### Implementation Plan

**Step 1 — Add annotation:**

File: `avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/avroAlias.scala`

Avro4s uses varargs. Simplest approach: single string, multiple annotations allowed:

```scala
final class avroAlias(val alias: String) extends StaticAnnotation
```

Or varargs if `AnnotationSupport` can extract string arrays.

**Step 2 — Extend `AnnotationSupport` for multi-annotation collection:**

Add `findAllAnnotationsOfType[Ann: Type](param: Parameter): List[UntypedExpr]` — returns all annotations of a given type, not just the first.

**Step 3 — Schema modification:**

For each `@avroAlias`, call `field.addAlias(alias)` or `schema.addAlias(alias)`.

### Tests

- Field with `@avroAlias("old_name")` → field alias in schema
- Record with `@avroAlias("OldName")` → record alias
- Multiple aliases on same field
- Schema evolution test: read data written with old name

---

## Gap 8: `@AvroError`

### Problem

Marker annotation that sets `isError=true` on RECORD schema (for Avro RPC error types).

### Implementation Plan

**Step 1 — Add annotation:**

```scala
final class avroError extends StaticAnnotation
```

**Step 2 — Schema modification:**

In record schema construction (currently hardcodes `isError=false`):
- Check `findTypeAnnotationOfType[avroError, A]`
- Pass `isError=true` to `Schema.createRecord(name, doc, namespace, isError)`

### Tests

- `@avroError` on case class → `schema.isError == true`
- Without annotation → `schema.isError == false`

---

## Gap 9: `@AvroSortPriority`

### Problem

Controls ordering of types in union schemas.

### Implementation Plan

**Step 1 — Add annotation:**

```scala
final class avroSortPriority(val priority: Int) extends StaticAnnotation
```

Uses the integer literal extraction from Gap 5 (`extractIntLiteralFromAnnotation` / `getAnnotationIntArg`) — already implemented.

**Step 2 — Schema modification:**

In sealed trait (union) schema construction, read `@avroSortPriority` from each child type, sort children by priority (lower first, default 0) before creating the union schema.

### Tests

- Sealed trait children with sort priorities → union schema members in specified order
- Default (no annotation) → original order preserved

---

## Gap 10: `ByteBuffer` Encoding (Avro)

### Problem

Avro4s supports `java.nio.ByteBuffer` mapping to Avro BYTES.

### Implementation Plan

Add `java.nio.ByteBuffer` type check to all 3 avro type rule files:

**Schema:** `Schema.create(Schema.Type.BYTES)` (same as `Array[Byte]`)
**Encoder:** `ByteBuffer.array()` → write bytes
**Decoder:** read bytes → `ByteBuffer.wrap(bytes)`

### Key Files

- `avro-derivation/src/main/scala/hearth/kindlings/avroderivation/internal/compiletime/SchemaForMacrosImpl.scala`
- `avro-derivation/src/main/scala/hearth/kindlings/avroderivation/internal/compiletime/EncoderMacrosImpl.scala`
- `avro-derivation/src/main/scala/hearth/kindlings/avroderivation/internal/compiletime/DecoderMacrosImpl.scala`

### Tests

- `ByteBuffer` field schema → BYTES
- Round-trip encode/decode
- Case class with ByteBuffer field

---

## Gap 11: UTF-8/Special Characters in Field Names — DONE

### What Was Implemented

Tests-only change. Verified that `@fieldName` works with non-ASCII characters across all 4 modules. No code changes were needed — the underlying `String` handling already supports UTF-8.

**Test type per module:** `WithUtf8FieldNames` with `@fieldName("名前")`, `@fieldName("données")`, `@fieldName("field with spaces")`.

**Files changed:** `examples.scala` and spec files in all 4 modules (shared Scala 2/3 test code).

---

## Gap 12: Union Types (Scala 3) — DONE

### What Was Implemented

Scala 3 union types (`String | Int`, `Parrot | Hamster`) are now supported in all 4 modules. Uses Hearth 0.2.0-241+ which integrates union types into `Enum.parse`/`Enum.unapply`.

**Approach:** No new rules needed — the existing `HandleAsEnumRule` in each module automatically handles union types because Hearth's `Enum.parse` now returns `Some(...)` for union types (`Type.isUnionType` check). Union type members are treated like sealed trait children.

**Code changes (beyond tests):**
- `build.sbt` — Updated Hearth to `0.2.0-241-gbc935a9-SNAPSHOT`
- Jsoniter `CodecMacrosImpl.scala` — Fixed `deriveChildDecoder` to use derived expression directly when `getHelper` returns `None` (built-in types like `String`/`Int` don't register helpers)
- Avro `DecoderMacrosImpl.scala` — Fixed FQN vs simple name mismatch in record dispatch. Union types return FQN child names from Hearth's `directChildren` (e.g., `"pkg.Parrot"`), but Avro's `record.getSchema.getName` returns simple names (`"Parrot"`). Added `simpleName()` helper to extract simple name at compile time. Also fixed "no helper" fallback.

**Test types per module:** `type StringOrInt = String | Int`, `case class Parrot(...)`, `case class Hamster(...)`, `type ParrotOrHamster = Parrot | Hamster`

**Known behavior:** Union type member names use fully-qualified names (e.g., `java.lang.String`, `scala.Int`, `pkg.Parrot`) as wrapper keys in JSON/YAML encoding. Users can customize this via `transformConstructorNames` config.

**Limitation:** The Avro module only supports case class union members (not primitive unions like `String | Int`), since Avro natively handles primitive types in unions differently.

---

## Gap 13: Circe `Codec.AsObject`

### Problem

Circe's `Codec.AsObject[A]` extends `Decoder[A] with Encoder.AsObject[A]`. Lower priority convenience.

### Implementation Plan

**Depends on:** Gap 1 (`Encoder.AsObject`) being completed first.

Create `KindlingsCodec[A]` extending both `KindlingsEncoder[A]` (or `KindlingsEncoderAsObject[A]`) and `KindlingsDecoder[A]`. Single macro entry point that derives both encoder and decoder in one pass.

This is a convenience — users can already compose `KindlingsEncoder.derived[A]` + `KindlingsDecoder.derived[A]`. Lower priority.

---

## Gap 14: Jsoniter `JsonCodec` (Combined) — Done

### Problem

`JsonCodec[A]` extends both `JsonValueCodec[A]` and `JsonKeyCodec[A]`. Rarely needed — users typically only need `JsonValueCodec`.

### Implementation Plan

Create `KindlingsJsonCodec[A]` extending `KindlingsJsonValueCodec[A]` and implementing `JsonKeyCodec[A]`. The value codec part is the existing derivation. The key codec part would need standalone key encoding/decoding derivation (built-in types + value types + enums), similar to what `deriveKeyEncoding`/`deriveKeyDecoding` already do internally for map key handling.

Lower priority since the internal key derivation already exists for map support — this would just expose it as a standalone type class.

---

## Appendix A: Key File Paths

### Circe Derivation

| File | Purpose |
|------|---------|
| `circe-derivation/src/main/scala/hearth/kindlings/circederivation/KindlingsEncoder.scala` | Encoder trait definition |
| `circe-derivation/src/main/scala/hearth/kindlings/circederivation/KindlingsDecoder.scala` | Decoder trait definition |
| `circe-derivation/src/main/scala/hearth/kindlings/circederivation/Configuration.scala` | Config: transformMemberNames, transformConstructorNames, useDefaults, discriminator, strictDecoding, enumAsStrings |
| `circe-derivation/src/main/scala/hearth/kindlings/circederivation/internal/compiletime/EncoderMacrosImpl.scala` | Encoder macro impl — rule chain at line 223, `Types` object at line 822 |
| `circe-derivation/src/main/scala/hearth/kindlings/circederivation/internal/compiletime/DecoderMacrosImpl.scala` | Decoder macro impl — rule chain at line 387, `DTypes` object at line 1684 |
| `circe-derivation/src/main/scala/hearth/kindlings/circederivation/internal/runtime/CirceDerivationUtils.scala` | Runtime helpers (jsonFromFields, decodeMapWith, decodeKeyInt, etc.) |
| `circe-derivation/src/main/scala-2/.../EncoderMacros.scala` | Scala 2 macro entry points |
| `circe-derivation/src/main/scala-3/.../EncoderMacros.scala` | Scala 3 macro entry points |
| `circe-derivation/src/main/scala-2/.../KindlingsEncoderCompanionCompat.scala` | Scala 2 companion (derive, encode, derived) |
| `circe-derivation/src/main/scala-3/.../KindlingsEncoderCompanionCompat.scala` | Scala 3 companion (derive, encode, derived) |
| `circe-derivation/src/test/scala/.../KindlingsEncoderSpec.scala` | Encoder tests (~75 tests) |
| `circe-derivation/src/test/scala/.../KindlingsDecoderSpec.scala` | Decoder tests (~111 tests) |
| `circe-derivation/src/test/scala/.../examples.scala` | Test model types (SimplePerson, UserId, CardinalDirection, etc.) |

### Jsoniter Derivation

| File | Purpose |
|------|---------|
| `jsoniter-derivation/src/main/scala/.../KindlingsJsonValueCodec.scala` | Codec trait definition |
| `jsoniter-derivation/src/main/scala/.../JsoniterConfig.scala` | Config: fieldNameMapper, adtLeafClassNameMapper, discriminatorFieldName, skipUnexpectedFields, enumAsStrings |
| `jsoniter-derivation/src/main/scala/.../internal/compiletime/CodecMacrosImpl.scala` | Combined codec macro — encoder rule chain at line 561, decoder rule chain at line 1370, `CTypes` at line ~2688 |
| `jsoniter-derivation/src/main/scala/.../internal/runtime/JsoniterDerivationUtils.scala` | Runtime helpers |
| `jsoniter-derivation/src/main/scala/.../annotations/` | fieldName.scala, transientField.scala |
| `jsoniter-derivation/src/test/scala/.../KindlingsJsonValueCodecSpec.scala` | Tests (~107 tests) |
| `jsoniter-derivation/src/test/scala/.../examples.scala` | Test model types |

### Avro Derivation

| File | Purpose |
|------|---------|
| `avro-derivation/src/main/scala/.../internal/compiletime/SchemaForMacrosImpl.scala` | Schema macro impl (line 285 rule chain) |
| `avro-derivation/src/main/scala/.../internal/compiletime/EncoderMacrosImpl.scala` | Encoder macro impl (line 289 rule chain) |
| `avro-derivation/src/main/scala/.../internal/compiletime/DecoderMacrosImpl.scala` | Decoder macro impl (line 289 rule chain) |
| `avro-derivation/src/main/scala/.../internal/compiletime/AnnotationSupport.scala` | Base trait: findAnnotationOfType, findTypeAnnotationOfType, extractStringLiteralFromAnnotation, extractIntLiteralFromAnnotation, hasAnnotationType, getAnnotationStringArg, getAnnotationIntArg, getTypeAnnotationStringArg |
| `avro-derivation/src/main/scala-2/.../AnnotationSupportScala2.scala` | Scala 2: `param.asUntyped.symbol.annotations`, `Literal(Constant(s: String))`, `Literal(Constant(n: Int))` |
| `avro-derivation/src/main/scala-3/.../AnnotationSupportScala3.scala` | Scala 3: `quotes.reflect.*`, `Literal(StringConstant(value))`, `Literal(IntConstant(value))` |
| `avro-derivation/src/main/scala/.../annotations/` | avroDefault.scala, avroDoc.scala, avroFixed.scala, avroNamespace.scala, fieldName.scala, transientField.scala |
| `avro-derivation/src/test/scala/.../AvroSchemaForSpec.scala` | Schema tests (~62 tests) |
| `avro-derivation/src/test/scala/.../AvroEncoderSpec.scala` | Encoder tests (~49 tests) |
| `avro-derivation/src/test/scala/.../AvroDecoderSpec.scala` | Decoder tests (~48 tests) |
| `avro-derivation/src/test/scala/.../AvroRoundTripSpec.scala` | Round-trip tests (~30 tests) |

### Cross-Module

| File | Purpose |
|------|---------|
| `CLAUDE.md` | Build instructions, cross-compilation pitfalls, test aliases |
| `docs/contributing/type-class-derivation-skill.md` | Derivation skill guide |
| `docs/contributing/hearth-api-knowledge.md` | Hearth API quick-reference |
| `docs/contributing/hearth-documentation-skill.md` | Finding Hearth docs |

## Appendix B: Build & Test Commands

```bash
# Clean + test specific modules (ALWAYS clean after macro changes)
sbt --client "circeDerivation/clean; circeDerivation3/clean; test-jvm-2_13; test-jvm-3"
sbt --client "jsoniterDerivation/clean; jsoniterDerivation3/clean; test-jvm-2_13; test-jvm-3"
sbt --client "avroDerivation/clean; avroDerivation3/clean; test-jvm-2_13; test-jvm-3"

# Nuclear option (all modules)
sbt --client clean
sbt --client "test-jvm-2_13; test-jvm-3"

# Redirect output for inspection
sbt --client "circeDerivation/clean; circeDerivation3/clean; test-jvm-2_13; test-jvm-3" 2>&1 | tee /tmp/sbt-output.txt
grep -E '(Failed|Errors|FAILED)' /tmp/sbt-output.txt
```

## Appendix C: Suggested Implementation Order

1. ~~**Gap 1** — Circe `Encoder.AsObject`~~ **DONE**
2. ~~**Gap 5** — `@AvroFixed`~~ **DONE** (introduced `extractIntLiteralFromAnnotation` reusable by Gap 9)
3. **Gap 6** — `@AvroProp` (medium, uses existing annotation pattern)
4. **Gap 7** — `@AvroAlias` (medium, needs `findAllAnnotationsOfType`)
5. **Gap 3** — `@stringified` (medium, self-contained)
6. **Gap 4** — Map as array (medium, self-contained)
7. ~~**Gap 2** — Literal types~~ **DONE**
8. ~~**Gap 8** — `@AvroError`~~ **DONE**
9. ~~**Gap 9** — `@AvroSortPriority`~~ **DONE**
10. ~~**Gap 10** — ByteBuffer~~ **DONE**
11. ~~**Gap 11** — UTF-8 field names~~ **DONE**
12. ~~**Gap 13** — `Codec.AsObject`~~ **DONE**
13. ~~**Gap 14** — `JsonCodec` combined~~ **DONE**
14. ~~**Gap 12** — Union types~~ **DONE**

**All gaps are now implemented.**
