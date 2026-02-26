# Remaining Gaps — Implementation Plan

**Replaces:** `gap-analysis.md` and `type-class-gap-analysis.md` (safe to delete).

**Last updated:** 2026-02-26

This document contains all the information needed to implement each remaining gap. Each section is self-contained — an agent with cleared context can pick up any section and implement it.

---

## Status Overview

| # | Gap | Module | Priority | Difficulty | Status |
|---|-----|--------|----------|------------|--------|
| 1 | `Encoder.AsObject` | Circe | **High** | Medium | **Done** |
| 2 | Literal types | All 4 | Medium | Medium | Not started |
| 3 | `@stringified` | Jsoniter | Medium | Medium | Not started |
| 4 | Map as array encoding | Jsoniter | Medium | Medium | Not started |
| 5 | `@AvroFixed` | Avro | High | Medium | **Done** |
| 6 | `@AvroProp` | Avro | Medium | Low | Not started |
| 7 | `@AvroAlias` | Avro | Medium | Medium | Not started |
| 8 | `@AvroError` | Avro | Low | Low | Not started |
| 9 | `@AvroSortPriority` | Avro | Low | Low | Not started |
| 10 | `ByteBuffer` encoding | Avro | Low | Low | Not started |
| 11 | UTF-8 field names | All | Low | Low (tests only) | Not started |
| 12 | Union types (Scala 3) | All | Low | Hard (blocked) | Blocked on Hearth |
| 13 | `Codec.AsObject` | Circe | Low | Low | Not started |
| 14 | `JsonCodec` (combined) | Jsoniter | Low | Low | Not started |

### Already Completed (for reference)

- Circe `Encoder.AsObject` — `KindlingsEncoder.deriveAsObject[A]` returns `Encoder.AsObject[A]` for case classes and sealed traits (2026-02-26)
- Circe `KeyEncoder`/`KeyDecoder` — built-in types inlined + user implicit summoning (2026-02-26)
- Jsoniter non-String map keys — built-in types + `JsonKeyCodec[K]` summoning (2026-02-26)
- Avro `@avroFixed(size)` — field-level annotation changing `Array[Byte]` fields from BYTES to FIXED schema, with encoder/decoder support and compile-time validation (2026-02-26)
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

## Gap 2: Literal Types

### Problem

Literal type fields (`case class Tagged(tag: "hello", count: 42)`) don't work. The derivation tries to summon `Encoder["hello"]` / `Decoder[42]` which don't exist.

### What's Needed

A new `HandleAsLiteralTypeRule` per module. Scala 3 only (Scala 2 has no literal type syntax for case class fields — the rule will be a no-op).

### Implementation Plan

**Per module** (circe, jsoniter, yaml, avro):

1. Add `EncHandleAsLiteralTypeRule` before `EncHandleAsValueTypeRule` in the rule chain
2. Use `Type.valueOfConstant[A]` (Hearth API) to extract the constant at compile time
3. **Encoder:** emit the constant value directly (the runtime value is always the same)
4. **Decoder:** read the value, validate it matches the literal, return error otherwise

**Circe example:**
```scala
// Encoder: literal String "hello" → Json.fromString("hello")
// Encoder: literal Int 42 → Json.fromInt(42)
// Decoder: validate parsed value == literal constant
```

**Jsoniter example:**
```scala
// Encoder: writeVal("hello") or writeVal(42)
// Decoder: read value, compare against constant, decodeError if mismatch
```

**Avro example:**
```scala
// Schema: use the underlying type schema (STRING for "hello", INT for 42)
// Add default value equal to the literal constant
// Encoder: write the constant
// Decoder: validate value == constant
```

### Key Hearth API

Verify `Type.valueOfConstant[A]` exists and returns `Option[A]` or similar. Check: `../hearth/` source or Hearth docs at `https://scala-hearth.readthedocs.io/en/latest/`.

### Tests

Per module, add "literal types" group (Scala 3 only tests):
- `case class Tagged(tag: "hello")` encodes as `{"tag":"hello"}`
- `case class Counted(n: 42)` encodes as `{"n":42}`
- `case class BoolFlag(flag: true)` encodes as `{"flag":true}`
- Decode: correct literal value succeeds
- Decode: wrong literal value returns error

### File Locations

- Rule chains are assembled in `deriveEncoderRecursively` / `deriveDecoderRecursively`:
  - Circe encoder: `EncoderMacrosImpl.scala` line 223
  - Circe decoder: `DecoderMacrosImpl.scala` line 387
  - Jsoniter: `CodecMacrosImpl.scala` lines 561 (encoder) and 1370 (decoder)
- Tests: each module's spec file + Scala 3 specific test files

---

## Gap 3: `@stringified` (Jsoniter)

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

## Gap 4: Map as Array Encoding (Jsoniter)

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

## Gap 11: UTF-8/Special Characters in Field Names

### Problem

Likely already works. Just needs tests.

### Implementation Plan

Add tests with Unicode field names via `@fieldName`:
- `@fieldName("名前")` (Japanese)
- `@fieldName("données")` (French accents)
- `@fieldName("🔑")` (emoji)
- `@fieldName("field with spaces")`

Test in at least circe and jsoniter modules.

---

## Gap 12: Union Types (Scala 3) — BLOCKED

### Problem

Scala 3 union types (`String | Int`) need Hearth support for `isUnion` and `unionMembers` APIs. Currently no Hearth APIs exist for union type introspection.

### Blockers

1. **Hearth upstream:** Need `Type[A].isUnion: Boolean` and `Type[A].unionMembers: List[Type[?]]`
2. Runtime erasure means no `Class`-based dispatch — would need try-parse fallback

### Action

File a Hearth issue requesting union type introspection APIs. Do not attempt implementation until Hearth provides the APIs.

---

## Gap 13: Circe `Codec.AsObject`

### Problem

Circe's `Codec.AsObject[A]` extends `Decoder[A] with Encoder.AsObject[A]`. Lower priority convenience.

### Implementation Plan

**Depends on:** Gap 1 (`Encoder.AsObject`) being completed first.

Create `KindlingsCodec[A]` extending both `KindlingsEncoder[A]` (or `KindlingsEncoderAsObject[A]`) and `KindlingsDecoder[A]`. Single macro entry point that derives both encoder and decoder in one pass.

This is a convenience — users can already compose `KindlingsEncoder.derived[A]` + `KindlingsDecoder.derived[A]`. Lower priority.

---

## Gap 14: Jsoniter `JsonCodec` (Combined)

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
7. **Gap 2** — Literal types (medium, Scala 3 only, needs Hearth API verification)
8. **Gap 8** — `@AvroError` (low, trivial with annotation infra from Gap 5)
9. **Gap 9** — `@AvroSortPriority` (low, uses int extraction from Gap 5)
10. **Gap 10** — ByteBuffer (low, pure type rule addition)
11. **Gap 11** — UTF-8 field names (low, tests only)
12. **Gap 13** — `Codec.AsObject` (low, Gap 1 prerequisite now done)
13. **Gap 14** — `JsonCodec` combined (low)
14. **Gap 12** — Union types (blocked on Hearth)
