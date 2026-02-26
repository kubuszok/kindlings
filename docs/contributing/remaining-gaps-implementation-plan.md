# Remaining Gaps — Implementation Plan

**Replaces:** `gap-analysis.md` and `type-class-gap-analysis.md` (safe to delete).

**Last updated:** 2026-02-26

This document contains all the information needed to implement each remaining gap. Each section is self-contained — an agent with cleared context can pick up any section and implement it.

---

## Status Overview

| # | Gap | Module | Priority | Difficulty | Status |
|---|-----|--------|----------|------------|--------|
| 1 | `Encoder.AsObject` | Circe | **High** | Medium | Not started |
| 2 | Literal types | All 4 | Medium | Medium | Not started |
| 3 | `@stringified` | Jsoniter | Medium | Medium | Not started |
| 4 | Map as array encoding | Jsoniter | Medium | Medium | Not started |
| 5 | `@AvroFixed` | Avro | High | Medium | Not started |
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

- Circe `KeyEncoder`/`KeyDecoder` — built-in types inlined + user implicit summoning (2026-02-26)
- Jsoniter non-String map keys — built-in types + `JsonKeyCodec[K]` summoning (2026-02-26)
- All items listed as RESOLVED in the former `gap-analysis.md` (generics, enums, opaque types, named tuples, java enums, Scala Enumeration, error accumulation, recursive types, HKTs, mutable collections, IArray, IntMap/LongMap/BitSet, etc.)

---

## Gap 1: Circe `Encoder.AsObject`

### Problem

All other Circe derivation libraries (`circe-generic`, `circe-derivation`, Scala 3 `derives`) return `Encoder.AsObject[A]` for case classes and sealed traits. Kindlings returns `Encoder[A]` — a strictly weaker type. Users migrating lose:

1. **`mapJsonObject`** — post-processing derived encoders (add/remove/merge fields)
2. **`Codec.AsObject` composition** — `Codec.AsObject[A]` requires `Encoder.AsObject[A]`
3. **Type-level guarantees** — implicit positions requiring `Encoder.AsObject[A]` won't find Kindlings encoders
4. **Scala 3 idiom** — `case class Foo(...) derives Encoder.AsObject` is the recommended circe pattern

### Circe's Encoder Hierarchy

```
Encoder[A]                     -- A => Json (any JSON value)
  └─ Encoder.AsRoot[A]        -- guarantees array or object output
       ├─ Encoder.AsArray[A]  -- A => Vector[Json], wrapped as JSON array
       └─ Encoder.AsObject[A] -- A => JsonObject, wrapped as JSON object
```

`Decoder[A]` has **no** similar hierarchy — no changes needed for `KindlingsDecoder`.

### Current State

- `KindlingsEncoder[A]` extends `Encoder[A]` (file: `circe-derivation/src/main/scala/hearth/kindlings/circederivation/KindlingsEncoder.scala`)
- The macro in `deriveEncoderTypeClass[A]` (line 36 of `EncoderMacrosImpl.scala`) creates `new KindlingsEncoder[A] { def apply(a: A): Json = ... }`
- Internally, case classes already produce `JsonObject` via `CirceDerivationUtils.jsonFromFields` → `Json.fromJsonObject(JsonObject.fromIterable(fields))` — the object is created then immediately wrapped as `Json`
- `jsonFromFields` is called at lines 644, 649, 740, 745 of `EncoderMacrosImpl.scala`

### Implementation Plan

**Approach:** Make `KindlingsEncoder[A]` extend `Encoder.AsObject[A]` for types that produce objects (case classes, sealed traits). Keep `Encoder[A]` for value types, options, collections, tuples.

**Step 1 — Split `KindlingsEncoder` into two variants:**

File: `circe-derivation/src/main/scala/hearth/kindlings/circederivation/KindlingsEncoder.scala`

```scala
trait KindlingsEncoder[A] extends Encoder[A] {
  def apply(a: A): Json
}
trait KindlingsEncoderAsObject[A] extends KindlingsEncoder[A] with Encoder.AsObject[A] {
  def encodeObject(a: A): JsonObject
  final def apply(a: A): Json = Json.fromJsonObject(encodeObject(a))
}
```

Add import: `import io.circe.JsonObject`

**Step 2 — Add `JsonObject`-returning runtime helpers:**

File: `circe-derivation/src/main/scala/hearth/kindlings/circederivation/internal/runtime/CirceDerivationUtils.scala`

Add `jsonObjectFromFields` alongside existing `jsonFromFields`:

```scala
def jsonObjectFromFields(fields: List[(String, Json)]): JsonObject =
  JsonObject.fromIterable(fields)
```

Similarly add `jsonObjectFromMappedPairs`, `addDiscriminatorObject`, `wrapWithTypeNameObject` that return `JsonObject` instead of `Json`.

**Step 3 — Modify the macro to detect object-producing types:**

File: `circe-derivation/src/main/scala/hearth/kindlings/circederivation/internal/compiletime/EncoderMacrosImpl.scala`

In `deriveEncoderTypeClass[A]` (line 36), after derivation, check if the result was produced by case class or sealed trait rules. If so, create `new KindlingsEncoderAsObject[A] { def encodeObject(a: A): JsonObject = ... }` instead.

The case class rule (`EncHandleAsCaseClassRule`) and named tuple rule (`EncHandleAsNamedTupleRule`) should call `jsonObjectFromFields` instead of `jsonFromFields`. The sealed trait rule (`EncHandleAsEnumRule`) should produce `JsonObject` for wrapper-style and discriminator-style encoding.

The value type / option / collection / map / tuple rules keep producing `Json`.

**Step 4 — Update companion entry points:**

Files: `KindlingsEncoderCompanionCompat.scala` (both Scala 2 and 3)

Add overloaded entry points or change `derive` return type:
- `derive[A]` → return `Encoder[A]` (backwards compatible, AsObject is a subtype)
- `derived[A]` → return `KindlingsEncoder[A]` (same — AsObject extends KindlingsEncoder)

No signature changes needed since `Encoder.AsObject <: Encoder` — the macro just returns a more specific runtime type.

**Step 5 — Add `Types` entries:**

File: `EncoderMacrosImpl.scala`, `Types` object (line 822)

```scala
val JsonObject: Type[JsonObject] = Type.of[JsonObject]
def EncoderAsObject: Type.Ctor1[Encoder.AsObject] = Type.Ctor1.of[Encoder.AsObject]
def KindlingsEncoderAsObject: Type.Ctor1[KindlingsEncoderAsObject] = Type.Ctor1.of[KindlingsEncoderAsObject]
```

### Tests

File: `circe-derivation/src/test/scala/hearth/kindlings/circederivation/KindlingsEncoderSpec.scala`

New group "Encoder.AsObject":
- `derive[SimplePerson]` is assignable to `Encoder.AsObject[SimplePerson]`
- `derived[SimplePerson]` is assignable to `Encoder.AsObject[SimplePerson]`
- `mapJsonObject` works on derived encoder
- Sealed trait produces `Encoder.AsObject`
- Value type (`WrappedInt`) does NOT produce `Encoder.AsObject` (still `Encoder`)
- `Option[Int]`, `List[Int]` do NOT produce `Encoder.AsObject`

### Verification

```bash
sbt --client "circeDerivation/clean; circeDerivation3/clean; test-jvm-2_13; test-jvm-3"
```

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

## Gap 5: `@AvroFixed`

### Problem

Avro4s's `@AvroFixed(size)` changes schema type from `BYTES` to `FIXED(size)`. Unlike other annotations that mutate properties, this changes which **schema type** is selected.

### Implementation Plan

**Step 1 — Add annotation:**

File: `avro-derivation/src/main/scala/hearth/kindlings/avroderivation/annotations/avroFixed.scala`

```scala
package hearth.kindlings.avroderivation.annotations
import scala.annotation.StaticAnnotation
final class avroFixed(val size: Int) extends StaticAnnotation
```

**Step 2 — Extend `AnnotationSupport` to extract integer literals:**

Current `AnnotationSupport` (file: `avro-derivation/src/main/scala/hearth/kindlings/avroderivation/internal/compiletime/AnnotationSupport.scala`) only has `extractStringLiteralFromAnnotation`. Add:

```scala
protected def extractIntLiteralFromAnnotation(annotation: UntypedExpr): Option[Int]

final def getAnnotationIntArg[Ann: Type](param: Parameter): Option[Int] =
  findAnnotationOfType[Ann](param).flatMap(extractIntLiteralFromAnnotation)
```

Update Scala 2 impl (`AnnotationSupportScala2.scala`): match `Literal(Constant(n: Int))` in annotation args.
Update Scala 3 impl (`AnnotationSupportScala3.scala`): match `Literal(IntConstant(n))` in annotation args.

**Step 3 — Schema rule:**

File: `avro-derivation/src/main/scala/hearth/kindlings/avroderivation/internal/compiletime/SchemaForMacrosImpl.scala`

In the case class field processing, when `@avroFixed(size)` is present on a field of type `Array[Byte]` or `ByteBuffer`:
- Generate `Schema.createFixed(fieldName, null, namespace, size)` instead of `Schema.create(Schema.Type.BYTES)`

**Step 4 — Encoder rule:**

When writing to a fixed-type schema, validate `bytes.length == size` at runtime, throw if mismatch.

**Step 5 — Decoder rule:**

When reading fixed-type schema, read fixed-size bytes.

### Tests

File: `AvroSchemaForSpec.scala` + `AvroRoundTripSpec.scala`:
- `@avroFixed(4)` field produces FIXED schema with size 4
- Encode/decode `Array[Byte]` of correct length
- Runtime error on wrong length

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

Requires the integer literal extraction from Gap 5 (`extractIntLiteralFromAnnotation`).

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
| `avro-derivation/src/main/scala/.../internal/compiletime/AnnotationSupport.scala` | Base trait: findAnnotationOfType, findTypeAnnotationOfType, extractStringLiteralFromAnnotation, hasAnnotationType, getAnnotationStringArg, getTypeAnnotationStringArg |
| `avro-derivation/src/main/scala-2/.../AnnotationSupportScala2.scala` | Scala 2: `param.asUntyped.symbol.annotations`, `Literal(Constant(s: String))` |
| `avro-derivation/src/main/scala-3/.../AnnotationSupportScala3.scala` | Scala 3: `quotes.reflect.*`, `Apply(_, List(Literal(StringConstant(value))))` |
| `avro-derivation/src/main/scala/.../annotations/` | avroDefault.scala, avroDoc.scala, avroNamespace.scala, fieldName.scala, transientField.scala |
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

1. **Gap 1** — Circe `Encoder.AsObject` (highest impact, unblocks Gap 13)
2. **Gap 5** — `@AvroFixed` (high priority, introduces `extractIntLiteralFromAnnotation` needed by Gap 9)
3. **Gap 6** — `@AvroProp` (medium, uses existing annotation pattern)
4. **Gap 7** — `@AvroAlias` (medium, needs `findAllAnnotationsOfType`)
5. **Gap 3** — `@stringified` (medium, self-contained)
6. **Gap 4** — Map as array (medium, self-contained)
7. **Gap 2** — Literal types (medium, Scala 3 only, needs Hearth API verification)
8. **Gap 8** — `@AvroError` (low, trivial with annotation infra from Gap 5)
9. **Gap 9** — `@AvroSortPriority` (low, uses int extraction from Gap 5)
10. **Gap 10** — ByteBuffer (low, pure type rule addition)
11. **Gap 11** — UTF-8 field names (low, tests only)
12. **Gap 13** — `Codec.AsObject` (low, depends on Gap 1)
13. **Gap 14** — `JsonCodec` combined (low)
14. **Gap 12** — Union types (blocked on Hearth)
