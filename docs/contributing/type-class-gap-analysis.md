# Type Class Gap Analysis: Missing Type Classes & Encoder Hierarchies

Investigation date: 2026-02-26

The original [gap-analysis.md](gap-analysis.md) focused on **which types/patterns** can be derived. This document focuses on **which type classes** are derived vs missing, and whether we're covering the full API surface of each library.

---

## Executive Summary

| Library | Missing Type Classes | Impact |
|---------|---------------------|--------|
| **Circe** | `KeyEncoder[A]`, `KeyDecoder[A]`, `Encoder.AsObject[A]`, `Codec.AsObject[A]` | **High** — AsObject is what all other Circe derivation libs produce; KeyEncoder/KeyDecoder needed for `Map[NonString, V]` |
| **Jsoniter** | `JsonKeyCodec[A]`, `JsonCodec[A]` (combined) | **Medium** — JsonKeyCodec needed for `Map[NonString, V]`; JsonCodec is convenience |
| **Avro** | None | N/A — Avro maps require String keys by spec; current 3 type classes are complete |
| **YAML** | None | N/A — YAML has no key codec concept; String-only map keys are correct |

---

## 1. Circe: `Encoder.AsObject` — HIGH PRIORITY

### The Problem

Circe's own derivation (`circe-generic`, `circe-derivation`, Scala 3 `derives`) **always** returns `Encoder.AsObject[A]` for case classes and sealed traits. Kindlings returns `Encoder[A]` — a **strictly weaker type**.

```scala
// Circe-generic (what users are migrating from):
implicit val enc: Encoder.AsObject[Foo] = deriveEncoder[Foo]

// Kindlings (what they get after migration):
implicit val enc: KindlingsEncoder[Foo] = KindlingsEncoder.derived[Foo]
// KindlingsEncoder extends Encoder[A], NOT Encoder.AsObject[A]
```

### Why It Matters

1. **`mapJsonObject`** — Users who post-process derived encoders (add/remove fields, merge metadata) need `Encoder.AsObject` to call `mapJsonObject`. With plain `Encoder`, they only get `mapJson`.
2. **`Codec.AsObject` composition** — Common pattern: `Codec.AsObject[A]` from `Encoder.AsObject[A] with Decoder[A]`. Without `AsObject`, this doesn't compose.
3. **Type-level guarantees** — Any code/library that requires `Encoder.AsObject[A]` in implicit positions won't find Kindlings-derived encoders.
4. **Circe's documented Scala 3 idiom** — `case class Foo(...) derives Encoder.AsObject` is the recommended pattern.

### Circe's Full Encoder Hierarchy

```
Encoder[A]                     -- A => Json (any JSON value)
  └─ Encoder.AsRoot[A]        -- guarantees array or object output
       ├─ Encoder.AsArray[A]  -- A => Vector[Json], wrapped as JSON array
       └─ Encoder.AsObject[A] -- A => JsonObject, wrapped as JSON object
```

`Decoder[A]` has **no** similar hierarchy — it's flat. No changes needed for `KindlingsDecoder`.

### Current Implementation Gap

The macro already produces JSON objects for case classes internally — `CirceDerivationUtils.jsonFromFields` calls `Json.fromJsonObject(JsonObject.fromIterable(fields))`. The `JsonObject` is created and immediately wrapped. The fix is to expose this through the type system.

### Implementation Approach

Option A (recommended): Make `KindlingsEncoder[A]` extend `Encoder.AsObject[A]` when derived for case classes and sealed traits. Add `encodeObject(a: A): JsonObject` to the generated code. For value types, options, collections — keep producing `Encoder[A]`.

Option B: Introduce `KindlingsEncoder.AsObject[A]` as a separate subtype, with a dedicated `deriveAsObject[A]` entry point.

### Codec.AsObject

Circe's `Codec.AsObject[A]` extends `Decoder[A] with Encoder.AsObject[A]`. Kindlings doesn't have a `KindlingsCodec` at all — users compose encoder + decoder separately. Lower priority since the pattern is `implicit val codec = (KindlingsEncoder.derived[Foo], KindlingsDecoder.derived[Foo])` — but `Codec.AsObject` would be a convenience.

---

## 2. Circe: `KeyEncoder[A]` / `KeyDecoder[A]` — MEDIUM-HIGH PRIORITY

### The Problem

Both `EncHandleAsMapRule` and `DecHandleAsMapRule` in circe-derivation immediately reject non-String map keys:

```scala
// EncoderMacrosImpl.scala:406, DecoderMacrosImpl.scala:594
if (!(Key <:< Type[String]))
  MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
```

`Map[Int, V]`, `Map[UUID, V]`, `Map[Long, V]` etc. all fall through to implicit summoning. Circe's built-in `encodeMapLike` requires `KeyEncoder[K]` — it *may* resolve through Circe's own companion implicits, but the behavior is fragile and untested.

### What Circe Provides

```scala
trait KeyEncoder[A] { def apply(key: A): String }
trait KeyDecoder[A] { def apply(key: String): Option[A] }
```

Built-in instances for: `String`, `Symbol`, `UUID`, `URI`, `Byte`, `Short`, `Int`, `Long`, `Double`.

Both are in `circe-core` (already a dependency).

### Unused Runtime Helper

`CirceDerivationUtils.encodeMapEntries[K, V]` (line 47) already accepts a generic `K => String` key encoder parameter but is **never called** from the macro. The infrastructure was partially anticipated but never completed.

### Implementation Approaches

**Approach A: Summon KeyEncoder/KeyDecoder in map rules (recommended)**
- In `EncHandleAsMapRule`: when `Key` is not `String`, try `Expr.summonImplicit[KeyEncoder[Key]]`. If found, use the existing `encodeMapEntries` runtime helper.
- In `DecHandleAsMapRule`: when `Key` is not `String`, try `Expr.summonImplicit[KeyDecoder[Key]]`. If found, create a `decodeMapWithKeyDecoder` runtime helper.
- ~15-20 lines per rule. Tests: `Map[Int, String]`, `Map[UUID, String]` round-trips.

**Approach B: Derive KindlingsKeyEncoder / KindlingsKeyDecoder**
- Full type class derivation for key encoders (enums → string names, value classes → inner type).
- Most comprehensive but significant work; not justified unless there's demand.

---

## 3. Jsoniter: `JsonKeyCodec[A]` — MEDIUM PRIORITY

### The Problem

Jsoniter-scala defines **three** codec type classes:

```scala
trait JsonValueCodec[A]  // value encoding/decoding
trait JsonKeyCodec[A]    // key encoding/decoding
trait JsonCodec[A] extends JsonValueCodec[A] with JsonKeyCodec[A]  // combined
```

Kindlings only derives `JsonValueCodec[A]`. Map handling has the same String-only guard as Circe.

### How Jsoniter Handles Non-String Keys Natively

`JsonReader` has `readKeyAsInt()`, `readKeyAsLong()`, `readKeyAsUUID()`, `readKeyAsBigDecimal()`, plus all `java.time` types. `JsonWriter` has matching `writeKey(int)`, `writeKey(long)`, etc. overloads. Jsoniter-scala's own `JsonCodecMaker.make` uses these internally for non-String map keys.

### Implementation Approaches

**Approach A: Built-in key type support in map rules (recommended)**
- Extend `EncHandleAsMapRule` with type-matching chain for built-in key types (`Int` → `writeKey(int)`, `Long` → `writeKey(long)`, etc.)
- Same for `DecHandleAsMapRule` (`Int` → `readKeyAsInt()`, etc.)
- Similar to how `EncHandleAsBuiltInRule` maps types to write methods.

**Approach B: Summon `JsonKeyCodec[K]`**
- For arbitrary key types, summon implicit `JsonKeyCodec[K]` and use `encodeKey`/`decodeKey`.
- More extensible but requires `JsonKeyCodec` instances to exist.

**Approach C: Both A + B**
- Built-in support for primitives/common types, fallback to `JsonKeyCodec[K]` summoning for custom types.

### `JsonCodec[A]` (Combined)

`JsonCodec[A]` extends both `JsonValueCodec[A]` and `JsonKeyCodec[A]`. Lower priority — users rarely need combined derivation, and the value codec is the primary use case.

---

## 4. Avro: No Gaps — String Keys by Spec

The Avro specification defines maps as:

> Map keys are assumed to be strings.

`Schema.createMap(valueSchema)` has no key schema parameter. The `Key <:< Type[String]` guard is correct and complete. **No KeyEncoder/KeyDecoder concept exists or is needed for Avro.**

The three current type classes (`AvroSchemaFor`, `AvroEncoder`, `AvroDecoder`) fully cover Avro's type class surface.

---

## 5. YAML: No Gaps — String Keys Correct

The `scala-yaml` library (`org.virtuslab:scala-yaml`) has flat `YamlEncoder[A]` / `YamlDecoder[A]` traits with no key codec concept and no encoder hierarchy. String-only map keys are the correct behavior for YAML serialization. **No gaps.**

---

## 6. Cross-Cutting: Map Key Handling Summary

| Module | String keys | Non-String keys | Library support for non-String | Action needed |
|--------|------------|----------------|-------------------------------|---------------|
| **Circe** | Works | Yields, may fallback to implicit | `KeyEncoder[K]` / `KeyDecoder[K]` | Summon KeyEncoder/KeyDecoder in map rules |
| **Jsoniter** | Works | Yields silently | `JsonKeyCodec[K]` + built-in `readKeyAs*`/`writeKey(*)` | Built-in type support + JsonKeyCodec summoning |
| **Avro** | Works | Correctly rejected | None (Avro spec: keys are always strings) | None |
| **YAML** | Works | Correctly rejected | None | None |

---

## 7. Priority Ranking

1. **Circe `Encoder.AsObject`** — High. All other Circe derivation libraries produce `AsObject`. Users migrating to Kindlings get a weaker type. The macro already produces objects internally.
2. **Circe `KeyEncoder`/`KeyDecoder` in map rules** — Medium-High. Enables `Map[Int, V]`, `Map[UUID, V]` etc. Partial infrastructure already exists (unused `encodeMapEntries` helper).
3. **Jsoniter non-String map keys** — Medium. Built-in `readKeyAs*`/`writeKey(*)` methods cover primitives, UUID, java.time. Need type-matching in map rules.
4. **Jsoniter `JsonKeyCodec` derivation** — Low. Standalone key codec derivation is rarely needed by users.
5. **Circe `Codec.AsObject`** — Low. Convenience type, users can compose encoder + decoder.
6. **Jsoniter `JsonCodec` (combined)** — Low. Rarely needed.

---

## Related: Existing Gap Analysis Items Affected

These items in [gap-analysis.md](gap-analysis.md) are related to findings here:

- **Map as array encoding (Jsoniter)** — Alternative approach to non-String keys; encode maps as `[[k,v],...]` arrays. Complementary to JsonKeyCodec support.
- **`@stringified` (Jsoniter)** — Separate concern (value encoding), not key encoding.

---

## Research Sources

- Circe encoder hierarchy: `io.circe.Encoder`, `Encoder.AsObject`, `Encoder.AsRoot`, `Encoder.AsArray` in circe-core
- Circe derivation returns `Encoder.AsObject`: confirmed in circe-generic semiauto, circe-derivation, Scala 3 `derives`
- Circe `KeyEncoder`/`KeyDecoder`: `io.circe.KeyEncoder`, `io.circe.KeyDecoder` in circe-core
- Jsoniter `JsonKeyCodec`: `com.github.plokhotnyuk.jsoniter_scala.core.JsonKeyCodec` in jsoniter-scala-core
- Jsoniter `JsonReader.readKeyAs*` / `JsonWriter.writeKey(*)`: built-in overloads for primitives, UUID, java.time
- Avro spec: "Map keys are assumed to be strings" — Apache Avro 1.12.1 specification
- Downstream Circe users (tapir, http4s-circe): require `Encoder[A]` not `Encoder.AsObject[A]` — but `AsObject` is a subtype so it satisfies both
- Existing Kindlings files: `EncoderMacrosImpl.scala`, `DecoderMacrosImpl.scala`, `CodecMacrosImpl.scala`, `CirceDerivationUtils.scala`, `JsoniterDerivationUtils.scala`
- Unused helper: `CirceDerivationUtils.encodeMapEntries[K, V]` at line 47 — accepts `K => String` but never called from macro
