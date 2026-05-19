# Performance Regression Analysis: Kindlings vs Original Libraries

> **Date**: 2026-05-13
> **JVM**: GraalVM CE 24+36-jvmci-b01
> **Method**: Generated code inspection + bytecode (javap) + JFR profiling
> **Scala**: 3.8.3

## Summary of Findings

| Module | Gap | Root Cause | Fixability |
|--------|-----|------------|------------|
| **Jsoniter Read** | 0.45x | `Array[Any]` boxing, runtime `fieldNameMapper`, runtime config checks, lambda dispatch | Medium — requires codegen redesign |
| **Jsoniter Write** | 0.68x | Runtime `fieldNameMapper` per field, `writeKey` vs `writeNonEscapedAsciiKey`, runtime transient checks | Medium |
| **Avro Encode** | 0.17x | **Schema rebuilt on every encode call** | High — cache schema at init |
| **Pureconfig Read** | 0.65x | Extra field-name-mapping calls compound regex cost | Low-Medium |

---

## 1. Jsoniter Read — 15.1M vs 33.3M ops/s (0.45x)

### Evidence: Generated Code (Phase 1)

**Original** (`JsonCodecMaker.make[SimpleCC]`) generates:
```scala
var _name: String = null
var _age: Int = 0
var _active: Boolean = false
while (...) {
  l = in.readKeyAsCharBuf()
  if (in.isCharBufEqualsTo(l, "name")) { _name = in.readString(_name) }
  else if (in.isCharBufEqualsTo(l, "age")) { _age = in.readInt() }
  else if (in.isCharBufEqualsTo(l, "active")) { _active = in.readBoolean() }
  else in.skip()
}
new SimpleCC(_name, _age, _active)
```

**Kindlings** (`KindlingsJsonValueCodec.derive[SimpleCC]`) generates:
```scala
JsoniterDerivationUtils.readObject[SimpleCC](reader, 3, (arr: Array[Any]) => {
  new SimpleCC(
    unsafeCast[String](arr(0), ...),   // boxing: Object → String
    unsafeCast[Int](arr(1), ...),      // boxing: Object → unboxToInt
    unsafeCast[Boolean](arr(2), ...))  // boxing: Object → unboxToBoolean
}) { (fieldName, arr, reader) =>
  if (fieldName == config.fieldNameMapper("name")) {       // function call per field
    arr(0) = decodeFn.apply(reader).asInstanceOf[Any]      // box to Any
  } else if (fieldName == config.fieldNameMapper("age")) { // function call per field
    arr(1) = decodeFn.apply(reader).asInstanceOf[Any]      // box int → Integer
  } else if (fieldName == config.fieldNameMapper("active")) {
    arr(2) = decodeFn.apply(reader).asInstanceOf[Any]      // box boolean → Boolean
  } else if (config.skipUnexpectedFields) reader.skip()    // runtime config check
}
```

### Evidence: Bytecode (Phase 2)

Kindlings decode path contains:
- `invokevirtual JsoniterConfig.fieldNameMapper()` — 3 calls per object (one per field)
- `invokevirtual JsoniterConfig.checkFieldDuplication()` — 1 call per object
- `invokevirtual JsoniterConfig.skipUnexpectedFields()` — 1 call per object
- `invokestatic BoxesRunTime.unboxToInt()` — 1 call (Int field)
- `invokestatic BoxesRunTime.unboxToBoolean()` — 1 call (Boolean field)
- `invokestatic BoxesRunTime.boxToInteger()` — 1 call (storing Int → Array[Any])
- `invokestatic BoxesRunTime.boxToBoolean()` — 1 call (storing Boolean → Array[Any])
- `invokevirtual JsoniterDerivationUtils.readObject()` — higher-order dispatch with Function3

Original decode path contains NONE of the above. It uses:
- `invokevirtual JsonReader.readKeyAsCharBuf()` — reads key as char buffer (no String allocation)
- `invokevirtual JsonReader.isCharBufEqualsTo()` — compares against constant string
- Typed local vars — `istore`/`iload` for int, `iconst`/`istore` for boolean (zero boxing)
- Inline while loop — no function dispatch

### Evidence: JFR Profiling (Phase 3)

| Method | Kindlings Samples | Original Samples |
|--------|-------------------|------------------|
| `JsoniterDerivationUtils.readObject` | **525** | — |
| `JProcedure3.apply` (dispatch lambda) | **394** | — |
| `decode_SimpleCC` (field dispatch) | **193** | — |
| `readKeyAsString` / `readKeyAsCharBuf` | 299 | 210 |
| `d0` / codec decode body | — | **673** |
| `isCharBufEqualsTo` | — | 172 |
| Total unique method overhead | **~1,100 samples** in infrastructure | **~0 samples** in infrastructure |

### Root Causes (ranked by impact)

1. **`Array[Any]` boxing/unboxing** — Every primitive field (Int, Boolean) is boxed to `Object` for storage in `Array[Any]`, then unboxed via `BoxesRunTime.unboxToInt`/`unboxToBoolean`. The original uses typed local `var`s with zero boxing.

2. **Runtime `config.fieldNameMapper` per field** — `config.fieldNameMapper("name")` is a `Function1.apply` call on every field comparison, even when the mapper is `identity`. The original uses compile-time constant strings with `isCharBufEqualsTo`.

3. **Higher-order function dispatch** — `readObject` takes `Function1` (construct) and `Function3` (dispatch) closures. Each call goes through `JProcedure3.apply` → lambda → actual body. The original has an inline while loop.

4. **`readKeyAsString` vs `readKeyAsCharBuf`** — Kindlings reads each JSON key as a full `String` (heap allocation). The original reads into a reusable char buffer and compares in-place.

5. **Runtime config branch checks** — `config.checkFieldDuplication`, `config.skipUnexpectedFields`, `config.isStringified` are all evaluated at runtime per-field, even when they have their default values. The original bakes these at compile time.

---

## 2. Jsoniter Write — 42.8M vs 62.6M ops/s (0.68x)

### Evidence: Generated Code

**Original**:
```scala
out.writeObjectStart()
out.writeNonEscapedAsciiKey("name")   // compile-time constant, fast path
out.writeVal(x.name)
out.writeNonEscapedAsciiKey("age")
out.writeVal(x.age)
out.writeNonEscapedAsciiKey("active")
out.writeVal(x.active)
out.writeObjectEnd()
```

**Kindlings**:
```scala
jsonwriter.writeObjectStart()
if (!config.transientEmpty && x.name.isEmpty) { } else {  // runtime transient check
  jsonwriter.writeKey(config.fieldNameMapper("name"))       // runtime mapper + writeKey
  encode_String(x.name, jsonwriter, config)
}
jsonwriter.writeKey(config.fieldNameMapper("age"))           // runtime mapper
if (config.isStringified) writeValAsString(x.age) else encode_Int(x.age, ...) // runtime check
jsonwriter.writeKey(config.fieldNameMapper("active"))        // runtime mapper
encode_Boolean(x.active, jsonwriter, config)
jsonwriter.writeObjectEnd()
```

### Evidence: JFR Profiling

| Method | Kindlings | Original |
|--------|-----------|----------|
| `JsonWriter.writeKey(String)` | **401** | — |
| `JsonWriter.writeNonEscapedAsciiKey(String)` | — | **215** |
| `JProcedure2.apply` (lambda dispatch) | **124** | — |
| `encode_SimpleCC` | 602 | — |
| `e2` (encode body) | — | 514 |

### Root Causes

1. **`writeKey` vs `writeNonEscapedAsciiKey`** — Kindlings calls `JsonWriter.writeKey(String)` which must check for characters needing escaping. The original calls `writeNonEscapedAsciiKey(String)` which skips escape checking entirely since field names are compile-time known ASCII.

2. **Runtime `config.fieldNameMapper` per field** — 3 calls to `config.fieldNameMapper.apply("name")` per encode. Even with `identity`, this is a virtual dispatch + function application.

3. **Runtime transient checks** — `config.transientEmpty`, `config.transientNone`, `config.transientDefault` checked per field at runtime, generating branches even when all are `false`.

4. **Runtime `config.isStringified` check** — For numeric fields, kindlings generates both stringified and normal paths and selects at runtime.

---

## 3. Avro Encode — 7.9M vs 46.7M ops/s (0.17x)

### Evidence: JFR Profiling

| Method | Kindlings Samples | Original Samples |
|--------|-------------------|------------------|
| **`AvroDerivationUtils.setRecordFieldsFromList`** | **281** | — |
| **`Schema$Field.<init>`** | **166** | — |
| **`AvroDerivationUtils.createField`** | **129** | — |
| `Schema$RecordSchema.setFields` | 180 | — |
| `schema_...SimpleCC` (schema creation) | **338 + 250** | — |
| `encode_SimpleCC` | 304 | — |
| `RecordEncoder.encodeT` | — | 517 |
| `FieldEncoder.encode` | — | 216 |

### Root Cause

**Kindlings rebuilds the Avro schema on every encode call.** The JFR shows 338+250 = 588 samples in `schema_hearth_kindlings_benchmarks_SimpleCC$macro$2$lzyINIT1$1` and related schema construction methods. Every encode creates new `Schema.Field` objects, calls `setRecordFieldsFromList`, and allocates `HashMap`s for the field map.

The original (avro4s) creates the schema **once** during initialization and reuses it. Its hot path is just `RecordEncoder.encodeT` → `FieldEncoder.encode` — pure encoding with no schema overhead.

This is the single largest performance gap (6x) and the most fixable: cache the schema at derivation time instead of rebuilding it per call.

---

## 4. Pureconfig Read — 855K vs 1.32M ops/s (0.65x)

### Evidence: JFR Profiling

Both sides are dominated by pureconfig's `CapitalizedWordsNamingConvention.toTokens` regex operations (~60-70% of total time). The kindlings side has ~2,700 regex-related samples out of 3,800 total; the original has ~1,800 out of 2,700.

Key difference: kindlings appears to invoke `ConfigFieldMapping$.apply` more frequently (70 samples vs 32), suggesting it maps field names more times per decode. This compounds the regex cost.

### Root Cause

Kindlings calls `config.fieldNameMapper(fieldName)` at runtime for every field during matching, while the original (pureconfig-generic) uses `ProductHintImpl.from` which integrates field mapping more efficiently. The regex-heavy naming convention makes each extra call ~10x more expensive than a simple string comparison.

---

## Recommendations (ordered by impact)

### 1. Avro: Cache schema at initialization (HIGH impact, ~6x improvement potential)

The schema is deterministic — it depends only on the case class structure, which is known at compile time. Cache it in a `lazy val` inside the generated codec, not rebuild it per encode/decode call.

### 2. Jsoniter Read: Use typed local vars instead of `Array[Any]` (HIGH impact)

Replace `readObject` + `Array[Any]` + `unsafeCast` with generated typed local `var`s and an inline decode loop. This eliminates all boxing/unboxing.

### 3. Jsoniter Read/Write: Pre-compute field names at init (MEDIUM impact)

Instead of calling `config.fieldNameMapper("name")` on every read/write, pre-compute the mapped names once during codec initialization and store as `val`s. For the default `identity` mapper, this also eliminates the function call entirely.

### 4. Jsoniter Write: Use `writeNonEscapedAsciiKey` for known-safe field names (MEDIUM impact)

When the field name (or pre-computed mapped name) is known to be ASCII-safe at compile time, generate `writeNonEscapedAsciiKey` instead of `writeKey`.

### 5. Jsoniter Read: Use `readKeyAsCharBuf` + `isCharBufEqualsTo` (MEDIUM impact)

Replace `readKeyAsString()` (which allocates a `String`) with `readKeyAsCharBuf()` + `isCharBufEqualsTo()` (which compares in-place without allocation).

### 6. Jsoniter: Eliminate runtime config checks for default values (LOW-MEDIUM impact)

When config is the default `JsoniterConfig()`, all boolean flags are `false`. The generated code should either:
- Check once at codec initialization and branch to a fast path
- Or bake the known-false checks at compile time when no custom config is provided

### 7. Pureconfig: Cache field name mappings (LOW impact)

Pre-compute `fieldNameMapper(fieldName)` for all fields during codec initialization to avoid repeated regex-heavy naming convention calls.

---

---

## Post-Optimization Results

### hearth 0.3.0-31-SNAPSHOT (semiEval fix for Inlined module field access)

| Benchmark | Before | After | vs Original | Change |
|-----------|--------|-------|-------------|--------|
| **Jsoniter Write** SimpleCC | 42.8M | **62.7M** | 63.0M (**1.00x**) | **+47%, gap closed** |
| **Jsoniter Read** SimpleCC | 15.1M | **17.4M** | 35.0M (0.50x) | +15% |
| **Avro Encode** SimpleCC | 7.9M | **43.0M** | 48.6M (**0.88x**) | **+5.4x, gap nearly closed** |

### What worked

**Jsoniter Write** (0.68x → 1.00x): `semiEval` evaluates `JsoniterConfig.default` at compile time, enabling:
- `writeNonEscapedAsciiKey` with pre-computed constant field names (no `fieldNameMapper` calls)
- Skip transient field checks (statically known `false`)
- Skip `isStringified` dual-path generation (statically known `false`)

**Avro Encode** (0.17x → 0.88x): Schema passed from instance init into encode body via `encoderInstanceWithSchema`, eliminating per-encode schema rebuilding.

### What didn't work

**`readKeyAsCharBuf` + `isCharBufEqualsTo` through lambda dispatch**: `isCharBufEqualsTo(int, String)` requires passing the `Int` char buf length through the `(Int, Array[Any], JsonReader) => Unit` lambda, causing `Int` boxing on every field. The regression was worse than the original `readKeyAsString`. Reverted to `readObjectOptimized` with `readKeyAsString` + constant string comparison.

### Remaining gap

**Jsoniter Read** (0.50x): Still bottlenecked by `Array[Any]` boxing for case class field storage. Fixing this requires `ValDefBuilder.ofVar` for typed local variables + fully inlined decode loop (no `readObject*` helper). This is architecturally complex due to cross-quotes scoping.

---

### Round 2: Nested type optimizations (2026-05-19, hearth 0.3.0-33-SNAPSHOT)

Methodology: Per-benchmark JFR profiling (single fork), production-config verification (2f/5w/10m).

| Benchmark | Before | After | vs Original | Change |
|-----------|--------|-------|-------------|--------|
| **Avro Encode** Person | 1.7M | **2.9M** | 5.7M (0.51x) | **+70% — schema caching for nested records** |
| **Avro Decode** Person | 1.9M | **13.4M** | 4.3M (**3.1x faster**) | **+7x — eliminated per-decode decoderInstance creation** |
| **Avro Decode** SimpleCC | 53.8M | **470.9M** | 42.6M (**11.1x faster**) | **+8.7x** |
| **Jsoniter Read** Person | 2.7M | **3.2M** | 3.5M (0.93x) | **+19% — evaluatedConfig propagation to nested types** |

### What was fixed

**Avro Encode Person** (0.30x → 0.51x): Nested record schemas (e.g. Address inside Person) were rebuilt from scratch on every encode call. Fixed by caching schemas as `lazy val`s in the encoder's outer `ValDefsCache` scope, using an `outerConfig` reference to avoid cross-scope config parameter issues. JFR confirmed 52% of samples were in schema reconstruction (regex Pattern compilation, Schema.Field creation, setRecordFieldsFromList). The remaining 0.51x gap is architectural: Kindlings uses def-cached encode methods per type while avro4s uses array-based FieldEncoder dispatch.

**Avro Decode Person** (0.45x → 3.1x faster): Each field in `decode_Person` created a fresh `AvroDecoderFactories.decoderInstance[Field]` wrapper — with a new schema + lambda — on every decode call. For Person's 6 fields, that was 6 decoderInstance allocations + 6 schema derivations per decode. Fixed by calling the decode def directly from the case class rule instead of wrapping each field in a decoderInstance. The field decode defs are already cached via `setHelper`/`getHelper`.

**Jsoniter Read Person** (0.73x → 0.93x): `deriveFieldDecoder` created a new `DecoderCtx` without `evaluatedConfig`, causing nested case class types (Address) to fall back to the non-optimized decode path (readObject + Array[Any] + lambda dispatch + readKeyAsString). Fixed by passing `ctx.evaluatedConfig` through to the nested context. The remaining 0.93x gap is from collection/map decoding using runtime helpers (`readCollection`, `readMap`) with lambda dispatch vs jsoniter-scala's fully inlined loops.

**Jsoniter Write Person** (0.90x → 0.87x, unchanged): JFR analysis confirmed the gap is architectural: def-cached encode methods + runtime helpers (`writeArray`, `writeMapStringKeyed`) vs jsoniter-scala's single-method inlining. No surgical fix available. At 0.87x, this is within the "at parity" target for jsoniter.

**Cats Order/Eq**: Re-benchmarked with production config. Order 2.13 gap is 0.90x (not 0.83x as previously documented — original has ±32M variance). Eq Scala 3 is actually 1.11x faster (not 0.91x). Both are JIT-sensitive at 100M-400M+ ops/s where JMH measurement noise dominates. No code fix needed.

### Key files changed

- `EncoderMacrosImpl.scala`: Added `outerConfig` field to `EncoderCtx`, `getCachedSchemaForEncode`/`setCachedSchemaForEncode` methods
- `AvroEncoderHandleAsCaseClassRule.scala`: Modified `cachedSchemaForEncode` to use encoder cache with outer config
- `AvroEncoderHandleAsCollectionRule.scala`: Inlined collection encoding (eliminated `encodeIterable` helper)
- `AvroEncoderHandleAsMapRule.scala`: Eliminated tuple allocation in map encoding
- `DecoderMacrosImpl.scala`: Added `getCachedFieldDecoder`/`setCachedFieldDecoder` methods
- `AvroDecoderHandleAsCaseClassRule.scala`: Call decode defs directly instead of creating decoderInstance wrappers
- `DecoderHandleAsCaseClassRule.scala` (jsoniter): Pass `evaluatedConfig` through `deriveFieldDecoder`

---

## Remaining Gaps — Root Causes and Fix Strategies

### Avro Encode Person (0.51x vs avro4s)

**JFR evidence** (hot path only, schema init excluded):

| Kindlings hot spot | Samples | avro4s equivalent | Samples |
|---|---|---|---|
| `encode_Person` (6 field puts) | 110 | `RecordEncoder.encodeT` (array loop) | 673+69 |
| `encode_Address` (4 field puts) | 54 | `FieldEncoder.encode` (virtual dispatch) | 424+124 |
| `encode_String/Int/Double` (identity or box) | 433+142 | Magnolia `Param.deref` | 342 |
| `encode_List.anonfun` (lambda per Address) | 82 | `CollectionEncoders.encode.anonfun` | 54 |
| `encode_Map.anonfun` (lambda + foreach) | 75 | `MapEncoder.encode.anonfun` | 189 |
| `HashMap.put/putVal` (map construction) | 153+89 | `HashMap.put/putVal` | 213+103 |
| `ArrayList.add` (list/vector construction) | 122+66+56 | `Array.copyOf` (record array) | 133×3 |
| `List.foreach` / `Map.foreach` | 79+76 | `List.map` / `Map.foreach` | 103+81+124 |

**Root causes (ranked by impact):**

1. **`encode_String` is a def call that does `string.asInstanceOf[Any]` — 433 samples.** For String and Int fields, the encode def is an identity cast wrapped in a method call. avro4s's `FieldEncoder.encode` skips the method for primitive/string fields and puts them directly into the record. **Fix strategy**: add an "inline built-in encode" rule (analogous to jsoniter's `inlineBuiltInDecode`) that emits `record.put(idx, person.name)` directly for String/Int/Long/Double/Boolean fields, bypassing the def-cached encode method entirely.

2. **`encode_Address` creates `new GenericData.Record(schema)` per Address — 54 samples in the method, but the `Record` constructor + `record.put` overhead compounds.** avro4s's `RecordEncoder.encodeT` uses a pre-allocated `FieldEncoder[]` array and iterates with an index. **Fix strategy**: this is architectural. The def-cached approach creates a method call per nested record. Making the encode loop index-based (like avro4s) would require a different codegen pattern that doesn't use `CaseClass.caseFieldValuesAt`.

3. **Lambda dispatch for collection/map encoding — `encode_List.anonfun` (82), `encode_Map.anonfun` (75), `JProcedure` overhead implicit.** The `LambdaBuilder.of1` pattern creates a `Function1` that wraps the encode def call. Each collection element goes through `lambda.apply(item)` → `encode_Address(item, config)`. **Fix strategy**: eliminate the `LambdaBuilder` layer for collection encoding. Instead of `list.foreach(item => lambda.apply(item))`, generate `list.foreach(item => encode_Address(item, config))` directly. This requires the collection rule to directly reference the def-cached encode method without the `LambdaBuilder` indirection.

4. **Unused `AvroConfig` parameter passed on every encode call.** Every `encode_X(value, avroconfig)` def takes an `AvroConfig` parameter even when the config is not used in the body (e.g., `encode_String` just returns `string.asInstanceOf[Any]`). The JIT may or may not eliminate this. **Fix strategy**: split encode defs into config-dependent and config-independent variants. Config-independent defs (builtins) take only the value.

### Jsoniter Read Person (0.93x vs jsoniter-scala)

**JFR evidence:**

| Kindlings hot spot | Samples | jsoniter-scala equivalent | Samples |
|---|---|---|---|
| `JProcedure3.apply` (readObject dispatch) | 198+188 | (none — inlined) | 0 |
| `readCollection` (List[Address] decode) | 184+107+35+19+18+17 | `d6` (inline loop) | 284 |
| `readMap` (Map decode) | 61+58+26+19 | `d5` (inline loop) | 213 |
| `decode_List.anonfun` (per-element lambda) | 143 | (none — inlined) | 0 |
| `readKeyAsString` | 89+82 | `readKeyAsCharBuf` | 138 |

**Root causes (ranked by impact):**

1. **`readCollection` / `readMap` runtime helpers use `Function1` lambda dispatch — ~580 samples total in Kindlings infrastructure vs 0 in original.** jsoniter-scala generates an inline `while` loop: `while (in.isNextToken(',')) { arr(i) = decodeElement(in); i += 1 }`. Kindlings calls `JsoniterDerivationUtils.readCollection(reader, decodeFn, factory, maxLen)` which iterates with `factory.newBuilder` + `while (in.isNextToken(',')) builder += decodeFn.apply(in)`. **Fix strategy**: generate inline collection/map decode loops in the optimized path. When `evaluatedConfig` is available, emit a `while` loop with direct decode calls instead of delegating to `readCollection`/`readMap`. This is the same pattern as `decodeCaseClassFieldsOptimized` — extend it to handle collection/map fields inline.

2. **`readKeyAsString` for nested Address fields — 171 samples.** The top-level Person uses `readKeyAsCharBuf` (optimized path), but the `readCollection` helper for `List[Address]` internally calls `decode_Address`, which uses `readKeyAsCharBuf` for Address's own fields (since evaluatedConfig now propagates). However, the `readObject` call inside `decode_Address` still uses `readKeyAsString` because... wait, let me re-check. The 189 `readKeyAsString` samples might be from the non-optimized path of the PREVIOUS JFR run (before the evaluatedConfig fix). The current run should have `readKeyAsCharBuf` for Address too. **Action**: re-run JFR after the fix to confirm this is resolved.

3. **`Factory`-based collection builder vs direct `ListBuffer`/`VectorBuilder`.** The `readCollection` helper uses `scala.collection.Factory[Item, Coll]` which goes through the generic builder pattern. jsoniter-scala knows the exact collection type at compile time and uses the specific builder. **Fix strategy**: when generating inline collection decode loops, use the specific builder type (e.g., `ListBuffer[Address]` for `List[Address]`) instead of the generic `Factory`.

### Jsoniter Write Person (0.87x vs jsoniter-scala)

**JFR evidence:**

| Kindlings hot spot | Samples | jsoniter-scala equivalent | Samples |
|---|---|---|---|
| `encode_String` (def call for each String field) | 433 | (inlined `writeVal(x.name)`) | 0 |
| `writeMapStringKeyed` (runtime helper) | 179 | `e12` (inline loop) | 33 |
| `encode_Double` (def call for each Double) | 142 | (inlined `writeVal(x.score)`) | 0 |
| `JProcedure1/2.apply` (lambda dispatch) | 132+124 | `JProcedure2.apply` | 96 |
| `writeArray` (runtime helper) | 111 | `e10`/`e13` (inline loops) | 106+112 |
| `encode_Address` (def call) | 70 | `e11` (inline body) | 366 |
| `encode_Option.anonfun` + `Option.fold` | 65+40 | (null check) | 0 |
| `JFunction1$mcVD$sp.apply` (Double boxing) | 54 | (none — typed) | 0 |

**Root causes (ranked by impact):**

1. **`encode_String` / `encode_Double` / `encode_Int` are def calls that just delegate to `writeVal` — 433+142 = 575 samples.** jsoniter-scala inlines `out.writeNonEscapedAsciiKey("name"); out.writeVal(x.name)` directly. Kindlings generates `out.writeNonEscapedAsciiKey("name"); encode_String(x.name, out, config)` where `encode_String` is `def encode_String(s, w, c) = w.writeVal(s)`. The extra method call + config parameter passing prevents full JIT inlining. **Fix strategy**: add "inline built-in encode" for the encoder optimized path. When `evaluatedConfig` is available and the field type is a built-in (String, Int, Long, Double, Boolean, Float), emit `writer.writeVal(x.fieldName)` directly instead of calling the def-cached encode method.

2. **`writeMapStringKeyed` / `writeArray` runtime helpers — 179+111 = 290 samples.** These are generic runtime methods that take `Function1` lambdas for per-element encoding. jsoniter-scala generates inline loops. **Fix strategy**: generate inline write loops in the optimized encoder path. When encoding a collection field, emit `out.writeArrayStart(); iterable.foreach(item => encode_Item(item, out, config)); out.writeArrayEnd()` directly in the case class encoder body instead of delegating to `writeArray`.

3. **`Option.fold` for Option encoding — 40+65 = 105 samples.** Kindlings uses `option.fold(writeNull)(item => encode_String(item, out, config))`. jsoniter-scala generates `if (x.email ne null) { writeKey("email"); writeVal(x.email) }` (for non-None) or similar pattern. `Option.fold` allocates two lambdas (one for empty, one for Some). **Fix strategy**: generate `if (option.isDefined) { writeKey("email"); encode_String(option.get, out, config) } else { writeKey("email"); out.writeNull() }` or use pattern matching.

4. **`JFunction1$mcVD$sp.apply` for Double encoding — 54 samples.** This is boxing of the `Double` primitive when passing through `Function1[Double, Unit]`. The `writeArray` helper takes `Function1[Item, Unit]` which boxes primitives. **Fix strategy**: inline write loops eliminate this (see #2 above), or add specialized `writeArrayDouble` helper.

### Cats Order SimpleCC (0.90x on Scala 2.13)

**JFR evidence**: At 384M+ ops/s, the operation takes ~2.6ns. The JFR captures almost no samples in the actual compare code — 99.5% of samples are in the JMH harness (`_Throughput`, `_jmhStub`). Only 16 samples total in Kindlings compare code (`compare_SimpleCC`, `order_String`). The original has 2 samples in `MkOrderDerivation.compare`.

**Root cause**: The gap (0.90x, within ±8% error on the original) is **JIT optimization sensitivity**, not a code-level inefficiency. At this throughput, the JIT compiler's inlining decisions for the compare method chain dominate. Kindlings generates `val c = String.compare(x.name, y.name); if (c != 0) c else { val c2 = Int.compare(x.age, y.age); ... }` through def-cached methods with `LazyRef` parameters. The original (kittens/Shapeless) generates a similar chain through `MkOrderDerivation` + `HCons` recursion. Both are trivial for the JIT to inline, but the exact inlining depth and register allocation differ.

**Fix strategy**: no code fix warranted. The gap is within measurement noise (original has ±33M error, i.e. ±8%). If this becomes a concern, profile with `-prof perfasm` to compare generated assembly.

---

## Hearth Utilities Validation Status

| Utility | Status | Notes |
|---------|--------|-------|
| `Expr.semiEval` | **WORKS** (hearth 0.3.0-31+) | Fixed for `Inlined` module field access. Evaluates `JsoniterConfig.default` at compile time. |
| `ValDefBuilder.ofVar` | **NOT TESTED** | Not needed for the current optimizations; will be needed for typed-var decode path |
| `ValDefs.createLazy` | **WORKS** | Tested but not useful for cross-scope caching (lazy val inside lambda is per-invocation) |
| `encoderInstanceWithSchema` | **WORKS** | Factory for passing schema to encode body |
| `ValDefsCache` schema caching | **WORKS** | Used to cache nested record schemas in encoder's outer scope |
| hearth 0.3.0-33-SNAPSHOT | **WORKS** | All tests pass on both Scala 2.13 and 3 |
