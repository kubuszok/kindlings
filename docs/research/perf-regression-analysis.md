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

## Hearth Utilities Validation Status

| Utility | Status | Notes |
|---------|--------|-------|
| `Expr.semiEval` | **WORKS** (hearth 0.3.0-31) | Fixed for `Inlined` module field access. Evaluates `JsoniterConfig.default` at compile time. |
| `ValDefBuilder.ofVar` | **NOT TESTED** | Not needed for the current optimizations; will be needed for typed-var decode path |
| `ValDefs.createLazy` | **WORKS** | Tested but not useful for cross-scope caching (lazy val inside lambda is per-invocation) |
| `encoderInstanceWithSchema` | **WORKS** | New factory for passing schema to encode body |
| hearth 0.3.0-31-SNAPSHOT | **WORKS** | All 235 jsoniter + 284 avro tests pass on both Scala 2.13 and 3 |

---

## Raw Data Locations

| Artifact | Path |
|----------|------|
| Kindlings derivation log (all 3 modules) | `/tmp/kindlings-derivation-log-scala3.txt` |
| Kindlings jsoniter SimpleCC generated code | `/tmp/kindlings-jsoniter-SimpleCC-codec-scala3.txt` |
| Original jsoniter SimpleCC generated code | `/tmp/original-jsoniter-debug-scala3.txt` |
| Kindlings jsoniter bytecode | `/tmp/bytecode-kindlings-jsoniter.txt` |
| Original jsoniter bytecode (anon classes) | `/tmp/bytecode-original-jsoniter.txt` |
| Original jsoniter bytecode (companion) | `/tmp/bytecode-original-jsoniter-companion.txt` |
| JFR profiles | `/tmp/jfr/*/profile.jfr` |

## Benchmark Results (this run, JDK 24 GraalVM CE, f=1)

| Benchmark | Kindlings ops/s | Original ops/s | Ratio |
|-----------|----------------|----------------|-------|
| Jsoniter Read SimpleCC | 15,149,180 | 33,340,322 | 0.45x |
| Jsoniter Write SimpleCC | 42,845,903 | 62,589,268 | 0.68x |
| Avro Encode SimpleCC | 7,889,684 | 46,665,052 | 0.17x |
| Pureconfig Read SimpleCC | 854,836 | 1,322,197 | 0.65x |
