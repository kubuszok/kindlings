# Skill: Runtime Performance Optimization for Derivation Modules

Use this skill when implementing or optimizing the **runtime performance** of macro-generated type class instances — codecs, encoders, decoders, schemas. These patterns apply to ALL derivation modules, not just jsoniter.

**Reference implementations**:
- Encoder: `jsoniter-derivation/.../EncoderHandleAsCaseClassRule.scala` (semiEval + writeNonEscapedAsciiKey)
- Decoder: `jsoniter-derivation/.../DecoderHandleAsCaseClassRule.scala` (typed vars + sentinel loop)
- Schema caching: `avro-derivation/.../AvroEncoderHandleAsCaseClassRule.scala` (precomputedSchema)

**Benchmark data**: `docs/research/perf-regression-analysis.md`, `docs/research/jsoniter-codegen-techniques.md`

---

## 1. Evaluate config at compile time with `semiEval`

Every module that takes a config parameter (`JsoniterConfig`, `Configuration`, `AvroConfig`, `YamlConfig`, etc.) should evaluate it at compile time using `Expr.semiEval` and store the result in the derivation context.

```scala
val evConfig: Option[MyConfig] = configExpr.semiEval.toOption
```

**What this enables**:
- Pre-compute field names: `evConfig.map(_.fieldNameMapper("myField"))` → compile-time constant `"my_field"`
- Skip boolean checks: if `transientDefault == false`, don't generate the `if (config.transientDefault)` branch at all
- Eliminate runtime function calls: no `config.fieldNameMapper(name)` per field per encode/decode

**Pattern**: Store evaluated config in the encoder/decoder context as `evaluatedConfig: Option[MyConfig] = None`. Pass it through `from()`, `nest()`, `nestInCache()`. Rules check `ctx.evaluatedConfig` to decide between optimized and fallback paths.

**When semiEval fails**: Fall back to the current runtime-checked codegen. The user gets a compiler warning suggesting they use a simpler config expression.

---

## 2. Use typed local vars instead of `Array[Any]` for field accumulation

When decoding a case class from a serialized format, use `ValDefs.createVar[FieldType]` to create typed local variables for each field. This eliminates boxing of primitives (`Int`, `Boolean`, `Long`, etc.) through `Array[Any]`.

**Before** (boxes primitives):
```scala
val arr = new Array[Any](fieldCount)
arr(0) = reader.readString(null)    // OK — String is reference
arr(1) = reader.readInt()           // BOXES Int → Integer
arr(2) = reader.readBoolean()       // BOXES Boolean → java.lang.Boolean
new SimpleCC(arr(0).asInstanceOf[String], arr(1).asInstanceOf[Int], ...)  // UNBOXES
```

**After** (zero boxing):
```scala
var _name: String = null    // reference type — no boxing
var _age: Int = 0           // primitive — no boxing
var _active: Boolean = false // primitive — no boxing
// ... decode loop ...
new SimpleCC(_name, _age, _active)  // direct — no casts
```

**Implementation with hearth**:
```scala
// Inside parTraverse over fields (where Field type is in scope):
val fieldVar = ValDefs.createVar[Field](deriveZeroValue[Field], s"_$fName")
fieldVar.map { case (getter, setter) =>
  val setFromReader: Expr[JsonReader] => Expr[Unit] = { readerExpr =>
    setter(decodeField(readerExpr))
  }
  (fName, mappedName, getter.as_??, setFromReader)
}
```

**Compose multiple vars** using `ValDefs` applicative:
```scala
val allVars = varDefsList.foldLeft(ValDefsTraverse.pure(List.empty)) { (acc, varDef) =>
  acc.map2(varDef) { case (list, entry) => list :+ entry }
}
allVars.use { fieldInfos => /* build loop + constructor */ }
```

---

## 3. Use the `l < 0` sentinel loop (NEVER duplicate the dispatch chain)

When generating a field-reading loop, use the jsoniter-scala sentinel pattern:

```scala
var _l: Int = -1
while (_l < 0 || reader.isNextToken(',')) {
  _l = reader.readKeyAsCharBuf()
  // dispatch on _l — written ONCE
}
```

The `_l < 0` condition ensures the first iteration runs without checking for a comma. This means the dispatch chain (if-else over field names) appears **once** in the method body, not twice.

**Why this matters**: Duplicating the dispatch chain doubles the method bytecode size. Large methods (>~200 bytecode instructions) are poorly optimized by the JIT — it may skip inlining, escape analysis, or other optimizations. The sentinel pattern keeps the method compact.

**Measured impact**: Without sentinel loop (duplicated dispatch): 12M ops/s. With sentinel loop: 35M ops/s. The JIT optimization threshold is the difference.

The `_l` variable should also be a `ValDefs.createVar[Int]` so it's accessible from the spliced dispatch chain:

```scala
val lenVar = ValDefs.createVar[Int](Expr(-1), "_l")
val combined = allFieldVars.map2(lenVar) { ... }
combined.use { case (fieldInfos, lenGetter, lenSetter) =>
  // dispatch references lenGetter via Expr.splice(lenGetter)
  // loop body calls lenSetter to update _l
}
```

---

## 4. Inline built-in type decoding

For built-in types (`Int`, `Boolean`, `String`, `Long`, `Double`, etc.), generate direct reader calls instead of going through cached def function indirection.

```scala
// Instead of:  _age = decodeFn.apply(reader)    // function call → cached def → reader.readInt()
// Generate:    _age = reader.readInt()           // direct call, zero indirection
```

**Guard**: Only inline when no user-provided implicit codec exists for the type. Check with `summonJsonValueCodecCached[Field]` — if it returns `Right`, a user codec exists and must be used instead.

```scala
val inlinedDecode: Option[Expr[JsonReader] => Expr[Field]] =
  if (canInline)
    inlineBuiltInDecode[Field](dummyReader).flatMap { _ =>
      summonJsonValueCodecCached[Field] match {
        case Right(_) => None        // user-provided codec — don't inline
        case Left(_)  => Some(...)   // no user codec — inline the reader call
      }
    }
  else None
```

---

## 5. Use `readKeyAsCharBuf` + `isCharBufEqualsTo` for field name matching

Avoid `readKeyAsString()` which allocates a `String` per field per decode. Instead use `readKeyAsCharBuf()` which stores the key bytes internally in the reader's reusable buffer, and `isCharBufEqualsTo(len, "name")` for comparison.

This only works in the inline loop pattern (technique #3) because `isCharBufEqualsTo` requires the buffer length as a parameter, which must be a local variable accessible from the dispatch chain — not passable through a lambda without boxing.

---

## 6. Use `writeNonEscapedAsciiKey` for known-safe field names

When the field name is known at compile time (via semiEval) and contains only printable ASCII characters (no `"`, no `\`, codepoints 0x20-0x7e), use `writeNonEscapedAsciiKey` instead of `writeKey`. This skips the escape-checking logic.

```scala
val isAsciiSafe = name.forall(c => c >= 0x20 && c < 0x7f && c != '"' && c != '\\')
if (isAsciiSafe) writer.writeNonEscapedAsciiKey(name)
else writer.writeKey(name)
```

---

## 7. Cache expensive computations outside the encode/decode body

Any computation that depends only on the type structure (not the runtime value) should be computed once at instance initialization, not per encode/decode call.

**Anti-pattern** (schema rebuilt per encode):
```scala
encoderInstance[A](schema, (value: A) => {
  val sch = <derive schema expression>  // WRONG — runs per encode
  val record = new GenericData.Record(sch)
  ...
})
```

**Correct pattern** (schema computed once):
```scala
encoderInstanceWithSchema[A](schema, (value: A, cachedSchema: Schema) => {
  val record = new GenericData.Record(cachedSchema)  // reuses pre-computed schema
  ...
})
```

Or pass it through the encoder context: `EncoderCtx.precomputedSchema: Option[Expr[Schema]]`.

---

## 8. Use position-based access for structured record formats

When the serialization format uses named fields with known positions (e.g., Avro `GenericData.Record`), use positional access:

```scala
// Instead of: record.put("name", value)   // HashMap key lookup per field
// Generate:   record.put(0, value)         // direct array access by compile-time index
```

Field indices are known at compile time from the schema/type structure. This eliminates HashMap operations and reduces per-field overhead from O(hash+equals) to O(1).

---

## Verification checklist

After applying these optimizations to a module:

1. **Tests**: `sbt --client "<module>3/clean ; <module>3/test ; <module>/clean ; <module>/test"`
2. **Bytecode check**: `javap -c -p <class> | grep "boxTo\|unboxTo\|BoxesRunTime"` — should find NO boxing for optimized case class codecs
3. **Benchmark**: `sbt --client 'benchmarks3/Jmh/run -f 2 -wi 5 -i 10 ".*<Module>Benchmark.*SimpleCC"'`
4. **JFR profile**: `sbt --client 'benchmarks3/Jmh/run -prof jfr:dir=/tmp/jfr -f 1 ...'` — confirm no `fieldNameMapper`, `BoxesRunTime`, `Function0/2.apply` in hot path
