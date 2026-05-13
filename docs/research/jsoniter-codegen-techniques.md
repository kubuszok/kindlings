# jsoniter-scala Codegen Techniques Analysis

Analysis of `/tmp/jsoniter-scala/.../macros/JsonCodecMaker.scala` (Scala 3, 3472 lines).

## Key Techniques

### 1. Config Evaluation via `FromExpr` (NOT `semiEval`)

jsoniter-scala uses Scala 3's `FromExpr` typeclass to recursively deconstruct the config builder chain at compile time:

```scala
given FromExpr[CodecMakerConfig] with {
  def unapply(x: Expr[CodecMakerConfig])(using Quotes): Option[CodecMakerConfig] = x match
    case '{ CodecMakerConfig } => Some(CodecMakerConfig)
    case '{ ($x: CodecMakerConfig).withTransientDefault($v) } => Some(x.valueOrAbort.withTransientDefault(v.valueOrAbort))
    case '{ ($x: CodecMakerConfig).withFieldNameMapper($v) } => Some(x.valueOrAbort.copy(fieldNameMapper = ExprPartialFunctionWrapper(v)))
    // ... 30+ builder method patterns ...
    case other => report.error(s"Can't interpret ${other.show}"); None
}
```

The `ExprPartialFunctionWrapper` wraps function expressions — field name mappers are kept as `Expr[PartialFunction]` and applied at compile time via the macro, not at runtime.

**Context**: jsoniter-scala uses `FromExpr` because it's a native Scala 3 macro with no cross-compilation layer. Each builder method (`.withTransientDefault(true)`) is matched as an AST pattern. This is Scala 3-specific and wouldn't work cross-compiled to Scala 2.

Hearth's `Expr.semiEval` serves the same purpose but is more general — it evaluates any expression via runtime reflection, including constructor calls, field access, and function application. It already handles `JsoniterConfig.default` and builder chains like `JsoniterConfig().withSnakeCaseFieldNames`. No `FromExpr`-style pattern matching is needed in kindlings.

### 2. Single Loop with `l < 0` Sentinel (NO Dispatch Duplication)

```scala
var l = -1
while (l < 0 || in.isNextToken(',')) {
  l = in.readKeyAsCharBuf()
  <dispatch on l>
}
```

The `l < 0` trick merges the first-read and loop-body into one: on the first iteration `l = -1` so `l < 0` is true and the loop runs without checking for a comma. On subsequent iterations, `l >= 0` so `in.isNextToken(',')` controls continuation.

**Lesson**: Our code duplicated the dispatch chain — once before the `while` and once inside it. This doubles the method size. jsoniter avoids it entirely with this sentinel pattern. This single change could halve our method size and improve JIT behavior.

### 3. Hash-Based Field Dispatch for Large Objects

For ≤ 8 fields with total name length ≤ 64, jsoniter uses a simple if-else chain:
```scala
if (in.isCharBufEqualsTo(l, "name")) { ... }
else if (in.isCharBufEqualsTo(l, "age")) { ... }
else in.skip()
```

For larger objects, it switches to hash-based `match` with `@switch`:
```scala
(in.charBufToHashCode(l): @switch) match {
  case 0x12345 => if (in.isCharBufEqualsTo(l, "name")) { ... } // collision bucket
  case 0x67890 => if (in.isCharBufEqualsTo(l, "age")) { ... }
  case _ => in.skip()
}
```

Hash collisions are handled by nesting if-else chains within each hash bucket.

**Lesson**: For `SimpleCC` (3 fields), the if-else chain is optimal. But for `Person` (6 fields) and `Event` (sealed trait with nested objects), hash dispatch would be significantly faster. We should implement the same threshold: ≤8 fields → if-else, >8 fields → hash match.

### 4. Manual AST Building (NOT Quotes) for Variables

jsoniter builds `ValDef` AST nodes directly for mutable variables:
```scala
val sym = symbol(s"_$mappedName", fTpe, Flags.Mutable)
val valDef = ValDef(sym, Some(defaultValue))
readVars.addOne(valDef)
```

And references them via `Ref(readVars(i).symbol)`.

**Lesson**: jsoniter doesn't use quoted expressions for vars — it builds the AST trees manually. This gives fine-grained control over the generated tree structure. Hearth's `ValDefs.createVar` is the cross-compilation equivalent, and our experiments showed it generates correct code. The issue wasn't the var creation mechanism but the method structure around it.

### 5. Method Splitting via `withDecoderFor` / `withEncoderFor`

Each distinct type gets its own decoder method (`d0`, `d1`, ...) and encoder method (`e0`, `e1`, ...):

```scala
def withDecoderFor[T](refKey, arg, in)(f) =
  Apply(refs.getOrElse(refKey, {
    val sym = Symbol.newMethod(spliceOwner, s"d${refs.size}", MethodType(...))
    refs.update(refKey, Ref(sym))
    defs.addOne(DefDef(sym, params => Some(f(...).asTerm)))
    Ref(sym)
  }), List(in.asTerm, arg.asTerm))
```

This pools methods by `DecoderMethodKey(tpe, isStringified, discriminator)` — same type+config reuses one method. All method definitions are collected in `defs: ListBuffer[Definition]` and emitted in a `Block(defs.toList, codecDef)`.

**Lesson**: This is essentially the same pattern as our `ValDefsCache` + cached defs. jsoniter names them `d0, d1, ...` while we use `decode_SimpleCC, decode_Int, ...`. The key difference: jsoniter puts ALL defs and the codec class in one `Block`, while we use `cache.toValDefs.use`. The effect is similar.

### 6. Bitmask Required-Field Tracking

Instead of `Array[Boolean]` or per-field checks, uses packed `Int` bitmasks:

```scala
var p0: Int = -1  // bits 0-31 for fields 0-31
var p1: Int = 7   // bits 0-2 for fields 32-34
```

Each field read clears its bit: `p0 = p0 ^ (1 << fieldIndex)`
After the loop: `if (p0 != 0) in.requiredFieldError(f1(Integer.numberOfTrailingZeros(p0)))`

`f1` is a pre-built lookup function from bit index to field name.

**Lesson**: This is extremely compact — 1 bit per field, 32 fields per Int. Our current code uses `Array[Boolean]` which is 1 byte per field + array allocation. For `SimpleCC` (3 fields), both are negligible. For `Person` (6 fields) or larger, the bitmask is significantly more cache-friendly.

### 7. Zero Closures in Decode Path

The entire decode path is built from AST nodes: `ValDef`, `Assign`, `If`, `While`, `Match`, `Block`. No `Function` objects are ever created at runtime for the decode logic. The only lambdas are:

- Macro-time Scala functions that generate `Expr` values (these don't exist at runtime)
- Collection read helpers that use `genReadCollection(builder, x => genReadVal(...), ...)` — but even these generate inline code, not runtime lambdas

**Lesson**: Our `readObjectDirect` approach creates a `(String, JsonReader) => Unit` lambda at runtime. Even our fully-inline `ValDefs.createVar` approach still generates the dispatch chain as spliced expressions, which is equivalent — but the by-name `construct: => A` parameter in `readObjectDirect` does create a `Function0`. The fully-inline approach (without `readObjectDirect`) matches jsoniter's zero-closure pattern.

### 8. `readKeyAsCharBuf` + `isCharBufEqualsTo` (NOT `readKeyAsString`)

jsoniter reads field names as char buffers without allocating `String` objects:
```scala
l = in.readKeyAsCharBuf()           // returns length, stores bytes internally
in.isCharBufEqualsTo(l, "name")     // compares without String allocation
```

For hash dispatch: `in.charBufToHashCode(l)` computes the hash of the char buffer.

**Lesson**: Our current approach uses `readKeyAsString()` which allocates a `String` per field per decode. For high-throughput scenarios, this is significant. However, since `isCharBufEqualsTo` requires the length `l` as a parameter, and passing `Int` through a lambda causes boxing, we need the inline loop pattern to use it effectively.

## What jsoniter Does That We Don't (Ranked by Impact)

### HIGH Impact
1. **Single loop body (l < 0 sentinel)** — Our dispatch duplication doubles method size, hurting JIT
2. **Zero closures** — Our `readObjectDirect` approach creates Function0 + Function2; fully inline approach needed
3. **readKeyAsCharBuf** — Avoids String allocation per field per decode; needs inline loop
4. **Hash dispatch for >8 fields** — Our linear if-else doesn't scale for Person/Event

### MEDIUM Impact
5. **Bitmask field tracking** — More compact than Array[Boolean], better cache behavior
7. **Method splitting by type** — Similar to our ValDefsCache; minor differences

### LOW Impact (Already Implemented)
6. **Compile-time config evaluation** — Already done via semiEval (hearth 0.3.0-31+)
7. **writeNonEscapedAsciiKey** — Already implemented in our encoder optimization
8. **Pre-computed field names** — Already done via semiEval

## Recommended Implementation Plan

### Phase 1: Fix the inline loop (critical path)
1. Use `var l = -1; while (l < 0 || isNextToken(',')) { l = readKeyAsCharBuf(); dispatch }` pattern
2. This eliminates dispatch duplication AND enables readKeyAsCharBuf in one change
3. No `readObjectDirect` or any runtime helper — fully inline body
4. Expected: significant JIT improvement from halved method size

### Phase 2: Apply across modules
- circe-derivation: same semiEval/FromExpr + inline patterns for field name handling
- avro-derivation: position-based puts already done; apply to decoder
- yaml, ubjson, xml: same config evaluation pattern
- pureconfig, sconfig: field name pre-computation

### Phase 3: Hash dispatch (for large objects)
- Implement hash-based field matching for objects with >8 fields
- Use `charBufToHashCode` + `@switch` match
- Collisions handled by nested if-else chains per bucket
