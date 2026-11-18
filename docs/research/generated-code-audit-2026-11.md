# Generated-code audit (2026-11): size and runtime cost after the 0.4.0 parity work

Kindlings 0.4.0 made every module match its upstream library. This audit checks whether that work:

- moved decisions that are known at compile time into the generated code (runtime `if`/`match` on configuration,
  re-computed names, branches for features the type does not use);
- duplicated logic per field or subtype that one MiMa-checked runtime helper could do;
- bloated the bytecode past what HotSpot JIT-compiles.

It also records the pre-existing problems the measurements uncovered.

## Method

- **Size probe.** A benchmark model (`SimpleCC`, `SimpleADT`, `Person`, `Event`) plus a 90-field case class with
  defaults (`Big90`). Compiled against the published 0.3.4 and 0.4.0 artifacts and against the fix branch, on Scala
  2.13.18 and 3.8.3. Measured with `javap` and a class-file parser:
  - per-derivation class bytes;
  - largest method;
  - bytes per field;
  - StackMapTable size.
- **Runtime.** JMH (`scala-cli --jmh`) on the same artifacts, comparing throughput and `-prof gc` allocations per
  operation. Runs on a shared machine are noisy, so only results with small error bars, confirmed by a focused
  re-run, count.

Background facts:

- HotSpot never JIT-compiles a method larger than 8000 bytes of bytecode (`-XX:-DontCompileHugeMethods`).
- A `try` inside an argument list makes the compiler spill all previously evaluated arguments, so the method grows
  quadratically.
- Branches nested in expressions need full stack-map frames that list every live local. On wide types this is a
  quadratic StackMapTable, especially on Scala 2.13.

## Fixed in PR #311

| Module | Problem (0.4.0) | Fix | Result |
|---|---|---|---|
| Avro decoder (#308) | Per defaulted field, `if (has) try decode catch NonFatal => default else default` was inlined as a constructor argument. That spilled the stack quadratically: >8 KB from ~26 defaulted fields, >64 KB at 88 ("Method too large"). | Each defaulted field decodes in its own local def. | 100 fields: 636 B (S3) / 540 B (2.13). 40-field decode 5.1x faster, at 0.3.4's level. |
| pureconfig sealed-trait reader | `obj.withoutKey(discriminator)` copied the object on every read. | Skipped when it cannot be observed: case objects, and derived case classes that statically allow unknown keys and have no field keyed as the (static) discriminator. | At or above 0.3.4; 208 vs 384 B/op. |
| sconfig sealed-trait reader | Same copy. | Same fix. | |
| pureconfig case-class reader | Per field `cursor.asObjectCursor.flatMap(...)`: a cursor, a `Right` and a lambda per field per read. | One `asObjectCursor`, each field read in its own local def. | 1.05-1.44x of 0.3.4. |

## Remaining regressions introduced in 0.4.0

1. **Avro schema initialisers check `scalaDefaultsInSchema` at runtime and build each field schema twice.**
   - Where: `AvroSchemaForHandleAsCaseClassRule.scala`, even though `sfctx.evaluatedConfig` is available.
   - Cost: +45 B per field in every derived schema (decoder, encoder, SchemaFor). Option fields build their union
     twice.
   - Size only, since schemas are built once per instance.
2. **The Avro encoder derives record schemas several times.** `deriveSelfContainedSchema` gets a fresh cache per
   call, and its copy ignores the evaluated config (`outerCfg` defeats `semiEval`). As a result, `encodeSchema_Big90`
   is 9.3 KB (S3) / 8.8 KB (2.13), the only Avro method still over 8000 B. Nested ADT encoders hold every schema
   2-4 times. The duplication predates 0.4.0; the size of each copy does not.
3. **The Avro decoder looks up each defaulted field twice and rebuilds its alias list.**
   `hasFieldOrAlias` + `getFieldByNameOrAlias` means two hash lookups. `List.empty`/`::` aliases are rebuilt per
   call. A ~35 B `NonFatal.unapply` handler sits in every field def. avro4s does one lookup
   (`SchemaFieldDecoder.safeDecodeFieldValue`).
4. **Avro timestamps and `OffsetDateTime` read precision/format from the runtime config per value.**
5. **The sconfig case-class reader puts each field's `if/else` inside a `::` list argument.**
   - StackMapTable grew from 20 B to 81 KB (class 46 KB → 103 KB for 90 fields).
   - Defaulted fields call `o.get(key)` twice.
   - Every read allocates a list cell per field, an `Array[Any]` and a `Right`.
6. **pureconfig and sconfig writers put `if (opt.isEmpty) null else w(opt)` inside `(key, value) :: acc`.**
   StackMapTable grew from 20 B to 6.4 KB: the +57% class size on 90 fields. A `Tuple2` and a list cell per field
   per write predate 0.4.0.
7. **The pureconfig reader's fail-fast fold.**
   - `buildEitherFailFast` re-wraps `Left` and adds a `MatchError` arm per level. The result is discarded on failure
     for `combineFieldFailures`.
   - 90 fields: 12.5 KB (S3) / 17.5 KB with a 574 KB StackMapTable (2.13). Still over 8000 B after #311.
   - Prototype ("all `Right` → construct, else combine an `Array` of failures", with per-field defs): 3 KB, about
     34 B per field, StackMapTable ~320 B, on both Scala versions.
8. **jsoniter sum types.** A new `isLeafName` lambda makes N extra `adtLeafClassNameMapper` calls per decode in
   discriminator mode. `notStringOrObjectError` is a runtime `if (config.circeLikeObjectEncoding)` whose two branches
   are the same string on Scala 3.
9. **yaml sum types.** The dispatch lambda body is spliced three times instead of twice (+15-17% on ADT decoders).

## Pre-existing problems (0.3.4 and earlier)

### The shared fail-fast fold (`derivation-commons` `EitherFieldsConstruct`, used by circe, pureconfig, yaml, sconfig-like code)

- Cost: ~120-170 B per field.
- The constructor argument `unsafeCast(value, <decoder expr>)` re-evaluates the decoder as a type witness, which
  allocates decoders on every call (e.g. `Decoder.decodeOption`, `decoderFromFF(lambda)`).
- circe `Big90` decoder: 14.5 KB (S3) / 17.9 KB (2.13), with a 377 KB StackMapTable in a 425 KB class.
- yaml decoder: 11.8 KB / 16.1 KB.
- Fix: per-field defs, an "all `Right`" check, an `Array`-based combine helper, and casts instead of witnesses.

### Statically known configuration emitted as runtime branches

- jsoniter, circe, yaml, sconfig and Avro sum-type dispatch.
- ubjson absent-field filling, which ignores the existing `staticAbsentFallback`.
- Name transforms (`transformConstructorNames`, `adtLeafClassNameMapper`) called per subtype per call.

Fix: specialise when `evaluatedConfig` is defined. Otherwise compute names once per instance in a `lazy val`.

### Per-call work that belongs per instance

- **cats** `Show`/`Eq`/`Order`/...: `toValDefs.use` sits inside the per-call lambda, so every call does 5× `new
  LazyRef` plus a synchronized re-summon. `HashMacrosImpl` already places it correctly.
- **Avro** rebuilds schemas inside encode/decode for case-object enums, case objects in mixed sealed traits, named
  tuples and `Either`.
- **Avro** `parseAvroName` compiles a regex on every call (`String.replaceAll`), once per candidate subtype per
  decode.
- **Strict mode** in pureconfig and sconfig rebuilds the expected-keys `Set` per read.
- **Non-evaluable config** (a user `implicit val cfg = ...`) recomputes name transforms per field per call.

### Shapes that bloat Scala 2.13 classes

fast-show-pretty's nested `foldLeft` appends: the `Big90` class is 503 KB, of which 484 KB is StackMapTable. Flat
statements plus a `nextField` helper would bring it to ~2.2 KB.

### Other

- jsoniter matches keys with a linear `isCharBufEqualsTo` chain; upstream hashes above a threshold.
  `readNullAsDefault` takes two capturing lambdas per defaulted field.
- scalacheck re-evaluates field `Gen`s per sample and reads a `List` by index, which is O(n²).
- yaml looks up fields with `collectFirst` per field, which is O(n²).

## Plan

1. **Remaining 0.4.0 regressions** (items 1-9), with a per-module bytecode-size guard test: compile a wide fixture
   and fail when the largest generated method exceeds 8000 B.
2. **Rework the shared fail-fast fold** in `derivation-commons` and its runtime helpers. This fixes circe, pureconfig
   and yaml at once.
3. **Specialise on compile-time config and cache per instance**, module by module, starting with Avro schemas and
   cats.

Every change keeps upstream semantics. The upstream oracles (`upstream-oracle-tests`) are the check.
