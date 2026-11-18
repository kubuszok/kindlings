# Post-mortem: generated-code size and speed regressions in 0.4.0

## Summary

0.4.0 (released 2026-11-16) made every module match its upstream library. Two days later, #308 reported that
deriving an `AvroDecoder` for a case class with 88 or more defaulted fields failed with "Method too large". The
audit that followed (`generated-code-audit-2026-11.md`) found that #308 was one case of a wider problem: the parity
work had made generated methods larger and slower in several modules, and some older code shapes were already
close to the JVM limits.

- **#311** (merged 2026-11-18) fixed #308 and the pureconfig/sconfig sum and product readers.
- **#313** (merged 2026-11-18) fixed the remaining 0.4.0 regressions, replaced the shared nested fail-fast fold with
  a flat construction, and added bytecode-size guards to six modules.

No wrong results were reported: the regressions were compile failures for very wide types, methods too large for
the JIT, and slower runtime.

## Impact

- **Compile failure.** Avro decoders of case classes with about 88+ defaulted fields failed with "Method too large"
  (64 KB). Users could not work around it.
- **No JIT.** HotSpot does not compile methods over 8000 bytes of bytecode. With 100 fields, these methods ran
  interpreted:
  - Avro decoder;
  - pureconfig and sconfig readers;
  - circe decoder: 14.6 KB, or 30 KB with a runtime config;
  - yaml decoder.
  Avro decoding of a 40-field type was 5.1x slower than in 0.3.4.
- **Class size.** On Scala 2.13, the StackMapTable of a 100-field circe or yaml decoder grew to about 1.1 MB, giving
  classes of about 1.2 MB.
- **Allocations.** Every read or decode allocated work that belongs once per instance or at compile time. For
  example, pureconfig's sum-type reader copied the config object on every read.

## Timeline

- 2026-11-15: the parity commits for each module land, with upstream oracle tests checking behaviour.
- 2026-11-16: 0.4.0 is released.
- 2026-11-18 08:21: #308 reports "Method too large" for Avro decoders.
- 2026-11-18: the audit measures 0.3.4, 0.4.0 and the fix branches on Scala 2.13 and 3 (javap, a class-file parser,
  JMH with `-prof gc`).
- 2026-11-18: #311 merged (#308 fixed); #313 merged (remaining regressions, flat construction, size guards).

## Root causes

### 1. Configuration known at compile time emitted as runtime branches

Every module evaluates the configuration at compile time when it can (`evaluatedConfig`). The parity changes often
added the new behaviour as a runtime `if (config.x)` in the generated code instead of branching in the macro. Both
branches were emitted for every field or subtype, even when the macro already knew which one applied. Examples:
- Avro emitted both `scalaDefaultsInSchema` branches and built each field schema twice.
- Timestamp precision was read from the config on every value.
- jsoniter called the name mapper per subtype per decode.

### 2. Control flow inside argument lists

New per-field logic was written as an expression inside a constructor call or a list-building argument:
- Avro: `if (has) try decode catch NonFatal => default else default` as a constructor argument;
- sconfig: `if` inside `::`;
- pureconfig and sconfig writers: `if (opt.isEmpty) null else w(opt)` inside `(key, value) :: acc`.

A `try` inside an argument list makes the compiler spill every argument evaluated before it, so the method grows
quadratically with the number of fields. Branches nested in expressions need stack-map frames listing every live
local, so the StackMapTable grows quadratically too, most visibly on Scala 2.13. This is what turned a few bytes per
field into "Method too large" at 88 fields.

### 3. A shared generator that was already expensive

`EitherFieldsConstruct`, the fail-fast fold shared by circe, pureconfig and yaml, predates 0.4.0. It cost 120–170 B
per field, nested one level per field, and passed each field's decoder as a type witness that was evaluated again at
runtime. Parity changes that added per-field logic inside it multiplied that cost. The fix was structural:
`FlatFieldsConstruct` emits one local `def` and one flat statement per field, for both fail-fast and accumulating
decoding. The 100-field methods dropped to 3–5 KB.

### 4. Nothing measured the shape that failed

- The **upstream oracles** checked behaviour, which is what they are for. They do not see bytecode.
- The **benchmarks** used small models (`SimpleCC`, `Person`, `Address`, a few ADTs). Quadratic growth is invisible
  at 5 fields.
- There was **no test** that compiled a wide type and checked the size of the generated methods.
- **Runtime-config paths** were tested less: a config defined next to the call site cannot be evaluated at compile
  time, so it takes a different, larger path that the benchmarks did not cover.

### 5. Per-call work instead of per-instance work

Several helpers were built inside the method that runs on every call, instead of once per instance:
- schemas, alias lists and expected-key sets;
- regexes;
- the cats `toValDefs.use`, placed inside a lambda.

Each one is small. Together they show the same habit: code that is correct and passes every test, but the macro
emitted it where it is evaluated most often.

## What went well

- The upstream oracle tests made the fixes safe. Every change in #311 and #313 kept upstream semantics, and the
  oracles proved it.
- Measuring before fixing paid off. A first attempt at the pureconfig reader inlined all fields; it measured 7.7 KB
  and 0.44x the speed of 0.3.4, so it was replaced with per-field defs before it shipped.
- MiMa kept the runtime helpers honest: all new helpers are additions, and nothing that older generated code calls
  was removed.

## What we changed

- **Size guards.** `*BytecodeSizeSpec` in avro, pureconfig, sconfig, jsoniter, circe and yaml. Each one derives
  instances for a 100-field fixture, reads the generated class files and fails when a method exceeds 8000 B. The
  specs check both a compile-time and a runtime config where both paths exist. jsoniter checks only the compile-time
  config, because its runtime-config decoder is 8.0–9.4 KB, which predates 0.4.0 and is tracked.
- **Flat construction.** `FlatFieldsConstruct` in `derivation-commons` replaces the nested fold.
- **Hearth skills.** The lessons below were added to the `hearth` plugin in `kubuszok/agent-skills`:
  - `hearth-generated-code-performance`: code shapes, size budget, how to measure;
  - `hearth-macro-review`: checklist item and new LLM mistakes;
  so that other Hearth users, and the agents writing macros for them, avoid the same mistakes.

## Lessons

1. **Branch in the macro, not in the generated code.** When the macro knows a value (evaluated config, annotation,
   type shape), emit only the branch that applies. Emit a runtime `if` only on the path where the config cannot be
   evaluated, and keep that path small.
2. **No control flow inside argument lists.** Bind each field's result with a statement, or decode it in its own
   local `def`, then pass plain values to the constructor or builder.
3. **Generated code must stay linear in the number of fields and subtypes.** No nesting per field. Each field costs
   a fixed number of statements.
4. **Every derived method has a size budget: under 8000 B for 100 fields.** Check it with a test that reads the class
   files, on both Scala versions, with compile-time and runtime configuration.
5. **Benchmark wide types and the runtime-config path,** not only small models.
6. **Move work to the right time:** compile time first, then once per instance, then per call. Per-call work needs a
   reason.
7. **Behavioural parity and generated-code cost are separate checks.** A PR that adds behaviour to generated code
   needs both oracles and size or benchmark numbers.

## Follow-ups (older than 0.4.0, tracked in the audit)

- Specialise sum-type dispatch on the compile-time config (jsoniter, circe, yaml, sconfig, Avro enums).
- Per-instance caching:
  - cats `toValDefs.use` placement;
  - Avro per-call schemas and the `parseAvroName` regex;
  - yaml field decoders;
  - circe's inline `decode`, which allocates a lazy cell per field type.
- fast-show-pretty's nested appends on Scala 2.13: a 503 KB class for 90 fields.
- jsoniter's runtime-config decoder (8.0–9.4 KB) and its linear key matching.
