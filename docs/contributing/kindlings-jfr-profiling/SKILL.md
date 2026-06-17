---
name: kindlings-jfr-profiling
description: >
  Profile JMH benchmarks with JFR: statistical execution sampling (any JDK) and deterministic
  method timing/tracing (JDK 25+, JEP 520) to measure exact invocation counts and timing of
  macro-generated codec/encoder/decoder methods. Covers exact sbt/JMH commands, targeting
  generated methods, reading results, and the JDK gating.
paths:
  - "benchmarks/**"
  - "docs/research/**"
user-invocable: false
---

# JFR profiling of Kindlings benchmarks

Two complementary techniques, both driven through the existing JMH harness:

1. **Execution sampling** (`jdk.ExecutionSample`) — statistical, low constant overhead (<2%),
   works on **any JDK**. Answers *"where does time go?"*. This is what we already use.
2. **Method timing / tracing** (`jdk.MethodTiming` / `jdk.MethodTrace`, **JEP 520, JDK 25+**) —
   deterministic bytecode instrumentation giving **exact** per-method invocation counts and
   min/avg/max execution time. Answers *"how many times, how long, exactly?"* for a **known**
   hot method.

> **Do not confuse with Hearth's MIO flame graphs.** The `.speedscope.json` files produced by
> `-Xmacro-settings:hearth.mioBenchmarkFlameGraphDir=...` profile **compile-time** macro
> expansion. This skill is about **runtime** profiling of the generated code. Different tool,
> different phase.

## JDK gating — check first

JEP 520 events require **JDK 25+** and are standard GA JFR events (not experimental, no unlock
flag). Verify both the JDK and that the events exist in this VM:

```bash
java -version                              # need 25+
jfr metadata | grep -E 'MethodTiming|MethodTrace'   # both must be listed
```

- **Dev machine** currently runs GraalVM CE 25 → method timing available.
- **CI / Temurin 17** → events absent; fall back to **sampling** (§ Sampling recipe). Everything
  in the sampling section works on 17.

If `jfr metadata` does not list the events, you are on a pre-25 JDK — use sampling only.

## Decision rule

| Goal | Technique | Why |
|------|-----------|-----|
| Broad "where is time spent" across a codec | **Sampling** | Any JDK, cheap, gives the call tree |
| Exact invocation count + timing of a *specific* hot method | **Method timing** | Aggregated, deterministic, overhead scales with call volume |
| Capture individual calls of a *low-frequency* method (+ stacks) | **Method tracing** | One event per call — **floods** on hot loops; only for rare methods |

Tracing a method called millions of times/sec overwhelms JFR buffers — use **timing**
(aggregated) for hot codec methods, **tracing** only for setup/rare paths.

## Sampling recipe (baseline — current workflow, any JDK)

JMH's `-prof jfr` starts/stops the recording around the **measurement** phase only (warmup
excluded by design) and writes **one `profile.jfr` per fork**.

```bash
sbt --client 'benchmarks3/Jmh/run -f 1 -wi 5 -i 10 \
  -prof "jfr:dir=/tmp/kindlings-jfr;configName=profile;debugNonSafePoints=true;stackDepth=256" \
  ".*JsoniterWriteBenchmark.kindlingsSemiAutoPerson"'
```

- Use **`-f 1`**: with `-f 2`, both forks write the *same* `profile.jfr` path and the second
  overwrites the first. For production numbers run `-f 2` *without* the profiler, and a separate
  `-f 1` *with* it.
- `configName=profile` (higher detail) vs `default` (lighter). `debugNonSafePoints=true` improves
  attribution accuracy.
- Output: `/tmp/kindlings-jfr/<benchmark-id>/profile.jfr`.

## Method timing / tracing recipe (JDK 25+)

### Step 1 — custom `.jfc` naming the methods to instrument

JMH forwards `configName=<path>` to `JFR.start settings=`, so point it at a custom config.
Filter grammar (JEP 520): `Class::method` (one method), `Class` (all methods of a class),
`::method` (that name across all classes), `@annotation.Class`; targets separated by `;`.
**No wildcards.** Overloads cannot be disambiguated (all same-named methods match).

`benchmarks/method-timing.jfc`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<configuration version="2.0" label="Kindlings method timing">
  <event name="jdk.MethodTiming">
    <setting name="enabled">true</setting>
    <setting name="period">1 s</setting>
    <!-- replace with the discovered generated-codec class (see Step 0) -->
    <setting name="filter">hearth.kindlings.benchmarks.KindlingsJsoniterInstances$$anon$1::encodeValue</setting>
  </event>
  <event name="jdk.MethodTrace">
    <setting name="enabled">true</setting>
    <setting name="stackTrace">true</setting>
    <setting name="threshold">0 ns</setting>
    <setting name="filter">hearth.kindlings.benchmarks.BenchmarkData$::person</setting>
  </event>
</configuration>
```

### Step 0 — discover the generated-codec method names

Generated codecs are **synthetic classes** (anonymous `JsonValueCodec`s or factory lambdas), so
their FQCNs are not in the source. Find them before writing the filter:

```bash
# Run a sampling pass first, then list the hottest methods (their real FQCNs):
jfr view hot-methods /tmp/kindlings-jfr/*/profile.jfr
# or dump generated bytecode classes during a benchmark JVM and grep:
#   -Djdk.attach... not needed; add -Xprint or scalac -Xprint:jvm at derivation site,
#   or javap the benchmark classes:
javap -p -classpath benchmarks/target/jvm-3/classes 'hearth.kindlings.benchmarks.KindlingsJsoniterInstances$'
```

The hot-methods view names the exact synthetic class (`...$$anon$N`) and method
(`encodeValue` / `decodeValue` / `nullValue`). Copy that FQCN into the `.jfc` filter.

### Step 2 — run JMH with the custom config

```bash
sbt --client 'benchmarks3/Jmh/run -f 1 -wi 5 -i 10 \
  -prof "jfr:dir=/tmp/kindlings-jfr;configName=/abs/path/benchmarks/method-timing.jfc;stackDepth=64;verbose=true" \
  ".*JsoniterWriteBenchmark.kindlingsSemiAutoPerson"'
```

## Reading results

```bash
jfr view method-timing  /tmp/kindlings-jfr/*/profile.jfr   # exact counts + min/avg/max per method
jfr view hot-methods    /tmp/kindlings-jfr/*/profile.jfr   # sampling call tree
jfr print --events jdk.MethodTiming                 /tmp/kindlings-jfr/*/profile.jfr
jfr print --events jdk.MethodTrace --stack-depth 32 /tmp/kindlings-jfr/*/profile.jfr
jfr view all-views      /tmp/kindlings-jfr/*/profile.jfr   # discover every built-in view
```

Flamegraph (async-profiler's `jfrconv`, replaces `jfr2flame`):

```bash
jfrconv --cpu /tmp/kindlings-jfr/*/profile.jfr flame.html
jfrconv -o collapsed /tmp/kindlings-jfr/*/profile.jfr folded.txt   # then import into speedscope.app
```

JDK Mission Control (JMC 8+) also has a Method Profiling page and a Flame Graph view.

## Caveats

- **Inlining distorts both.** Instrumenting a method can *prevent* the JIT from inlining it
  (slowing the very thing you measure); in *sampling*, inlined callees vanish into the caller's
  frame. Cross-check a method-timing result against the sampling call tree before trusting it.
- **Flooding.** `MethodTrace` on a tight loop overwhelms JFR — use `MethodTiming` for hot paths.
- **Steady state.** Early samples reflect interpreted/C1 code; JMH's profiler already excludes
  warmup, so keep `-wi 5` or higher.
- **Recursion hazard.** Never filter JDK-internal methods that the instrumentation itself uses —
  it can recurse infinitely. Keep filters to our own packages.
- **What timing cannot see.** Native/abstract methods, arguments, return values, or time *within*
  a method body (only entry→exit).
- **Per-fork overwrite.** `-f >1` with the profiler overwrites `profile.jfr`. Profile with
  `-f 1`; benchmark for numbers with `-f 2` and no profiler.

## Methodology (non-negotiable — see `feedback_benchmark_methodology.md`)

- Never reduce iterations or skip JFR to "save time" when investigating a perf gap.
- One JFR recording **per benchmark method** (separate `dir`/run), not a bulk run.
- Triangulate: **generated code** (`-Xprint:jvm` / Hearth derivation logging) + **bytecode**
  (`javap -c -p`, grep `boxTo`/`unboxTo`/`BoxesRunTime`) + **JFR profile**. A claim that a gap is
  closed needs all three plus a full `-f 2 -wi 5 -i 10` confirmation run.

## Related

- `docs/contributing/kindlings-runtime-perf/SKILL.md` — the optimizations you apply *after*
  profiling points at a hot spot.
- `docs/research/perf-regression-analysis.md` — worked examples of JFR + bytecode + generated-code
  triangulation across rounds of optimization.
- `docs/research/benchmark-results.md` — the canonical results table to update after a sweep.
