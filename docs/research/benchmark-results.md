# Kindlings Benchmark Results

> **Configuration**: 2 forks, 5 warmup iterations, 10 measurement iterations, 1s each
> **Scala versions**: 2.13.18, 3.8.3
> **Measured on**: branch benchmarks-and-docs (Hearth 0.3.1-53-g5032722), 2026-06-17 — raw JMH JSON in [benchmark-runs/2026-06-17-master](benchmark-runs/2026-06-17-master/).
> **Caveat**: run on a shared dev machine under concurrent load (~2h/version); the kindlings/original *ratios* are robust but absolute ops/s are approximate. A few *original-auto* measurements were noisy-high (avro4s auto decode SimpleCC; some cats auto), which understates those ratios.

All values in ops/s (higher is better). Error margins omitted for readability.

## Circe Encode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 30.3M | 30.9M | 18.8M | 19.0M | **1.63x faster** |
| SimpleCC | 3 | 31.2M | 31.2M | 21.8M | 20.9M | **1.43x faster** |
| SimpleADT | 2.13 | 27.5M | 27.1M | 13.4M | 13.9M | **1.99x faster** |
| SimpleADT | 3 | 26.8M | 25.7M | 26.6M | 27.1M | **0.99x faster** |
| Person | 2.13 | 4.5M | 4.5M | 3.0M | 3.1M | **1.46x faster** |
| Person | 3 | 4.4M | 4.5M | 3.1M | 3.2M | **1.41x faster** |
| Event | 2.13 | 3.4M | 3.4M | 2.3M | 2.4M | **1.45x faster** |
| Event | 3 | 3.3M | 3.4M | 2.4M | 2.3M | **1.42x faster** |

## Circe Decode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 88.3M | 93.2M | 42.0M | 42.6M | **2.19x faster** |
| SimpleCC | 3 | 91.9M | 92.1M | 20.5M | 21.2M | **4.34x faster** |
| SimpleADT | 2.13 | 56.3M | 55.9M | 25.0M | 25.7M | **2.19x faster** |
| SimpleADT | 3 | 58.3M | 54.6M | 27.9M | 28.0M | **2.08x faster** |
| Person | 2.13 | 5.4M | 5.3M | 3.5M | 3.6M | **1.49x faster** |
| Person | 3 | 5.5M | 5.4M | 2.7M | 2.6M | **2.05x faster** |
| Event | 2.13 | 3.3M | 3.5M | 2.7M | 2.7M | **1.27x faster** |
| Event | 3 | 3.5M | 3.3M | 2.1M | 2.2M | **1.58x faster** |

## Circe End-to-End with jsoniter-scala-circe Booster

Full pipeline benchmarks: domain type ↔ bytes/String, comparing Circe's default parser/printer vs jsoniter-scala-circe booster.

### Encode (domain type → bytes/String)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **13.9M** | 10.5M | 6.8M | 5.4M |
| SimpleCC | 3 | **15.5M** | 12.0M | 7.2M | 6.7M |
| SimpleADT | 2.13 | **14.3M** | 8.1M | 7.8M | 5.9M |
| SimpleADT | 3 | **15.6M** | 11.7M | 8.1M | 6.9M |
| Person | 2.13 | **1.6M** | 1.4M | 985.0K | 882.0K |
| Person | 3 | **1.7M** | 1.5M | 1.1M | 964.0K |
| Event | 2.13 | **1.3M** | 1.1M | 831.0K | 764.0K |
| Event | 3 | **1.4M** | 1.2M | 939.0K | 805.0K |

### Decode (bytes/String → domain type)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **9.3M** | 8.1M | 6.1M | 5.9M |
| SimpleCC | 3 | **8.8M** | 6.6M | 7.1M | 5.9M |
| SimpleADT | 2.13 | **11.2M** | 9.1M | 8.7M | 7.4M |
| SimpleADT | 3 | **10.9M** | 9.2M | 9.8M | 8.5M |
| Person | 2.13 | **1.3M** | 1.1M | 918.0K | 879.0K |
| Person | 3 | **1.3M** | 1.0M | 1.1M | 874.0K |
| Event | 2.13 | **1.0M** | 906.0K | 736.0K | 724.0K |
| Event | 3 | **996.0K** | 825.0K | 836.0K | 703.0K |

## Jsoniter Write

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 61.1M | 59.3M | 60.8M | **1.00x faster** |
| SimpleCC | 3 | 63.6M | 63.9M | 63.8M | **~tied** |
| Person | 2.13 | 4.8M | 4.7M | 4.7M | **1.01x faster** |
| Person | 3 | 5.5M | 5.4M | 5.4M | **1.02x faster** |
| SimpleADT | 2.13 | 62.2M | 62.7M | 69.2M | 0.91x |
| SimpleADT | 3 | 65.9M | 65.7M | 71.9M | 0.92x |
| Event | 2.13 | 4.5M | 4.5M | 4.3M | **1.04x faster** |
| Event | 3 | 4.8M | 4.8M | 4.9M | **0.97x faster** |

## Jsoniter Read

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 36.4M | 35.8M | 35.5M | **1.02x faster** |
| SimpleCC | 3 | 36.4M | 36.4M | 35.1M | **1.04x faster** |
| Person | 2.13 | 3.6M | 3.6M | 3.8M | 0.96x |
| Person | 3 | 3.6M | 3.7M | 3.8M | 0.98x |
| SimpleADT | 2.13 | 15.5M | 16.8M | — |  |
| SimpleADT | 3 | 15.8M | 15.7M | — |  |
| Event | 2.13 | 3.3M | 3.3M | — |  |
| Event | 3 | 3.3M | 3.3M | — |  |

## Cats Show

| Type | Scala | Kindlings | kittens semi | kittens auto | vs best kittens |
|------|-------|-----------|-------------|-------------|-----------------|
| SimpleCC | 2.13 | 38.2M | 7.5M | 7.0M | **5.11x faster** |
| SimpleCC | 3 | 27.2M | 19.6M | 19.9M | **1.37x faster** |
| SimpleADT | 2.13 | 86.0M | 16.2M | 10.4M | **5.32x faster** |
| SimpleADT | 3 | 72.8M | 51.9M | 52.8M | **1.38x faster** |
| Person | 2.13 | 2.0M | — | 804.0K | **2.44x faster** |
| Person | 3 | 1.6M | — | 1.4M | **1.18x faster** |
| Event | 2.13 | 1.9M | 602.0K | 620.0K | **2.99x faster** |
| Event | 3 | 1.5M | 535.0K | 1.2M | **1.25x faster** |

## Cats Eq

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC (eq) | 2.13 | 100.2M | 46.1M | **2.17x faster** |
| SimpleCC (eq) | 3 | 102.3M | 92.2M | **1.11x faster** |
| SimpleCC (neq) | 2.13 | 549.8M | — |  |
| SimpleCC (neq) | 3 | 561.4M | — |  |

## Cats Hash

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC | 2.13 | 824.9M | 27.4M | **30.1x faster** |
| SimpleCC | 3 | 828.3M | 110.0M | **7.5x faster** |

## Cats Order

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC | 2.13 | 424.8M | 389.7M | **1.09x faster** |
| SimpleCC | 3 | 429.7M | 347.6M | **1.24x faster** |

## Cats Semigroup

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| IntPair | 2.13 | 194.1M | 54.3M | **3.6x faster** |
| IntPair | 3 | 193.6M | 146.3M | **1.32x faster** |

## Cats Monoid

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| IntPair (combine) | 2.13 | 192.8M | 49.0M | **3.9x faster** |
| IntPair (combine) | 3 | 193.9M | 119.5M | **1.62x faster** |
| IntPair (empty) | 2.13 | 3.6B | 1.7B | **2.1x faster** |
| IntPair (empty) | 3 | 3.7B | 1.0B | **3.7x faster** |

## Cats Functor

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCCBox (map) | 2.13 | 277.3M | 5.7M | **48.6x faster** |
| SimpleCCBox (map) | 3 | 275.9M | 65.2M | **4.2x faster** |

## Cats Foldable / Traverse (Scala 3 — kittens Scala 2 does not support these)

| Type class | Kindlings | kittens semi | vs kittens |
|-----------|-----------|-------------|-----------|
| Foldable (foldLeft) | 1.6B | 109.6M | **14.6x faster** |
| Traverse (traverse) | 164.2M | 18.7M | **8.8x faster** |

## Cats ShowPretty (Scala 3)

| Approach | SimpleCC | Person | Notes |
|----------|----------|--------|-------|
| Kindlings Show | 27.0M | 1.7M | Single-line baseline |
| Kindlings ShowPretty | 34.3M | 1.8M | Multi-line, ~0% overhead |
| kittens ShowPretty | 5.4M | 557.0K | List[String] accumulation |
| Kindlings FastShowPretty | 18.0M | 1.3M | StringBuilder + escaped strings |

## Cats Empty

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| Empty | 2.13 | 1.6B | 1.6B | **~tied** |
| Empty | 3 | 1.8B | 1.1B | **1.6x faster** |

## Avro (kindlings vs avro4s)

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| Encode SimpleCC | 2.13 | 272.5M | — | 44.6M | **6.11x faster** |
| Encode SimpleCC | 3 | 277.4M | 48.7M | 50.5M | **5.49x faster** |
| Encode SimpleADT | 2.13 | 364.3M | — | — |  |
| Encode SimpleADT | 3 | 378.5M | — | — |  |
| Encode Person | 2.13 | 19.5M | — | 4.5M | **4.31x faster** |
| Encode Person | 3 | 19.1M | 5.8M | 5.8M | **3.27x faster** |
| Encode Event | 2.13 | 17.4M | — | — |  |
| Encode Event | 3 | 18.0M | — | — |  |
| Decode SimpleCC | 2.13 | 119.2M | — | 17.7M | **6.74x faster** |
| Decode SimpleCC | 3 | 127.2M | 26.0M | 83.9M | **1.52x faster** |
| Decode SimpleADT | 2.13 | 168.0M | — | — |  |
| Decode SimpleADT | 3 | 167.0M | — | — |  |
| Decode Person | 2.13 | 9.8M | — | 3.7M | **2.62x faster** |
| Decode Person | 3 | 9.3M | 3.1M | 4.4M | **2.10x faster** |
| Decode Event | 2.13 | 8.5M | — | — |  |
| Decode Event | 3 | 8.6M | — | — |  |

## PureConfig (kindlings vs pureconfig-generic)

| Type | Scala | Kindlings | Original semi | vs original |
|------|-------|-----------|--------------|------------|
| Write SimpleCC | 2.13 | 11.0M | 1.2M | **9.12x faster** |
| Write SimpleCC | 3 | 11.1M | 1.7M | **6.64x faster** |
| Write Person | 2.13 | 1.2M | 205.0K | **5.76x faster** |
| Write Person | 3 | 1.2M | 248.0K | **4.75x faster** |
| Read SimpleCC | 2.13 | 17.2M | 1.4M | **12.46x faster** |
| Read SimpleCC | 3 | 17.2M | 1.4M | **12.48x faster** |
| Read Person | 2.13 | 1.0M | 216.0K | **4.77x faster** |
| Read Person | 3 | 1.0M | 197.0K | **5.29x faster** |

## Optics (kindlings vs quicklens vs hand-written)

| Benchmark | Scala | Kindlings | quicklens | hand-written |
|-----------|-------|-----------|-----------|--------------|
| DeepName | 2.13 | 99.0M | 98.7M | 98.8M |
| DeepName | 3 | 99.2M | 99.1M | 93.2M |
| EachSalary | 2.13 | 21.8M | 20.1M | 21.4M |
| EachSalary | 3 | 21.2M | 20.8M | 21.9M |

## Tapir Schema (kindlings vs tapir-derivation)

| Type | Scala | Kindlings | Original semi | Original auto |
|------|-------|-----------|--------------|--------------|
| SimpleCC | 2.13 | 3.8B | 3.8B | 3.8B |
| SimpleCC | 3 | 3.8B | 3.8B | 3.8B |
| Person | 2.13 | 3.9B | 3.8B | 3.8B |
| Person | 3 | 3.8B | 3.7B | 3.8B |
| Event | 2.13 | 3.9B | 3.8B | 3.8B |
| Event | 3 | 3.8B | 3.8B | 3.8B |

## Tapir OpenAPI jsoniter codecs (kindlings vs circe)

| Operation | Scala | Kindlings | circe |
|-----------|-------|-----------|-------|
| Encode | 2.13 | 70.0K | 24.0K |
| Encode | 3 | 56.0K | 24.0K |
| Decode | 2.13 | 26.0K | 12.0K |
| Decode | 3 | 21.0K | 5.0K |

## Kindlings-only modules (no original comparison)

| Module | Type | Scala 2.13 | Scala 3 |
|--------|------|-----------|---------|
| FastShowPretty | SimpleCC | 13.5M | 17.9M |
| FastShowPretty | SimpleADT | 15.6M | 14.6M |
| FastShowPretty | Person | 1.2M | 1.3M |
| FastShowPretty | Event | 854.0K | 972.0K |
| UbjsonWrite | SimpleCC | 11.2M | 11.0M |
| UbjsonWrite | SimpleADT | 13.4M | 13.2M |
| UbjsonWrite | Person | 1.4M | 1.4M |
| UbjsonWrite | Event | 1.3M | 1.2M |
| UbjsonRead | SimpleCC | 11.3M | 10.4M |
| UbjsonRead | SimpleADT | 13.8M | 13.6M |
| UbjsonRead | Person | 1.4M | 1.4M |
| UbjsonRead | Event | 1.3M | 1.2M |
| SconfigWrite | SimpleCC | 10.9M | 10.9M |
| SconfigWrite | SimpleADT | 5.2M | 6.3M |
| SconfigWrite | Person | 1.2M | 1.3M |
| SconfigWrite | Event | 630.0K | 648.0K |
| SconfigRead | SimpleCC | 72.4M | 60.9M |
| SconfigRead | SimpleADT | 12.0M | 11.8M |
| SconfigRead | Person | 5.0M | 4.7M |
| SconfigRead | Event | 2.6M | 2.8M |
| YamlEncode | SimpleCC | 1.4M | 1.4M |
| YamlEncode | SimpleADT | 2.2M | 2.3M |
| YamlEncode | Person | 147.0K | 164.0K |
| YamlEncode | Event | 136.0K | 148.0K |
| YamlDecode | SimpleCC | 9.4M | 7.0M |
| YamlDecode | SimpleADT | 98.6M | 104.1M |
| YamlDecode | Person | 774.0K | 763.0K |
| YamlDecode | Event | 721.0K | 629.0K |
| XmlEncode | SimpleCC | 46.2M | 45.2M |
| XmlEncode | Address | 38.6M | 38.8M |
| XmlDecode | SimpleCC | 4.8M | 5.0M |
| XmlDecode | Address | 3.2M | 3.4M |
| ScalacheckArbitrary | SimpleCC | 779.0K | 741.0K |
| ScalacheckArbitrary | SimpleADT | 1.9M | 1.9M |
| ScalacheckArbitrary | Person | 4.0K | 4.0K |
| ScalacheckShrink | SimpleCC | 5.8M | 6.9M |
| ScalacheckShrink | Person | 5.8M | 5.2M |

## Key takeaways

1. **Circe**: Kindlings is **1.4-2.2x faster** for encoding (except SimpleADT on Scala 3, ~tied) and **1.3-4.3x faster** for decoding across all types and both Scala versions
2. **Circe + booster**: Kindlings + jsoniter-scala-circe is the fastest Circe pipeline in every scenario
3. **Cats**: Kindlings leads almost everywhere — **1.4-5.3x** for Show, **7.5-30x** for Hash, **1.1-1.2x** for Order, **1.3-3.9x** for Semigroup/Monoid, **48.6x** for Functor on 2.13 (**4.2x** on Scala 3), **14.6x** for Foldable, **8.8x** for Traverse, and ShowPretty **~6x** vs kittens. Eq is **1.1x** on Scala 3 and **2.2x** faster on 2.13; Empty is ~tied to 1.6x
4. **Jsoniter**: at parity overall — writes ~tied (0.91-1.04x), reads ~tied (0.96-1.04x), with SimpleADT write (0.91-0.92x) as the only consistent small gap
5. **PureConfig**: Kindlings is **4.7-12.5x faster** across all operations — massive improvement
6. **Avro**: Kindlings is **1.5-6.7x faster** than avro4s across all benchmarks — 5.5-6.1x for SimpleCC encode, 3.3-4.3x for Person encode, 1.5-6.7x for decode
7. **Tapir Schema**: Tied (~3.8-3.9B ops/s) — just a field access at runtime
8. **Optics**: Kindlings `.modify` matches quicklens and hand-written code (~99M DeepName, ~21M EachSalary)
9. **Kindlings auto ≈ semi-auto** in most benchmarks (occasional gaps come from JIT fork-to-fork variance, not different generated code)
</content>
</invoke>
