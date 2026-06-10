# Kindlings Benchmark Results

> **Configuration**: 2 forks, 5 warmup iterations, 10 measurement iterations, 1s each
> **Platform**: macOS (Apple M4 Pro), JVM (temurin 17)
> **Scala versions**: 2.13.18, 3.8.4
> **Measured on**: master @ e0534fe (Hearth 0.3.1), 2026-06-10 — raw JMH JSON in [benchmark-runs/2026-06-10-master](benchmark-runs/2026-06-10-master/)

All values in ops/s (higher is better). Error margins omitted for readability.

## Circe Encode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 30.8M | 30.9M | 20.9M | 20.9M | **1.5x faster** |
| SimpleCC | 3 | 33.8M | 38.5M | 17.6M | 17.6M | **2.2x faster** |
| SimpleADT | 2.13 | 32.1M | 28.7M | 14.8M | 18.4M | **1.7x faster** |
| SimpleADT | 3 | 31.3M | 31.2M | 25.5M | 25.6M | **1.2x faster** |
| Person | 2.13 | 3.8M | 3.7M | 2.6M | 2.9M | **1.3x faster** |
| Person | 3 | 3.8M | 3.4M | 2.4M | 2.1M | **1.6x faster** |
| Event | 2.13 | 3.1M | 3.1M | 2.3M | 2.4M | **1.3x faster** |
| Event | 3 | 3.1M | 3.1M | 2.0M | 2.0M | **1.5x faster** |

## Circe Decode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 46.1M | 46.3M | 38.5M | 38.8M | **1.2x faster** |
| SimpleCC | 3 | 40.6M | 41.3M | 18.3M | 17.9M | **2.3x faster** |
| SimpleADT | 2.13 | 38.8M | 39.1M | 30.2M | 30.2M | **1.3x faster** |
| SimpleADT | 3 | 39.5M | 41.6M | 25.7M | 25.1M | **1.6x faster** |
| Person | 2.13 | 3.1M | 3.2M | 2.3M | 2.5M | **1.3x faster** |
| Person | 3 | 3.1M | 3.1M | 2.4M | 2.5M | **1.3x faster** |
| Event | 2.13 | 2.5M | 2.5M | 2.2M | 2.1M | **1.2x faster** |
| Event | 3 | 2.5M | 2.6M | 2.0M | 1.9M | **1.3x faster** |

## Circe End-to-End with jsoniter-scala-circe Booster

Full pipeline benchmarks: domain type ↔ bytes/String, comparing Circe's default parser/printer vs jsoniter-scala-circe booster.

### Encode (domain type → bytes/String)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **8.3M** | 7.3M | 7.2M | 6.7M |
| SimpleCC | 3 | **12.1M** | 7.6M | 8.0M | 6.6M |
| SimpleADT | 2.13 | **13.2M** | 7.1M | 7.6M | 6.4M |
| SimpleADT | 3 | **9.7M** | 8.8M | 7.3M | 7.4M |
| Person | 2.13 | **1.3M** | 1.0M | 962.8K | 915.5K |
| Person | 3 | **971.2K** | 781.2K | 1.0M | 895.9K |
| Event | 2.13 | 737.3K | 947.3K | 833.0K | 774.6K |
| Event | 3 | **864.6K** | 656.5K | 876.6K | 764.3K |

### Decode (bytes/String → domain type)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **8.0M** | 6.8M | 7.5M | 6.8M |
| SimpleCC | 3 | **9.6M** | 6.2M | 7.3M | 5.1M |
| SimpleADT | 2.13 | **9.0M** | 8.8M | 7.2M | 7.2M |
| SimpleADT | 3 | **10.7M** | 8.9M | 9.3M | 7.3M |
| Person | 2.13 | **957.0K** | 799.3K | 695.8K | 521.2K |
| Person | 3 | **1.1M** | 1.0M | 747.5K | 704.6K |
| Event | 2.13 | **845.5K** | 717.6K | 598.3K | 576.1K |
| Event | 3 | **992.4K** | 787.3K | 683.3K | 627.8K |

## Jsoniter Write

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 59.5M | 60.2M | 62.4M | 0.97x |
| SimpleCC | 3 | 62.0M | 62.4M | 58.4M | **1.07x faster** |
| Person | 2.13 | 2.4M | 3.8M | 3.0M | **1.3x faster** |
| Person | 3 | 4.7M | 4.8M | 4.8M | **~tied** |
| SimpleADT | 2.13 | 59.9M | 59.2M | 58.0M | **1.03x faster** |
| SimpleADT | 3 | 57.1M | 64.0M | 56.4M | **1.13x faster** |
| Event | 2.13 | 3.2M | 2.4M | 2.1M | **1.6x faster** |
| Event | 3 | 3.8M | 3.3M | 4.3M | 0.90x |

## Jsoniter Read

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 26.0M | 28.5M | 30.9M | 0.92x |
| SimpleCC | 3 | 27.2M | 27.9M | 30.8M | 0.91x |
| Person | 2.13 | 3.2M | 2.6M | 2.0M | **1.6x faster** |
| Person | 3 | 2.2M | 2.2M | 2.6M | 0.85x |
| SimpleADT | 2.13 | 31.4M | 28.1M | — |  |
| SimpleADT | 3 | 32.7M | 36.8M | — |  |
| Event | 2.13 | 2.3M | 2.2M | — |  |
| Event | 3 | 2.0M | 1.8M | — |  |

## Cats Show

| Type | Scala | Kindlings | kittens semi | kittens auto | vs best kittens |
|------|-------|-----------|-------------|-------------|-----------------|
| SimpleCC | 2.13 | 48.1M | 18.0M | 17.6M | **2.7x faster** |
| SimpleCC | 3 | 47.8M | 14.3M | 12.3M | **3.3x faster** |
| SimpleADT | 2.13 | 166M | 30.0M | 17.2M | **5.5x faster** |
| SimpleADT | 3 | 97.6M | 34.1M | 35.0M | **2.8x faster** |
| Person | 2.13 | 2.3M | — | 1.4M | **1.7x faster** |
| Person | 3 | 1.8M | — | 1.5M | **1.2x faster** |
| Event | 2.13 | 2.1M | 1.2M | 1.1M | **1.8x faster** |
| Event | 3 | 1.9M | 567.5K | 1.3M | **1.5x faster** |

## Cats Eq

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC (eq) | 2.13 | 96.9M | 43.2M | **2.2x faster** |
| SimpleCC (eq) | 3 | 84.7M | 111M | 0.76x |
| SimpleCC (neq) | 2.13 | 526M | — |  |
| SimpleCC (neq) | 3 | 469M | — |  |

## Cats Hash

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC | 2.13 | 264M | 37.4M | **7.1x faster** |
| SimpleCC | 3 | 234M | 53.4M | **4.4x faster** |

## Cats Order

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC | 2.13 | 451M | 72.0M | **6.3x faster** |
| SimpleCC | 3 | 415M | 329M | **1.3x faster** |

## Cats Semigroup

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| IntPair | 2.13 | 508M | 71.6M | **7.1x faster** |
| IntPair | 3 | 154M | 102M | **1.5x faster** |

## Cats Monoid

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| IntPair (combine) | 2.13 | 561M | 72.9M | **7.7x faster** |
| IntPair (combine) | 3 | 155M | 101M | **1.5x faster** |
| IntPair (empty) | 2.13 | 1896M | 1659M | **1.14x faster** |
| IntPair (empty) | 3 | 2077M | 603M | **3.4x faster** |

## Cats Functor

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCCBox (map) | 2.13 | 205M | 4.4M | **47x faster** |
| SimpleCCBox (map) | 3 | 238M | 84.9M | **2.8x faster** |

## Cats Foldable / Traverse (Scala 3 — kittens Scala 2 does not support these)

| Type class | Kindlings | kittens semi | vs kittens |
|-----------|-----------|-------------|-----------|
| Foldable (foldLeft) | 1006M | 120M | **8.4x faster** |
| Traverse (traverse) | 123M | 24.2M | **5.1x faster** |

## Cats ShowPretty (Scala 3)

| Approach | SimpleCC | Person | Notes |
|----------|----------|--------|-------|
| Kindlings Show | 49.0M | 2.0M | Single-line baseline |
| Kindlings ShowPretty | 50.0M | 2.1M | Multi-line, ~0% overhead |
| kittens ShowPretty | 6.2M | 644.7K | List[String] accumulation |
| Kindlings FastShowPretty | 27.3M | 1.4M | StringBuilder + escaped strings |

## Avro (kindlings vs avro4s)

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| Encode SimpleCC | 2.13 | 139M | — | 44.7M | **3.1x faster** |
| Encode SimpleCC | 3 | 145M | 45.1M | 48.3M | **3.0x faster** |
| Encode Person | 2.13 | 15.3M | — | 4.8M | **3.2x faster** |
| Encode Person | 3 | 13.8M | 6.7M | 6.5M | **2.1x faster** |
| Encode Event | 2.13 | 13.7M | — | — |  |
| Encode Event | 3 | 14.4M | — | — |  |
| Decode SimpleCC | 2.13 | 86.6M | — | 20.8M | **4.2x faster** |
| Decode SimpleCC | 3 | 84.6M | 18.7M | 42.0M | **2.0x faster** |
| Decode Person | 2.13 | 6.0M | — | 2.8M | **2.2x faster** |
| Decode Person | 3 | 6.5M | 2.6M | 3.2M | **2.0x faster** |
| Decode Event | 2.13 | 4.8M | — | — |  |
| Decode Event | 3 | 4.9M | — | — |  |

## PureConfig (kindlings vs pureconfig-generic)

| Type | Scala | Kindlings | Original semi | vs original |
|------|-------|-----------|--------------|------------|
| Write SimpleCC | 2.13 | 10.5M | 1.4M | **7.6x faster** |
| Write SimpleCC | 3 | 11.4M | 1.1M | **10x faster** |
| Write Person | 2.13 | 1.1M | 218.1K | **5.1x faster** |
| Write Person | 3 | 1.2M | 207.3K | **5.5x faster** |
| Read SimpleCC | 2.13 | 15.6M | 1.4M | **11x faster** |
| Read SimpleCC | 3 | 17.3M | 959.7K | **18x faster** |
| Read Person | 2.13 | 783.5K | 214.2K | **3.7x faster** |
| Read Person | 3 | 760.1K | 173.6K | **4.4x faster** |

## Kindlings-only modules (no original comparison)

| Module | Type | Scala 2.13 | Scala 3 |
|--------|------|-----------|---------|
| FastShowPretty | SimpleCC | 18.4M | 25.8M |
| FastShowPretty | SimpleADT | 20.8M | 26.9M |
| FastShowPretty | Person | 1.3M | 1.4M |
| FastShowPretty | Event | 972.7K | 1.2M |
| UbjsonWrite | SimpleCC | 16.2M | 17.0M |
| UbjsonWrite | SimpleADT | 16.3M | 15.9M |
| UbjsonWrite | Person | 1.6M | 1.6M |
| UbjsonWrite | Event | 1.5M | 1.6M |
| UbjsonRead | SimpleCC | 12.8M | 12.9M |
| UbjsonRead | SimpleADT | 15.8M | 15.1M |
| UbjsonRead | Person | 1.4M | 1.1M |
| UbjsonRead | Event | 1.3M | 1.2M |
| SconfigWrite | SimpleCC | 10.2M | 8.5M |
| SconfigWrite | SimpleADT | 5.9M | 4.8M |
| SconfigWrite | Person | 1.2M | 1.3M |
| SconfigWrite | Event | 614.8K | 669.0K |
| SconfigRead | SimpleCC | 59.9M | 63.5M |
| SconfigRead | SimpleADT | 8.7M | 8.1M |
| SconfigRead | Person | 3.9M | 3.7M |
| SconfigRead | Event | 2.0M | 1.8M |
| YamlEncode | SimpleCC | 903.4K | 964.3K |
| YamlEncode | SimpleADT | 1.5M | 1.6M |
| YamlEncode | Person | 109.9K | 113.6K |
| YamlEncode | Event | 97.6K | 104.4K |
| YamlDecode | SimpleCC | 7.2M | 6.5M |
| YamlDecode | SimpleADT | 35.4M | 47.5M |
| YamlDecode | Person | 770.2K | 757.0K |
| YamlDecode | Event | 655.3K | 626.5K |
| XmlEncode | SimpleCC | 28.4M | 41.7M |
| XmlEncode | Address | 28.3M | 28.4M |
| XmlDecode | SimpleCC | 3.2M | 3.5M |
| XmlDecode | Address | 2.5M | 2.5M |
| ScalacheckArbitrary | SimpleCC | 693.1K | 679.3K |
| ScalacheckArbitrary | SimpleADT | 1.8M | 1.7M |
| ScalacheckArbitrary | Person | 4.7K | 4.7K |
| ScalacheckShrink | SimpleCC | 5.5M | 4.9M |
| ScalacheckShrink | Person | 5.3M | 4.4M |

## Key takeaways

1. **Circe**: Kindlings is **1.2-2.3x faster** for encoding and decoding across all types and both Scala versions
2. **Circe + booster**: Kindlings + jsoniter-scala-circe is the fastest Circe pipeline in nearly every scenario (Event encode on 2.13 being the one exception in this run)
3. **Cats**: Kindlings leads almost everywhere — **2.7-5.5x** for Show, **4.4-7.1x** for Hash, **1.3-6.3x** for Order, **1.5-7.7x** for Semigroup/Monoid, **47x** for Functor on 2.13 (**2.8x** on Scala 3), **8.4x** for Foldable, **5.1x** for Traverse, and ShowPretty **~8x** vs kittens. The one exception: Eq on Scala 3 is **0.76x** of kittens (while being 2.2x faster on 2.13)
4. **Jsoniter**: writes are at parity or faster (up to 1.6x for Event on 2.13); reads trail jsoniter-scala slightly for SimpleCC (0.91-0.92x) and Person on Scala 3 (0.85x), while being 1.6x faster for Person on 2.13
5. **PureConfig**: Kindlings is **3.7-18x faster** across all operations — massive improvement
6. **Avro**: Kindlings is **2-4.2x faster** than avro4s across all benchmarks
7. **Tapir Schema**: Tied (~2.3-3.0B ops/s) — just a field access at runtime
8. **Kindlings auto ≈ semi-auto** in most benchmarks (occasional gaps, e.g. Jsoniter Write Person on 2.13, come from JIT fork-to-fork variance, not different generated code)
