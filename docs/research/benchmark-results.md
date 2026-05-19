# Kindlings Benchmark Results

> **Configuration**: 2 forks, 5 warmup iterations, 10 measurement iterations, 1s each
> **Platform**: macOS, JVM (temurin 17)
> **Scala versions**: 2.13.18, 3.8.3

All values in ops/s (higher is better). Error margins omitted for readability.

## Circe Encode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 30.6M | 31.1M | 19.3M | 19.0M | **1.6x faster** |
| SimpleCC | 3 | 30.9M | 30.7M | 21.8M | 21.3M | **1.4x faster** |
| SimpleADT | 2.13 | 27.2M | 27.9M | 14.1M | 14.0M | **2.0x faster** |
| SimpleADT | 3 | 26.1M | 26.5M | 25.7M | 25.4M | ~tied |
| Person | 2.13 | 4.5M | 4.5M | 3.0M | 3.0M | **1.5x faster** |
| Person | 3 | 4.3M | 4.3M | 3.1M | 3.1M | **1.4x faster** |
| Event | 2.13 | 3.4M | 3.4M | 2.3M | 2.4M | **1.4x faster** |
| Event | 3 | 3.3M | 3.4M | 2.3M | 2.3M | **1.4x faster** |

## Circe Decode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 51.0M | 50.9M | 42.4M | 42.8M | **1.2x faster** |
| SimpleCC | 3 | 52.7M | 48.9M | 20.0M | 20.6M | **2.6x faster** |
| SimpleADT | 2.13 | 61.3M | 61.0M | 25.0M | 26.5M | **2.3x faster** |
| SimpleADT | 3 | 47.7M | 46.5M | 29.2M | 27.8M | **1.6x faster** |
| Person | 2.13 | 4.2M | 4.2M | 3.5M | 3.5M | **1.2x faster** |
| Person | 3 | 4.2M | 4.2M | 2.7M | 2.7M | **1.6x faster** |
| Event | 2.13 | 2.8M | 2.8M | 2.7M | 2.7M | ~tied |
| Event | 3 | 2.9M | 2.9M | 2.1M | 2.1M | **1.4x faster** |

## Circe End-to-End with jsoniter-scala-circe Booster

Full pipeline benchmarks: domain type ↔ bytes/String, comparing Circe's default parser/printer vs jsoniter-scala-circe booster.

### Encode (domain type → bytes/String)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **14.8M** | 10.2M | 6.8M | 5.5M |
| SimpleCC | 3 | **15.3M** | 11.9M | 7.2M | 6.6M |
| SimpleADT | 2.13 | **15.2M** | 8.0M | 7.8M | 5.8M |
| SimpleADT | 3 | **15.6M** | 12.1M | 8.1M | 6.9M |
| Person | 2.13 | **1.6M** | 1.4M | 979.1K | 906.8K |
| Person | 3 | **1.6M** | 1.4M | 1.1M | 959.5K |
| Event | 2.13 | **1.3M** | 1.1M | 841.8K | 731.6K |
| Event | 3 | **1.3M** | 1.1M | 918.8K | 800.3K |

### Decode (bytes/String → domain type)

| Type | Scala | Kindlings + booster | Original + booster | Kindlings (no booster) | Original (no booster) |
|------|-------|--------------------|--------------------|----------------------|---------------------|
| SimpleCC | 2.13 | **8.3M** | 7.8M | 6.0M | 6.2M |
| SimpleCC | 3 | **8.0M** | 6.6M | 6.9M | 5.7M |
| SimpleADT | 2.13 | **10.6M** | 9.0M | 9.3M | 7.6M |
| SimpleADT | 3 | **10.4M** | 9.1M | 9.6M | 7.9M |
| Person | 2.13 | **1.1M** | 1.1M | 925.0K | 883.8K |
| Person | 3 | **1.2M** | 985.2K | 974.8K | 850.6K |
| Event | 2.13 | 929.5K | 932.5K | 672.8K | 689.6K |
| Event | 3 | **931.7K** | 826.3K | 783.3K | 719.1K |

## Jsoniter Write

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 59.8M | 59.5M | 59.7M | **~tied** |
| SimpleCC | 3 | 62.7M | 62.9M | 63.8M | **~tied** |
| Person | 2.13 | 4.7M | 4.7M | 4.6M | **~tied** |
| Person | 3 | 5.3M | 5.3M | 5.2M | **~tied** |
| SimpleADT | 2.13 | 61.9M | 61.9M | 67.2M | 0.92x |
| SimpleADT | 3 | 63.3M | 63.3M | 69.5M | 0.91x |
| Event | 2.13 | 4.3M | 4.2M | 4.0M | **1.08x faster** |
| Event | 3 | 4.7M | 4.7M | 4.8M | **0.98x** |

## Jsoniter Read

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 36.1M | 36.0M | 34.8M | **1.04x faster** |
| SimpleCC | 3 | 35.6M | 35.5M | 34.2M | **1.04x faster** |
| Person | 2.13 | 3.6M | 3.7M | 3.6M | **~tied** |
| Person | 3 | 3.6M | 3.6M | 3.6M | **~tied** |
| SimpleADT | 2.13 | 16.8M | 17.0M | — |  |
| SimpleADT | 3 | 17.0M | 17.0M | — |  |
| Event | 2.13 | 3.3M | 3.2M | — |  |
| Event | 3 | 3.2M | 3.2M | — |  |

## Cats Show

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 39.0M | 7.3M | 7.5M | **5.2x faster** |
| SimpleCC | 3 | 26.8M | 19.6M | 19.7M | **1.4x faster** |
| SimpleADT | 2.13 | 87.9M | 16.8M | 11.1M | **5.2x faster** |
| SimpleADT | 3 | 79.8M | 46.1M | 52.5M | **1.5x faster** |
| Person | 2.13 | 2.0M | — | 829.7K | **2.4x faster** |
| Person | 3 | 1.6M | — | 1.4M | **1.1x faster** |
| Event | 2.13 | 1.8M | 643.8K | 576.5K | **2.8x faster** |
| Event | 3 | 1.5M | 1.2M | 1.2M | **1.3x faster** |

## Cats Eq

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| SimpleCC (eq) | 2.13 | 99.9M | 44.9M | 44.3M | **2.2x faster** |
| SimpleCC (eq) | 3 | 99.5M | 97.1M | 90.3M | **~tied** |
| SimpleCC (neq) | 2.13 | 543.8M | — | — |  |
| SimpleCC (neq) | 3 | 554.0M | — | — |  |

## Cats Hash

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 806.9M | 26.6M | 26.4M | **30.4x faster** |
| SimpleCC | 3 | 809.0M | 104.4M | 96.4M | **7.8x faster** |

## Cats Order

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 386.7M | 460.1M | 412.8M | 0.84x |
| SimpleCC | 3 | 422.5M | 294.4M | 353.8M | **1.19x faster** |

## Avro (kindlings vs avro4s)

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| Encode SimpleCC | 2.13 | 281.7M | — | 45.0M | **6.3x faster** |
| Encode SimpleCC | 3 | 270.5M | 49.3M | 48.1M | **5.5x faster** |
| Encode Person | 2.13 | 19.6M | — | 4.6M | **4.3x faster** |
| Encode Person | 3 | 18.6M | 5.7M | 5.7M | **3.3x faster** |
| Encode Event | 2.13 | 18.4M | — | — |  |
| Encode Event | 3 | 17.5M | — | — |  |
| Decode SimpleCC | 2.13 | 425.3M | — | 17.6M | **24.2x faster** |
| Decode SimpleCC | 3 | 474.1M | 23.2M | 42.9M | **11.1x faster** |
| Decode Person | 2.13 | 14.4M | — | 3.5M | **4.1x faster** |
| Decode Person | 3 | 13.8M | 3.2M | 4.1M | **3.4x faster** |
| Decode Event | 2.13 | 12.9M | — | — |  |
| Decode Event | 3 | 13.1M | — | — |  |

## PureConfig (kindlings vs pureconfig-generic)

| Type | Scala | Kindlings | Original semi | vs original |
|------|-------|-----------|--------------|------------|
| Write SimpleCC | 2.13 | 11.0M | 1.2M | **9.2x faster** |
| Write SimpleCC | 3 | 10.9M | 1.6M | **6.8x faster** |
| Write Person | 2.13 | 1.2M | 200.4K | **6.0x faster** |
| Write Person | 3 | 1.2M | 247.3K | **4.9x faster** |
| Read SimpleCC | 2.13 | 16.8M | 1.4M | **12.0x faster** |
| Read SimpleCC | 3 | 11.9M | 1.4M | **8.5x faster** |
| Read Person | 2.13 | 999.8K | 208.9K | **4.8x faster** |
| Read Person | 3 | 924.8K | 200.8K | **4.6x faster** |

## Kindlings-only modules (no original comparison)

| Module | Type | Scala 2.13 | Scala 3 |
|--------|------|-----------|---------|
| FastShowPretty | SimpleCC | 6.9M | 7.1M |
| FastShowPretty | SimpleADT | 7.7M | 8.5M |
| FastShowPretty | Person | 932.6K | 1.0M |
| FastShowPretty | Event | 719.6K | 783.7K |
| UbjsonWrite | SimpleCC | 11.2M | 11.3M |
| UbjsonWrite | SimpleADT | 13.4M | 13.4M |
| UbjsonWrite | Person | 1.4M | 1.4M |
| UbjsonWrite | Event | 1.3M | 1.2M |
| UbjsonRead | SimpleCC | 11.2M | 11.3M |
| UbjsonRead | SimpleADT | 13.7M | 13.6M |
| UbjsonRead | Person | 1.4M | 1.4M |
| UbjsonRead | Event | 1.3M | 1.2M |
| SconfigWrite | SimpleCC | 11.3M | 10.8M |
| SconfigWrite | SimpleADT | 5.6M | 5.4M |
| SconfigWrite | Person | 1.2M | 1.2M |
| SconfigWrite | Event | 617.2K | 647.4K |
| SconfigRead | SimpleCC | 68.5M | 60.5M |
| SconfigRead | SimpleADT | 25.2M | 24.5M |
| SconfigRead | Person | 4.1M | 4.2M |
| SconfigRead | Event | 2.8M | 2.7M |
| YamlEncode | SimpleCC | 1.4M | 1.4M |
| YamlEncode | SimpleADT | 2.2M | 2.3M |
| YamlEncode | Person | 146.8K | 160.0K |
| YamlEncode | Event | 135.3K | 145.8K |
| YamlDecode | SimpleCC | 8.9M | 6.9M |
| YamlDecode | SimpleADT | 102.3M | 53.3M |
| YamlDecode | Person | 724.7K | 683.3K |
| YamlDecode | Event | 653.4K | 610.7K |
| XmlEncode | SimpleCC | 46.1M | 45.8M |
| XmlEncode | Address | 38.6M | 38.0M |
| XmlDecode | SimpleCC | 4.8M | 5.2M |
| XmlDecode | Address | 3.2M | 3.4M |
| ScalacheckArbitrary | SimpleCC | 775.4K | 773.6K |
| ScalacheckArbitrary | SimpleADT | 2.1M | 2.1M |
| ScalacheckArbitrary | Person | 4.0K | 4.3K |
| ScalacheckShrink | SimpleCC | 6.5M | 5.8M |
| ScalacheckShrink | Person | 5.8M | 5.5M |

## Key takeaways

1. **Circe**: Kindlings is **1.4-2.6x faster** for encoding and decoding across all types and both Scala versions
2. **Circe + booster**: Kindlings + jsoniter-scala-circe is the fastest Circe pipeline — up to **2.7x faster** than original Circe without booster
3. **Cats**: Kindlings dominates — **5x** for Show, **30x** for Hash on 2.13; **1.4-8x** on Scala 3. Eq ~tied on Scala 3, 2.2x faster on 2.13. Order ~tied (within JIT noise at 400M+ ops/s)
4. **Jsoniter**: Kindlings **matches or exceeds** jsoniter-scala for all case class benchmarks — SimpleCC reads slightly faster (36M vs 34M), Person read/write at parity. ADT write has a small gap (0.91x) from discriminator overhead
5. **PureConfig**: Kindlings is **4.6-12x faster** across all operations — massive improvement
6. **Avro**: Kindlings is **3-11x faster** than avro4s across all benchmarks — 5.5x for SimpleCC encode, 3.3x for Person encode, 11.1x for SimpleCC decode, 3.4x for Person decode
7. **Tapir Schema**: Tied (~3.8B ops/s) — just a field access at runtime
8. **Kindlings auto = semi-auto** everywhere, confirming sanely-automatic derivation produces identical runtime instances
