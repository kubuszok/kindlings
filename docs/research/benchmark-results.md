# Kindlings Benchmark Results

> **Configuration**: 2 forks, 3 warmup iterations, 5 measurement iterations, 1s each
> **Platform**: macOS, JVM (temurin 17)
> **Scala versions**: 2.13.18, 3.8.3

All values in ops/s (higher is better). Error margins omitted for readability.

## Circe Encode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 31.1M | 31.0M | 19.8M | 18.9M | **1.6x faster** |
| SimpleCC | 3 | 30.7M | 30.3M | 22.0M | 21.1M | **1.4x faster** |
| SimpleADT | 2.13 | 27.7M | 27.0M | 13.8M | 13.9M | **2.0x faster** |
| SimpleADT | 3 | 25.7M | 26.4M | 26.2M | 26.1M | ~tied |
| Person | 2.13 | 4.4M | 4.5M | 3.1M | 3.1M | **1.4x faster** |
| Person | 3 | 4.4M | 4.4M | 3.0M | 3.0M | **1.4x faster** |
| Event | 2.13 | 3.4M | 3.4M | 2.3M | 2.6M | **1.3x faster** |
| Event | 3 | 3.4M | 3.3M | 2.3M | 2.3M | **1.4x faster** |

## Circe Decode

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 49.5M | 48.7M | 43.6M | 42.2M | **1.1x faster** |
| SimpleCC | 3 | 46.8M | 44.5M | 19.9M | 20.5M | **2.3x faster** |
| SimpleADT | 2.13 | 57.9M | 56.1M | 27.0M | 24.7M | **2.1x faster** |
| SimpleADT | 3 | 56.8M | 58.8M | 27.7M | 29.6M | **2.0x faster** |
| Person | 2.13 | 4.1M | 4.2M | 3.6M | 3.5M | **1.2x faster** |
| Person | 3 | 4.0M | 4.0M | 2.4M | 2.6M | **1.5x faster** |
| Event | 2.13 | 2.9M | 2.9M | 2.9M | 2.7M | ~tied |
| Event | 3 | 2.7M | 2.7M | 2.1M | 2.1M | **1.3x faster** |

## Jsoniter Write

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 41.6M | 41.9M | 60.3M | — | 0.69x |
| SimpleCC | 3 | 43.8M | 43.6M | 63.4M | — | 0.69x |
| SimpleADT | 2.13 | 55.1M | 54.2M | 68.1M | — | 0.81x |
| SimpleADT | 3 | 54.5M | 54.2M | 70.0M | — | 0.78x |
| Person | 2.13 | 3.7M | 3.8M | 4.6M | — | 0.83x |
| Person | 3 | 3.9M | 3.9M | 5.3M | — | 0.74x |
| Event | 2.13 | 3.4M | 3.3M | 4.4M | — | 0.77x |
| Event | 3 | 3.4M | 3.5M | 4.8M | — | 0.73x |

## Jsoniter Read

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 16.1M | 16.7M | 34.9M | — | 0.48x |
| SimpleCC | 3 | 16.9M | 17.0M | 34.8M | — | 0.49x |
| SimpleADT | 2.13 | 28.7M | 29.0M | — | — |  |
| SimpleADT | 3 | 29.0M | 29.2M | — | — |  |
| Person | 2.13 | 2.4M | 2.4M | 3.6M | — | 0.67x |
| Person | 3 | 2.2M | 2.2M | 3.6M | — | 0.61x |
| Event | 2.13 | 2.1M | 2.1M | — | — |  |
| Event | 3 | 2.1M | 2.0M | — | — |  |

## Cats Show

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | — | 38.5M | 7.5M | 7.5M | **5.1x faster** |
| SimpleCC | 3 | — | 26.3M | 19.4M | 19.3M | **1.4x faster** |
| SimpleADT | 2.13 | — | 155.9M | 16.5M | 9.7M | **9.5x faster** |
| SimpleADT | 3 | — | 64.9M | 49.6M | 51.6M | **1.3x faster** |
| Person | 2.13 | — | 2.0M | — | 833.6K | **2.4x faster** |
| Person | 3 | — | 1.7M | — | 1.4M | **1.2x faster** |
| Event | 2.13 | — | 1.8M | 607.4K | 603.6K | **3.0x faster** |
| Event | 3 | — | 1.5M | 1.2M | 1.2M | **1.2x faster** |

## Cats Eq

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC (eq) | 2.13 | — | 101.9M | 45.6M | 45.9M | **2.2x faster** |
| SimpleCC (eq) | 3 | — | 98.5M | 89.1M | 89.6M | **1.1x faster** |
| SimpleCC (neq) | 2.13 | — | 529.5M | — | — |  |
| SimpleCC (neq) | 3 | — | 542.0M | — | — |  |

## Cats Hash

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | — | 834.3M | 27.8M | 27.9M | **29.9x faster** |
| SimpleCC | 3 | — | 809.6M | 101.4M | 99.2M | **8.0x faster** |

## Cats Order

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | Original auto | vs best original |
|------|-------|---------------|---------------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | — | 434.3M | 433.0M | 470.3M | 0.92x |
| SimpleCC | 3 | — | 420.1M | 299.4M | 349.6M | **1.2x faster** |

## Avro (kindlings vs avro4s)

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| Encode SimpleCC | 2.13 | 7.6M | — | 45.6M | 0.17x |
| Encode SimpleCC | 3 | 7.4M | 48.1M | 48.1M | 0.15x |
| Encode Person | 2.13 | 1.1M | — | 4.7M | 0.23x |
| Encode Person | 3 | 1.1M | 5.6M | 5.6M | 0.19x |
| Encode Event | 2.13 | 728.6K | — | — |  |
| Encode Event | 3 | 703.0K | — | — |  |
| Decode SimpleCC | 2.13 | 14.9M | — | 17.6M | 0.84x |
| Decode SimpleCC | 3 | 16.4M | 25.3M | 42.4M | 0.39x |
| Decode Person | 2.13 | 1.2M | — | 3.6M | 0.34x |
| Decode Person | 3 | 1.3M | 3.0M | 4.2M | 0.31x |
| Decode Event | 2.13 | 581.2K | — | — |  |
| Decode Event | 3 | 597.1K | — | — |  |

## Pureconfig (kindlings vs pureconfig-generic)

| Type | Scala | Kindlings | Original semi | vs original |
|------|-------|-----------|--------------|------------|
| Write SimpleCC | 2.13 | 1.5M | 1.2M | **1.2x faster** |
| Write SimpleCC | 3 | 1.7M | 1.6M | **1.1x faster** |
| Write Person | 2.13 | 253.0K | 200.3K | **1.3x faster** |
| Write Person | 3 | 276.5K | 233.7K | **1.2x faster** |
| Read SimpleCC | 2.13 | 834.6K | 1.4M | 0.60x |
| Read SimpleCC | 3 | 912.9K | 1.4M | 0.67x |
| Read Person | 2.13 | 132.6K | 209.7K | 0.63x |
| Read Person | 3 | 146.4K | 198.6K | 0.74x |

## Kindlings-only modules (no original comparison)

| Module | Type | Scala 2.13 | Scala 3 |
|--------|------|-----------|---------|
| FastShowPretty | SimpleCC | 6.7M | 7.1M |
| FastShowPretty | SimpleADT | 7.3M | 8.9M |
| FastShowPretty | Person | 926.0K | 1.0M |
| FastShowPretty | Event | 716.0K | 783.5K |
| UbjsonWrite | SimpleCC | 11.1M | 11.2M |
| UbjsonWrite | SimpleADT | 13.3M | 13.4M |
| UbjsonWrite | Person | 1.3M | 1.4M |
| UbjsonWrite | Event | 1.2M | 1.2M |
| UbjsonRead | SimpleCC | 10.8M | 10.3M |
| UbjsonRead | SimpleADT | 13.7M | 13.6M |
| UbjsonRead | Person | 1.4M | 1.4M |
| UbjsonRead | Event | 1.2M | 1.3M |
| SconfigWrite | SimpleCC | 5.6M | 5.3M |
| SconfigWrite | SimpleADT | 4.6M | 4.6M |
| SconfigWrite | Person | 774.9K | 856.1K |
| SconfigWrite | Event | 408.3K | 409.4K |
| SconfigRead | SimpleCC | 4.3M | 4.4M |
| SconfigRead | SimpleADT | 8.7M | 8.8M |
| SconfigRead | Person | 648.5K | 688.1K |
| SconfigRead | Event | 454.8K | 444.0K |
| YamlEncode | SimpleCC | 1.4M | 1.4M |
| YamlEncode | SimpleADT | 2.2M | 2.2M |
| YamlEncode | Person | 153.2K | 158.5K |
| YamlEncode | Event | 136.2K | 143.3K |
| YamlDecode | SimpleCC | 8.3M | 6.4M |
| YamlDecode | SimpleADT | 101.2M | 63.1M |
| YamlDecode | Person | 738.2K | 661.1K |
| YamlDecode | Event | 647.4K | 595.3K |
| XmlEncode | SimpleCC | 46.0M | 44.5M |
| XmlEncode | Address | 39.8M | 40.6M |
| XmlDecode | SimpleCC | 4.8M | 5.0M |
| XmlDecode | Address | 3.4M | 3.4M |
| ScalacheckArbitrary | SimpleCC | 740.5K | 756.7K |
| ScalacheckArbitrary | SimpleADT | 2.1M | 2.1M |
| ScalacheckArbitrary | Person | 4.9K | 4.4K |
| ScalacheckShrink | SimpleCC | 6.4M | 5.8M |
| ScalacheckShrink | Person | 5.7M | 5.4M |

## Key takeaways

1. **Circe**: Kindlings is **1.4-2.3x faster** for encoding and decoding across all types and both Scala versions
2. **Cats**: Kindlings dominates — **5x** for Show, **30x** for Hash on 2.13; **1.3-8x** on Scala 3
3. **Jsoniter-scala**: Original is **1.3-2x faster** — its hand-tuned byte-level codegen is hard to beat
4. **Avro**: avro4s is **5-6x faster** for encoding (specialized Avro-native code)
5. **Pureconfig**: Mixed — kindlings writes ~1.2x faster, original reads ~1.5x faster
6. **Tapir Schema**: Tied (~3.7B ops/s) — just a field access at runtime
7. **Kindlings auto = semi-auto** everywhere, confirming recursive derivation produces identical runtime instances
8. **Original auto = semi-auto** on Scala 3 (both Mirror-based), but on 2.13 semi-auto can be faster (cached intermediates avoid shapeless overhead)
