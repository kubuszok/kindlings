# Benchmark run: master @ e0534fe (Hearth 0.3.1), 2026-06-10

Full JMH run of all benchmarks, intended as the source for refreshing the published
numbers in `docs/research/benchmark-results.md` and the module user guides before
the Kindlings release.

## Environment

- **Machine**: Apple M4 Pro (14 cores), 48 GB RAM, macOS 26.5.1
- **JVM**: Eclipse Temurin 17.0.17+10 (aarch64) — both the sbt server and the JMH forks
- **Fork JVM options**: `-Dfile.encoding=UTF8 -Xms1G -Xmx3G -Xss4M -XX:+UseG1GC` (project `.jvmopts`)
- **JMH config**: 2 forks, 5 warmup iterations, 10 measurement iterations, 1s each (`-f 2 -wi 5 -i 10`)
- **Scala**: 2.13.18 (`benchmarks`), 3.8.4 (`benchmarks3`)
- **Kindlings commit**: e0534fe "Update Hearth to 0.3.1"
- **Hearth**: 0.3.1

## Files

- `scala3-temurin17.json` — full `benchmarks3/Jmh/run` results (Scala 3)
- `scala213-temurin17.json` — full `benchmarks/Jmh/run` results (Scala 2.13)
- `rerun-scala3.json` / `rerun-scala213.json` — 5-fork re-run (`-f 5 -wi 5 -i 10`) of
  families that were unstable in the 2-fork run: Cats Eq, Jsoniter Read/Write
  (SimpleCC/Person/Event), Avro (SimpleCC/Person), Circe booster Encode (Person/Event).
  **These supersede the full-run values for those benchmarks** — the 2-fork run had
  several fork-bimodal results (e.g. Avro encode SimpleCC measured ~140M there vs a
  stable ~270M here).
- `benchmark-list-scala3.txt` / `benchmark-list-scala213.txt` — `Jmh/run -l` listings

The JSON files contain per-iteration raw data. Six `original*` benchmarks failed
(broken baselines, matching the `—` cells in the docs): jsoniter-scala's own
SimpleADT/Event read codecs on both Scala versions, and avro4s semi-auto on 2.13.

Note: `docs/user-guide/cats-derivation.md` and `feature-parity.md` historically used
GraalVM CE 25 numbers; as of this run all docs are standardized on temurin 17, so their
numbers are replaced from these results as well (expect deltas vs the old GraalVM values).
