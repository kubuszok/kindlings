# Benchmark run — 2026-06-17, Hearth 0.3.1-53

- **Config**: 2 forks, 5 warmup, 10 measurement iterations, 1s each (JMH).
- **Scala**: 2.13.18, 3.8.3. **Hearth**: 0.3.1-53-g5032722-SNAPSHOT.
- **Branch**: benchmarks-and-docs (this session's optimizations: zero-closure foreach/Either fold,
  fail-fast Either decode for circe/yaml/pureconfig, parse-once map/collection merge across all
  modules, avro decode single-lookup + semiEval constant field names).
- **Caveat**: run on a **shared dev machine with concurrent agent load** (~2h/version). The
  kindlings/original **ratios** within each run are robust; absolute ops/s are approximate. A small
  number of *original* (auto) measurements were noisy-high (e.g. avro4s auto decode SimpleCC, some
  cats auto) which understates a few ratios.
- Raw JMH JSON: `scala3.json`, `scala213.json`. Per-method both-version table: `summary.md`.
