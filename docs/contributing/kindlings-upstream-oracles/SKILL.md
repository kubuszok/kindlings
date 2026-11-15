---
name: kindlings-upstream-oracles
description: Differential tests against the upstream libraries Kindlings mirrors (upstream-oracle-tests module). How to add a suite, the exact-differences rule, and where regression tests for downstream reports belong.
---

# Upstream oracle tests

Kindlings modules are supersets of the libraries they mirror (tapir, circe, jsoniter-scala, Iron's integrations, ...):
same behaviour by default, extra behaviour only as opt-in config, differences only when documented. Reading upstream's
docs and feature lists ("parity audits") misses what the derived instances actually *do*, so the
`upstream-oracle-tests` module checks it: the upstream library is a test-only dependency, and each suite derives the same
fixtures with Kindlings and with upstream and compares the results.

Each project is tested in isolation: no downstream project (Chimney, applications) is needed to catch a regression.

## The module

- `upstream-oracle-tests/` — JVM only, Scala 2.13 + 3, not published, part of the `test-jvm-*` aliases.
- `Oracle.scala` — `Oracle.diff(kindlings, upstream)` (a structural diff of case classes / maps / sequences reporting
  every differing path) and `Oracle.jsonSchema(schema)` (tapir's docs interpreter, i.e. what users see in OpenAPI).
- Suites: `TapirSchemaOracleSpec` (tapir's `generic.auto` with the equivalent tapir `Configuration`),
  `TapirCatsOracleSpec` (tapir-cats), `scala-3/IronOracleSpec` (tapir-iron, iron-circe, iron-jsoniter).
- `-Xcheck-macros` is off in this module (upstream macros fail it); Kindlings' macros are checked in their own modules.

## Rules

1. **Compare what users observe**: rendered JSON Schemas, encoded JSON, accepted/rejected inputs and decoded values —
   not internal representations.
2. **Assert the exact list of differences.** Each expected difference is written out in the test with a comment saying
   why it is intentional and where it is documented (`docs/user-guide/feature-parity.md`, the module's guide). A new
   difference fails the test, and so does a documented one that disappears (update the docs then).
3. **Fix undocumented differences toward upstream** (see the upstream-parity policy), or add upstream's missing flag.
   Extra Kindlings behaviour goes behind opt-in config.
4. **Use upstream the way its docs say** (e.g. tapir: `generic.auto` + `implicitly`, recursive schemas declared
   explicitly — `implicit lazy val` on Scala 2, `implicit def` on Scala 3; hand-written schemas where upstream has no
   derivation), so the oracle reflects what upstream users get.
5. **Cover inputs that should be rejected**, not just round trips: upstream often differs in what it accepts.

## Downstream reports: test at the owning layer

When a report comes from a downstream project, reduce it to the lowest layer that owns the bug and keep the regression
test there:

- a Hearth type-system / macro bug (e.g. export aliases, hygiene of generated trees) → a Hearth test (and a reproducer in
  `docs/research/` here while Hearth is not fixed);
- a Kindlings behaviour difference from upstream → an oracle suite here;
- an inconsistency between Kindlings modules (e.g. a tapir schema vs the codec derived with the same config) → a
  consistency test in the module (e.g. `AbsentFieldSchemaSpec`, `SchemaConformance`).

No project should have to build another project to find its own bugs.

## Adding a suite

1. Add the upstream library to `upstreamOracleTests` in `build.sbt` as a `% Test` dependency (Scala 3-only libraries in
   the `for3` branch, tests in `src/test/scala-3`).
2. Put fixtures next to the suite (top-level, so both sides derive the same types). Derive the Kindlings side and the
   upstream side in separate objects, so each sees only its own implicits/config.
3. Start with `expectedDifferences = Nil`, run, and triage every reported difference: fix it, or document it and list it.
