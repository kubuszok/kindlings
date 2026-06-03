# Kindlings: Remaining Gaps & Action Items

Last updated: 2026-06-03.

Legend: **P1** = important for migrating users, **P2** = nice to have / quality.

---

## 1. Cross-Cutting: Testing Methodology

### 1.1 Combinatorial testing matrices (P1)

Wrapper x Inner, Annotation x Type shape, Annotation x Codec direction, and Name
collision tests have been added across modules. Remaining: expand matrices to cover
more module/wrapper combinations.

---

## 2. avro-derivation

| # | Gap | Priority | Reason not yet addressed |
|---|---|---|---|
| 2.3 | `@avroNamespace` on fields (field-level override) | P2 | Requires threading a namespace override through recursive schema derivation — the annotation exists at type level but field-level support needs a cross-cutting change to the `SchemaForCtx` derivation context, not a local edit. |
| 2.6 | Streaming / container format (`AvroInputStream`/`AvroOutputStream`) | P2 | Entirely new runtime API (not a derivation gap). Needs input/output stream design around Avro's `DataFileWriter`/`DataFileReader` — unrelated to macro derivation. |

---

## 3. jsoniter-derivation

| # | Gap | Priority | Reason not yet addressed |
|---|---|---|---|
| 5.4 | `skipNestedOptionValues` — config field added | P2 | Blocked: `semiEval` cannot evaluate `JsoniterConfig` at compile time when it contains function-type fields (`fieldNameMapper: String => String = identity`). The rule needs the config value at compile time to decide the code path. Fixing requires either removing function fields from the config or adding runtime branching to the case class rule. |
| 5.5 | `alwaysEmitDiscriminator` — config field added | P2 | Blocked: same `semiEval` limitation as 5.4. Additionally, the case class encoder doesn't know its ADT parent type, so emitting a discriminator for a concrete subtype requires threading parent context through the derivation. |
| 5.6 | `inlineOneValueClasses` — config field + rule infrastructure added | P2 | Blocked: same `semiEval` limitation as 5.4. Rule files exist and are wired into the chain, but never fire because `evaluatedConfig` is always `None`. |

---

## 4. tapir-schema-derivation

| # | Gap | Priority | Reason not yet addressed |
|---|---|---|---|
| 6.2 | Scala 3 union type schemas | P2 | Blocked: Hearth's runtime type printer (`RuntimeAwareTypePrinterScala3`) triggers a cross-quotes splice error when processing union types like `String \| Int`. The derivation infrastructure (via `Enum.parse` + `isUnionType`) is ready, but the type printer crashes before the schema can be built. Requires upstream Hearth fix. |

---

## 5. cats-tagless-derivation

| # | Gap | Priority | Reason not yet addressed |
|---|---|---|---|
| 10.1 | SemigroupalK, ApplyK | P1 | Blocked: deferred to Hearth 0.4.0 which adds the dependent-type abstractions needed for higher-kinded type class derivation at kind `(* → *) → *`. |
| 10.2 | AOP / Instrumentation (`Instrument`, `Aspect`) | P2 | The cats-tagless module is an empty placeholder with no source code. All of 10.1–10.3 require bootstrapping the entire module (build config, entry points, macro impl, rules, tests). 10.2/10.3 also depend on the same HKT infrastructure as 10.1, making them impractical to implement independently. |
| 10.3 | Utility derivations (`const`, `void`, `readerT`) | P2 | Same as 10.2 — requires the cats-tagless module to be bootstrapped first via 10.1. |

---

## 6. yaml-derivation

| # | Gap | Priority | Reason not yet addressed |
|---|---|---|---|
| 11.1 | Custom YAML tags | P2 | Requires investigation of scala-yaml's `Tag` API to determine if custom tags can be applied to `Node` objects and parsed from YAML input. The library imports `org.virtuslab.yaml.Tag` but the API surface for custom tag creation and recognition is not documented and couldn't be determined from local dependency files. Needs experimentation with scala-yaml's tag system before implementation can begin. |

---

## 7. Known Bugs (Hearth-level)

| Module | Test | Issue |
|---|---|---|
| **all encoder modules** | `*.derived[CaseClassContaining[Option[SealedTrait]]]` on Scala 3 | Hearth-level splice isolation: `toValDefs.use` + `parMatchOn` interaction creates cross-Quotes references when sealed traits are derived recursively inside Option wrappers. Requires upstream Hearth fix. |
| jsoniter | `decodingOnly + encodingOnly compile error` | `semiEval` can't evaluate config in `compileErrors()` context; works in real usage |

---

## 8. Bug Pattern Analysis

6 of 10 real bugs came from features tested in isolation but never composed.

| Pattern | Issues | Mitigation |
|---|---|---|
| Combinatorial gap | #120, #78, #80, #79 | Wrapper x Inner matrices |
| Annotation tested one-direction only | #110, #108 | Round-trip tests |
| Upstream fix not ported | #92, #91 | Bug tracker scan |
| Hearth/cross-quotes | #115, #65 | Already mitigated |
| Performance/codegen | #109, #86 | Codegen audits |
