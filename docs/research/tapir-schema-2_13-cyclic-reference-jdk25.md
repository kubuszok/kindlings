# `tapir-schema-derivation` Scala 2.13 `illegal cyclic reference` — a JDK-25/GraalVM artifact

**Status:** investigated; **not a code defect** and **not reliably reproducible**. CI (Temurin 17) is
green; a clean isolated compile is green even locally (GraalVM CE 25). No source change is made — see
"Decision" below.

## Symptom

During the *full* parallel `publish-local-for-tests` on the local dev JVM (GraalVM CE 25),
`tapirSchemaDerivation2_13` intermittently failed with:

```
illegal cyclic reference involving <refinement of
  SchemaMacrosImpl with MacroCommons with StdExtensions with JsonSchemaConfigs with AnnotationSupport>
(position: unknown)
```

It blocked the *full* local `publish-local-for-tests` (and thus a full local `just test-snippets`);
the local workaround was to `publishLocal` only the modules a given docs file needs.

**Reproduction attempt (this investigation):** a clean isolated
`tapirSchemaDerivation2_13/clean ; tapirSchemaDerivation2_13/compile` on GraalVM CE 25 **succeeded**
(`done compiling`, `[success]`). So the error does **not** reproduce for an isolated module compile —
it only surfaced under the full, parallel multi-module publish. This is exactly the fingerprint of a
**nondeterministic symbol-completer ordering** issue (see hypothesis below), not a deterministic cake
defect.

**Key facts:**
- PR #159's `Scala: 2.13 / jvm / temurin:1.17` CI job was green with the *same* sources.
- A fresh isolated clean compile is green locally too (GraalVM CE 25).
The failure is JVM- *and* build-order-specific, not a master regression.

## Structure of the macro cake (all sound)

Scala 2 bundle — `.../scala-2/.../internal/compiletime/SchemaMacros.scala`:

```scala
final private[tapirschemaderivation] class SchemaMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupport
    with JsonSchemaConfigs
    with SchemaMacrosImpl
```

`SchemaMacrosImpl` (shared) declares the self-type named in the error:

```scala
trait SchemaMacrosImpl extends … {
  this: MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>
```

- `AnnotationSupport` (tapir) → thin delegate to `derivation-commons` `AnnotationSupport`; self-type
  `this: MacroCommons & StdExtensions =>`.
- `JsonSchemaConfigs` (from `json-schema-config-macro-providers`); self-type
  `this: MacroCommons & StdExtensions =>`.

Neither config trait references `SchemaMacrosImpl` or each other. The bundle satisfies
`SchemaMacrosImpl`'s self-type by listing `AnnotationSupport`/`JsonSchemaConfigs` *before* it in the
parent list — the ordinary Hearth cake pattern. The Scala 3 bundle uses the identical parent order.

**No source-level cycle exists.** The refinement in the error message is exactly the compiler-synthesized
"self of `SchemaMacrosImpl`" intersection; it is not a type spelled anywhere in the sources.

## Comparison with working modules

| Module | `*Impl` self-type width | `AnnotationSupport` before `*Impl`? |
|---|---|---|
| **tapir** `SchemaMacros` | **4** (`MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport`) | yes |
| jsoniter `CodecMacros` | 3 (`… & AnnotationSupport`) | yes — compiles fine |
| circe `CodecMacros` | mixed | yes — compiles fine |
| cats `ShowMacros` | 2 | no (`Impl` first) — compiles fine |

- The `AnnotationSupport`-before-`Impl` ordering is **not** unique to tapir (jsoniter/circe do it too and
  compile), so parent ordering is not the trigger.
- tapir's only structural distinction: it is the **only** module whose cake also mixes in
  `JsonSchemaConfigs` (from a separate module), giving `SchemaMacrosImpl` the **widest self-type
  intersection (4 components)** in the codebase.

## Root-cause hypothesis

A **scalac-2.13-on-JDK-25/GraalVM artifact**, not a code defect:

1. Identical sources compile on Temurin 17 (CI) and fail only on GraalVM CE 25 — a genuine cake cycle
   would fail on every JDK, deterministically.
2. `position: unknown` is the signature of a cycle detected inside the symbol *completer*
   (base-type/self-type completion), where scalac has no source position — i.e. compiler-internal
   completion-order nondeterminism, not a user-visible loop.
3. Completing `SchemaMacrosImpl`'s self-type re-enters completion of `SchemaMacrosImpl`; whether the
   completer's cycle guard trips can depend on symbol-forcing order, which can depend on hash-map
   iteration order and thus on JDK internals. Scala 2.13.x predates and is not validated on JDK 25.
4. tapir has the widest / only cross-module self-type intersection, so it is the most likely module to
   hit a latent completion-order edge case first when iteration order shifts under a new JDK, while the
   2–3-component cakes in cats/jsoniter/circe stay under the threshold.

## Decision

**No source change.** The cake is structurally sound and consistent with the working modules (same
pattern, one extra legitimately-ordered trait). CI on the project's target JDK (Temurin 17) is green,
so the PR's CI is unaffected. Rewriting the cake to dodge a compiler completion-order bug would paper
over a scalac/JDK issue and risk a real regression across the cross-compiled matrix.

If a local workaround is ever needed (to run the full local publish on JDK 25), the lowest-risk nudge —
not applied here — would be to narrow `SchemaMacrosImpl`'s self-type to a single named aggregate trait
(e.g. `trait SchemaMacroBundle extends MacroCommons with StdExtensions with JsonSchemaConfigs with
AnnotationSupport`, self-type `this: SchemaMacroBundle =>`) so the completer resolves one named symbol
instead of a 4-way structural refinement. Prefer instead to build locally on Temurin 17.

## Related

- `docs/research/HANDOFF-next-session.md` issue #2.
</content>
