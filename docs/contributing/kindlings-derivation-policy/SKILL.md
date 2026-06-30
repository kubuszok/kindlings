---
name: kindlings-derivation-policy
description: >
  Wiring the per-library derivation policy (issue #85) into a derivation module: the shared
  DerivationPolicy trait, the AllowDerivation marker + policy package object, gating the structural
  case-class/enum rules, the ;/| list-encoding gotcha, and the integration test module.
paths:
  - "*-derivation/src/main/scala/**/internal/compiletime/**/*.scala"
  - "fast-show-pretty/src/main/scala/**/*.scala"
  - "derivation-commons/src/main/scala/**/DerivationPolicy.scala"
user-invocable: false
---

# Derivation Policy

The **derivation policy** lets a build globally opt out of automatic (structural) derivation per library
(issue [kubuszok/kindlings#85](https://github.com/kubuszok/kindlings/issues/85)). It gates ONLY the structural rules
(case class + enum); built-in support, collections, `Option`, value types, named tuples, and any pre-existing in-scope
implicit are reached BEFORE the structural rules and stay unconditional.

The shared mechanism lives in `derivation-commons`
(`hearth.kindlings.derivation.compiletime.DerivationPolicy`). Reference wiring: the **FastShowPretty** module.
User-facing docs: `docs/user-guide/derivation-policy.md` + the FAQ entry "Why not separate automatic and
semi-automatic derivation?".

## What the shared trait gives you

`trait DerivationPolicy { this: MacroCommons => ... }`:

- reuses the per-module `derivationSettingsNamespace` (same one `DerivationTimeout` uses),
- reads `<namespace>.policy.{enabled,allowedScopes,optInByImport}` from `Environment.typedSettings`,
- in `opt-in` mode, matches `enclosingScope` (the cake-level `enclosingScope: NonEmptyVector[Enclosure]`, Hearth ≥ 0.4.0)
  against `allowedScopes` (package-prefix aware), then falls back to the opt-in import marker,
- exposes `protected def enforceDerivationPolicy[A: Type]: MIO[Unit]` — `MIO.pure(())` when allowed, else
  `MIO.fail(new DerivationPolicy.PolicyViolation(<actionable message>))`.

The decision is a memoized `lazy val derivationPolicyDecision`, so it is computed once per expansion and short-circuits
on the default `always-allowed` path before touching `enclosingScope`.

Pure, unit-tested helpers live in the `DerivationPolicy` companion: `parseMode`, `splitScopes`, `scopeMatches`,
`scopeAllows`, `decide`. See `DerivationPolicySpec` in `derivation-commons`.

## Wiring a module (the rollout pattern)

1. **Mix the trait in** next to `DerivationTimeout` on the module's `*MacrosImpl`:

   ```scala
   trait FooMacrosImpl
       extends hearth.kindlings.derivation.compiletime.DerivationTimeout
       with hearth.kindlings.derivation.compiletime.DerivationPolicy
       with ... { this: MacroCommons & StdExtensions =>
   ```

2. **Implement the three abstract members** (`derivationSettingsNamespace` already exists for the timeout):

   ```scala
   override protected def derivationPolicyTypeClassName: String = "Foo"
   override protected def derivationOptInImportHint: String =
     "import hearth.kindlings.foo.policy.allowDerivationForFoo"
   override protected def isDerivationOptInMarkerInScope: Boolean = {
     implicit val AllowDerivation: Type[Foo.AllowDerivation] = Types.AllowDerivation
     Expr.summonImplicit[Foo.AllowDerivation].isDefined
   }
   ```

   For a multi-method / multi-entrypoint module (codec = encoder + decoder), the type class name is the user-facing
   name; mix `DerivationPolicy` into whichever `*MacrosImpl` owns the structural rules.

3. **Add the `AllowDerivation` marker** to the type class companion — exactly parallel to the existing `LogDerivation`
   marker, kept OUTSIDE the implicit scope so it is never auto-summoned:

   ```scala
   sealed trait AllowDerivation
   object AllowDerivation extends AllowDerivation
   ```

   and cache its `Type` in the `Types` object:

   ```scala
   val AllowDerivation: Type[Foo.AllowDerivation] = Type.of[Foo.AllowDerivation]
   ```

4. **Add a `policy` package object** (parallel to the `debug` package object) exposing the importable marker:

   ```scala
   package object policy {
     implicit val allowDerivationForFoo: Foo.AllowDerivation = Foo.AllowDerivation
   }
   ```

5. **Gate the structural rules.** Inside the MATCHED branch of the case-class rule and the enum rule (after
   `CaseClass.parse`/`Enum.parse` succeeds), prefix the body with `enforceDerivationPolicy[A] >>`:

   ```scala
   case Right(caseClass) =>
     enforceDerivationPolicy[A] >> {
       ... existing body returning MIO[Rule.Applicability[...]] ...
     }
   ```

   Do NOT gate before the parse, and do NOT gate the non-structural rules — that would block found implicits, built-ins,
   collections, etc. A denied `enforceDerivationPolicy` fails the `MIO`, which the module's existing entrypoint error
   path renders.

## List encoding gotcha (`allowedScopes`)

`allowedScopes` is a single setting split on `;` or `|` — **never `,`**. Scala 3 splits a single `-Xmacro-settings:a,b`
option on commas into two settings; Scala 2 keeps `"a,b"` as one string. Hearth's `Data.parseList` also never produces a
list from settings (repeated leaf keys are an error). `;`/`|` are untouched by both compilers, so the value arrives
intact and `DerivationPolicy.splitScopes` splits it.

## Testing

The integration module `derivation-policy-tests` (depends on `fastShowPretty`, with a single global
`Test / scalacOptions` policy config) covers the end-to-end behavior:

- a derivation in an `allowedScopes` package compiles (and renders at runtime),
- a derivation behind the imported opt-in marker compiles,
- a derivation outside both is a compile error asserted with `compileErrors(...).check("is not allowed at this location")`.

Because the policy is global per compilation unit, distinct configurations need distinct modules. The `optInByImport=false`
and `always-allowed` branches are covered by the `DerivationPolicySpec` unit tests and the module's own default-config
suite, respectively — no extra module needed for those.

## Related skills

- [`../kindlings-new-module/`](../kindlings-new-module/SKILL.md) — bootstrapping a module (add the policy wiring there)
- [`../kindlings-debugging/`](../kindlings-debugging/SKILL.md) — the parallel `LogDerivation` marker / `debug` package
