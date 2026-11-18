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
(issue [kubuszok/kindlings#85](https://github.com/kubuszok/kindlings/issues/85)). It is checked **once per macro
expansion** by a single root rule placed AFTER the "use implicit" and "use cached def" rules and BEFORE any derivation
rule: a pre-existing in-scope implicit is used without gating (it is not derivation), a cached recursive type already
passed at the first level, and when permitted at the outermost level all nested derivations are permitted too.

The shared mechanism lives in `derivation-commons`
(`hearth.kindlings.derivation.compiletime.DerivationPolicy`). Reference wiring: the **FastShowPretty** module.
User-facing docs: `docs/user-guide/derivation-policy.md` + the FAQ entry "Why not separate automatic and
semi-automatic derivation?".

## What the shared trait gives you

`trait DerivationPolicy { this: MacroCommons => ... }`:

- reuses the per-module `derivationSettingsNamespace` (same one `DerivationTimeout` uses),
- reads `<namespace>.policy.{enabled,allowedScopes,optInByImport}` from `Environment.typedSettings`,
- in `opt-in` mode, matches the full names of the enclosing owners against `allowedScopes` (package-prefix aware),
  then falls back to the opt-in import marker. The owners come from `EnclosingOwnerNames` (`scala-2`/`scala-3` sources
  walking `c.internal.enclosingOwner` / `Symbol.spliceOwner`), NOT from Hearth's `enclosingScope`: that one lists the
  members of the enclosing classes and the types of local values, which fails with "illegal cyclic reference" while the
  type of an untyped `val e = X.derived[A]` (or `val errors = compileErrors(...)`) is being inferred,
- exposes two enforcement entry points (both no-ops under the default `always-allowed`, both run the check at most once
  per expansion via an internal flag):
  - `protected def checkDerivationPolicyOncePerExpansionFor[A: Type]: MIO[Unit]` — yields `()` when permitted, else
    `MIO.fail(new DerivationPolicy.PolicyViolation(<message>))`. Used as the body of a per-module **root rule**. It
    only gates **structural** derivation: for a built-in, collection/map, `Option`, `Either`, value type, tuple or
    named tuple it yields without recording the check, so the elements are gated when they need structural derivation
    (`derived[List[User]]` at a denied site compiles when a `User` instance is in scope).
    (`checkDerivationPolicyOncePerExpansion(typeName)` gates whatever it is called for.)
  - `protected def enforceDerivationPolicyOrAbort(typeName: => String): Unit` — eager variant that calls
    `Environment.reportErrorAndAbort`, for polymorphic derivations that are NOT structured as a `Rules(...)` pipeline
    (the cats / cats-tagless `deriveXxx[F]` entry points).

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

5. **Insert ONE root rule into the `Rules(...)` chain** (for rule-pipeline modules). Define a rule whose body is the
   shared check, and place it right after the `*UseImplicit*` rule (and after any `*UseCached*` rule) and before every
   derivation rule — exactly like `FastShowPrettyMacrosImpl`:

   ```scala
   object FooDerivationPolicyRule extends FooDerivationRule("derivation policy") {
     def apply[A: FooCtx]: MIO[Rule.Applicability[Out]] =
       checkDerivationPolicyOncePerExpansionFor[A].map(_ => Rule.yielded())
   }
   // ...
   Rules(FooUseCachedRule, FooUseImplicitRule, FooDerivationPolicyRule, FooBuiltInRule, ...)
   ```

   The rule yields (so derivation proceeds) when permitted and fails the `MIO` when denied. Do **not** gate the
   individual structural rules. For a module with two chains (encoder + decoder), add one root rule per chain.

   **Polymorphic (HKT) modules** (cats `Functor`/`Apply`/`Traverse`/…, cats-tagless `*K`) don't derive through a
   `*`-kinded `Rules(...)` chain, so instead call `enforceDerivationPolicyOrAbort(<InstanceType>.prettyPrint)` once at
   the top of each `deriveXxx[F]` entry point (right after `val macroName = ...`), where the type-class instance `Type`
   is available for a good message.

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
- a derivation outside both is a compile error asserted with `compileErrors(...).check("is enabled only in the following scopes")` (or, for an opt-in build with no `allowedScopes`, `"is globally disabled"`).

Because the policy is global per compilation unit, distinct configurations need distinct modules. The `optInByImport=false`
and `always-allowed` branches are covered by the `DerivationPolicySpec` unit tests and the module's own default-config
suite, respectively — no extra module needed for those.

## Related skills

- [`../kindlings-new-module/`](../kindlings-new-module/SKILL.md) — bootstrapping a module (add the policy wiring there)
- [`../kindlings-debugging/`](../kindlings-debugging/SKILL.md) — the parallel `LogDerivation` marker / `debug` package
