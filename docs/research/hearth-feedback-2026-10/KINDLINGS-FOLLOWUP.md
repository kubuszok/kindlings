# Kindlings follow-up once the Hearth fixes are released

This is a hand-off for the agent that moves kindlings onto the Hearth snapshot carrying the fixes for the items in
this directory. For each item it gives the kindlings workaround to remove (or feature to add) and the test that proves
it. Background and evidence: [`../macwire-hearth-evaluation-claims.md`](../macwire-hearth-evaluation-claims.md).

## Setup

1. Set `val hearth` in `project/Versions.scala` (currently `0.4.3`) to the snapshot. The Sonatype Central Snapshots
   resolver is already configured in `build.sbt`.
2. Run `sbt --client cleanFull` before the first build. The macros change, and `clean` alone restores stale outputs
   from sbt 2's cache.
3. Establish a baseline before editing anything: `sbt --client "test-jvm-2_13 ; test-jvm-3"`. On 0.4.3 the only failure
   is the 2.13 test listed under item 1.

## Items

Check first which items the Hearth release actually fixes. Items 1–7 have drafts and failing tests in Hearth
(`hearth-reproducer-tests.patch`). Items 8–11 were only documented in kindlings (`../hearth-gap-*.md`), so they may
still be open. Leave the workaround for any item that is still open.

| # | Hearth item | Kindlings site | Change | Proof |
|---|---|---|---|---|
| 1 | Generic method return type after `ApplyTypes` (`Nothing` on 2.13) | `di-cats/.../ResourceWiringMacrosImpl.scala` `returnsResourceOf`, `nullPlaceholder`, trial application in `companionResourceMethod` | read `knownReturning` after `ApplyTypes`; drop the placeholder trial application | `ResourceMethodSpec` "ignores a companion resource[F] that does not return a Resource of the type" turns green on 2.13 JVM + JS (the PR's only red CI) |
| 2 | Cross-quotes: `Type.of` before `import x.Underlying as T` (Scala 3) | `ResourceWiringMacrosImpl` `preciseDependency` / `asPreciselyTyped` split; the import order in `companionResourceMethod` | merge `asPreciselyTyped` back into `preciseDependency`; any import order | di-cats compiles on Scala 3, `ResourceMethodSpec` passes |
| 3 | `decompose1/2`, `typeArguments` on singleton types | no workaround in kindlings: dependencies are widened by `DestructuredExpr` (`preciseDependency`) before `isResource` | optional: drop the "widened static type" remark in the `isResource` scaladoc if it no longer matters | — |
| 4 | Enclosing-method parameters (`Environment.enclosingMethodParameters` prototype) | `di/.../WiringMacrosImpl.scala` scope collection; `docs/user-guide/di.md` ("not currently exposed cross-platform") | new feature: resolve `wire[A]` dependencies from the enclosing method's parameters on 2.13 and 3 | new `WiringSpec` tests (parameter used; parameter vs member ambiguity) |
| 5 | `isFunction` / `isContextFunction` predicate | `WiringMacrosImpl.isFunctionExpr`, `ResourceWiringMacrosImpl.isFunction` (`fqcn.startsWith("scala.Function")`) | use the predicate; accept context functions as factories (claim 6 in the evaluation) | new tests in `WiringSpec` / `ResourceMethodSpec` (Scala 3 dir: context functions are a Scala 3 feature) |
| 6 | `Type.fqcn` scaladoc | — | nothing | — |
| 7 | `Ctor2.of[Function1].unapply` on subtypes (Scala 3) | `fromUntyped[Function1]` in cats-derivation `InvariantMacrosImpl`, `ContravariantMacrosImpl` | plain `Type.Ctor2.of[Function1]`; drop the pitfall in `docs/contributing/hearth-cross-compilation/pitfalls.md` §11, the matching `SKILL.md` items, `hearth-value-types`/`hearth-collection-map` skills and the AGENTS.md line | cats-derivation `CatsDerivationSpec` (Contravariant, Invariant) |
| 8 | `VarArgs.toIterable` drops non-literal varargs | Scala 3 `Varargs(...)` matches: `optics/src/main/scala-3/.../ModifyMacros.scala`, `di/src/main/scala-3/.../WiringMacros.scala`, `di-cats/src/main/scala-3/.../ResourceWiringMacros.scala` | shared `VarArgs` API, if it now reports non-literal varargs instead of returning `Nil` | `optics/src/test/scala-3/.../ModifyAllVarargsSpec.scala` |
| 9 | Path-dependent type prefix / type members | `newtype-integration/src/main/scala-{2,3}/.../NewtypeReprPlatform.scala`; `IsValueTypeProviderForNeotype.scala` (owner lookup) | shared code | newtype and neotype integration tests |
| 10 | `Ctor*.of` on a Scala 2 macro's own type parameter | one-line Scala 2 entry points: `Type.CtorN.fromUntyped(ft.tpe.typeConstructor)` in `cats-derivation/src/main/scala-2` (19), `cats-tagless-derivation/src/main/scala-2`, `di-cats/src/main/scala-2` | `Type.Ctor1.of[F]` / `CtorK1.of[Alg]` | cats-derivation and cats-tagless suites on 2.13 |
| 11 | No `Ctor` for mixed kinds (`Resource[F[_], A]`) | `ResourceWiringMacrosImpl.isResource` / `resourceUnderlying` (FQCN + `decompose2`) | the new extractor | `ResourceWiringSpec`, `ResourceMethodSpec` |

## After the changes

- Clean the touched modules, then run `test-jvm-2_13 ; test-jvm-3`, and `test-js-2_13 ; test-js-3` for di-cats
  (2.13 JS is where CI showed the item 1 failure).
- Mark each fixed item RESOLVED (with the Hearth version) in its `../hearth-gap-*.md` note, in `../GAPS.md` and in the
  Hearth feedback table of `../macwire-hearth-evaluation-claims.md`.
