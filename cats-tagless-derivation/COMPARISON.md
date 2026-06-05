# cats-tagless-derivation: Feature Comparison with cats-tagless 0.16.5

Status: **Not yet ready for release.** Trait support (the primary cats-tagless use case) requires
anonymous instance generation, planned for Kindlings 0.4.0. Revisit this module after that lands.

## Supported algebra types

| Feature | cats-tagless 0.16.5 | Kindlings (current) | Notes |
|---|---|---|---|
| Traits (`trait Alg[F[_]]`) | Yes (primary target) | **No** | cats-tagless creates anonymous implementations via `Symbol.newClass` + `overridableMembers`. Kindlings needs anonymous trait instance support (planned for 0.4.0). |
| Abstract classes | Yes | **No** | Same mechanism as traits in cats-tagless. |
| Case classes (`case class Alg[F[_]](...)`) | **No** | Yes (primary target) | cats-tagless explicitly rejects: "Not supported: $T is not a trait or abstract class". |
| Sealed traits / enums | No | No | Neither supports this. |

## Supported field/member types

| Feature | cats-tagless | Kindlings | Notes |
|---|---|---|---|
| Direct `F[X]` return types | Yes | Yes | Both apply the natural transformation directly. |
| Invariant fields (no `F`) | Yes (passed through) | Yes (copied) | |
| Nested algebras (`OtherAlg[F]`) | Yes (summons type class) | Yes (recursive derivation + summoning with caching) | Kindlings recursively derives if no implicit exists; cats-tagless only summons. |
| `F` in method parameters (contravariant) | Yes | **No** | cats-tagless FunctorK summons `ContravariantK` for args; ContravariantK summons `FunctorK`. Kindlings errors on direct `F[X]` fields in ContravariantK. |
| Type members | Yes (aliases forwarded) | N/A | Case classes don't have overridable type members. |
| By-name, default, curried, varargs params | Yes | N/A | Trait method features not applicable to case class fields. |
| `F[F[X]]` nested type constructor | Yes (recursive `summonLambda`) | **Error** | Kindlings detects this via probe mismatch and reports an error. |

## Variance handling

**cats-tagless** checks `TypeRepr.contains(G)` on each member's type:
- FunctorK: return types → `FunctorK.mapK`; parameter types → `ContravariantK.contramapK`
- ContravariantK: return types → `ContravariantK.contramapK`; parameter types → `FunctorK.mapK`
- InvariantK: both positions → `InvariantK.imapK`

**Kindlings** classifies fields via two-probe decomposition (`Option`/`List`):
- Direct `F[X]`: FunctorK applies `fk`; ContravariantK errors with suggestion to use InvariantK
- Nested `OtherAlg[F]`: summons/derives the matching type class recursively
- No Function1 decomposition for method parameter variance (future work)

## Derivation infrastructure

| Aspect | cats-tagless | Kindlings |
|---|---|---|
| Trigger | `Derive.functorK[Alg]` or `FunctorK.derived` (`@experimental`) | `KindlingsFunctorK.derived[Alg]` |
| Summoning | `Implicits.search` (direct, via `summonLambda`) | `summonExprIgnoring` + self-type exclusion |
| Caching | None (single-pass tree transform) | `ValDefsCache` — lazy val for summoned, cached derivation for recursive |
| Error messages | `report.errorAndAbort` | Rule chain with aggregated reasons + suggestions |
| `@experimental` | Required (Scala 3 macros) | Not required |

## Additional type classes in cats-tagless not yet in Kindlings

- **SemigroupalK** — `productK[F, G](af: Alg[F], ag: Alg[G]): Alg[Tuple2K[F, G, *]]`
- **ApplyK** — extends FunctorK + SemigroupalK
- **Instrument / Aspect** — AOP-style type classes

## What Kindlings adds over cats-tagless

- Case class support (data-style algebras)
- Recursive derivation with caching for nested algebras without explicit instances
- Self-type exclusion preventing infinite recursion with auto-derivation implicits
- Rule chain (UseCached → UseImplicit → CaseClass) with per-rule failure diagnostics
- Informative error messages suggesting alternative type classes
- Confirmed JVM + Scala.js + Scala Native support
- No `@experimental` annotation required

## TODO before release

- [ ] Trait-based algebra support (requires anonymous trait instance generation — Kindlings 0.4.0)
- [ ] Method parameter variance (Function1 decomposition for `F` in contravariant position)
- [ ] `F[F[X]]` nested type constructor support
- [ ] SemigroupalK / ApplyK derivation
- [ ] Sealed trait support for algebras
