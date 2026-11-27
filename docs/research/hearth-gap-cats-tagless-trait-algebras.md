# Hearth gaps met by cats-tagless-derivation's trait algebras

Found 2026-11-27 with Hearth `0.4.5-18-g99ac2ae-SNAPSHOT` (Scala 2.13.18, 3.8.3) while bringing `cats-tagless-derivation`
to parity with cats-tagless 0.16.6 (`docs/user-guide/feature-parity.md`, section cats-tagless-derivation). The visible
tests are `cats-tagless-derivation/src/test/scala/.../KnownGapsSpec.scala` (each asserts today's compile error, so a fix
in Hearth makes it fail and forces the docs update) and the fixtures in `UpstreamAlgebras.scala`.

## 1. No cross-platform type lambda construction (worked around)

cats-tagless looks up `FunctorK[[F[_]] =>> EitherT[F, E, A]]` (and `ContravariantK` / `InvariantK` / `SemigroupalK`)
for every member type that nests the algebra's `F`. Hearth can apply a type constructor (`UntypedType.applyTypeArgs`)
but cannot build `[F[_]] =>> T[F]` from `T[X]`. Kindlings applies the algebra to a marker constructor
(`CatsTaglessFactories.KProbe`) and replaces it with a lambda parameter in a per-version helper
(`TypeLambdaOverProbe`: `TypeLambda` + `substituteTypes` on Scala 3, `polyType` + `Type.map` on Scala 2).

Wanted: `UntypedType.lambdaOver(tpe: UntypedType, param: UntypedType): UntypedType` (or a `Type.CtorK1.abstractOver`).

## 2. `Parameter.byNameUnderlying` is not substituted (partly worked around)

For `trait Alg[F[_]] { def whenM(cond: F[Boolean])(action: => F[Unit]): F[Unit] }` parsed as `Alg[Option]`:

- `Parameter.tpe` of `action` is `=> Option[Unit]` (substituted),
- `Parameter.byNameUnderlying` is `Alg#F[Unit]` (the declaration's `F`, not substituted).

The same happens with a polymorphic method's type parameters: for `def defer[A](x: => A): F[A]`, after
`Method.ApplyTypes` the by-name parameter's underlying type is still the declaration's `A`. `Method.fold`'s argument
check uses `byNameUnderlying`, so an argument of the override's `A` is rejected (Scala 3: "Found: A, Required: A²" once
cast to it; Scala 2 accepts the generated code).

Kindlings unwraps the by-name type from `Parameter.tpe` itself (`TypeLambdaOverProbe.byNameUnderlying`) to classify
the parameter, and casts the argument to what `fold` expects. Still failing: a by-name parameter whose type mentions the
method's own type parameters, on Scala 3 (`AlgWithConstantReturnTypes.defer`, cats-tagless' `autoSemigroupalKTests`).

## 3. Dependent method types in forwarded calls

`def dependent[A](f: DepFn[A])(out: f.Out): F[A => f.Out]` (cats-tagless' `autoApplyKTests.AlgWithDependentType`):
forwarding the override's `out` to the source method fails - Hearth's expected type for `out` is `f.Out` with the
*source* method's `f` (Scala 3: "Found: f.Out, Required: f².Out"; Scala 2: "type mismatch; found: out.type (with
underlying type f.Out) required: f.Out"). The expected parameter type should be resolved against the arguments passed
for the earlier clause.

## 4. Refined algebras (type members)

`AlgWithTypeMember[F] { type T = String }` (cats-tagless' `AlgWithTypeMember.Aux[F, String]`, `AlgWithTypeBound`,
`AlgWithAbstractTypeClass`): `KindlingsFunctorK.derived[[F[_]] =>> AlgWithTypeMember.Aux[F, String]]` fails on Scala 3
with "Expected `fun.tpe` to widen into a `MethodType`" (a compiler assertion while building a tree for the refined
type; with `-Xcheck-macros` the non-alias refinement reports "Malformed tree"), and on Scala 2 no rule applies
(`AnonymousInstance.parse` of the refined type). cats-tagless implements the refinement's type members in the
anonymous class (`delegateAbstractTypes` / `newTypeAlias`).

Wanted: `AnonymousInstance` of a refined type, implementing its type members (and the parent's abstract ones from the
source instance).

## 5. Scala 2: algebra type lambdas with extra type parameters

`KindlingsFunctorK.derived[AlgWithExtraTP[*[_], String]]` (or a `type L[F[_]] = AlgWithExtraTP[F, String]` alias)
fails on Scala 2 with "AlgWithExtraTP takes 2 type parameters, expected: 1": `Type.CtorK1.of[Alg]` reads the
constructor from the `WeakTypeTag[Alg[Option]]` (`AlgWithExtraTP[Option, String]`) and keeps only its type constructor,
and `Type.CtorK1.fromUntyped` of the lambda dealiases it the same way when applying it. On Scala 3 the type lambda
`[F[_]] =>> AlgWithExtraTP[F, String]` works; a type alias for it fails like item 4.
