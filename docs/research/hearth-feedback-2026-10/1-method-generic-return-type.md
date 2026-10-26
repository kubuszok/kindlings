# `Method` chain: return type of a generic method is unknown after `ApplyTypes` (and `Nothing` on Scala 2)

**Status:** RESOLVED by kubuszok/hearth#389 (Hearth 0.4.4); see `../hearth-gap-method-generic-return-type.md`.

**Verdict:** confirmed. It is a gap on both platforms and a bug on Scala 2.13. There is also a cosmetic Scala 3 side issue (1b).

**Platforms:** Scala 2.13 (gap + bug), Scala 3 (gap; cosmetic 1b). Tested with Hearth `0.4.3-4-gaded1a31`, Scala 2.13.16 / 3.3.8, JVM.

## Reproducer

`hearth-tests/src/main/scala/hearth/examples/kindlings_feedback.scala` (local stand-ins, no cats):

```scala
trait Res[F[_], A]; trait SyncLike[F[_]]; trait SomeF[A]
object SomeF { implicit val syncLike: SyncLike[SomeF] = new SyncLike[SomeF] {} }
final class Config

object Widget  { def resource[F[_]]: Res[F, Widget] = null }
object Service { def resource[F[_]: SyncLike](config: Config): Res[F, Service] = null }
object Plain   { def resource[F[_]]: Option[Plain] = None }   // must be rejectable by result type
```

The fixture `KindlingsFeedbackFixturesImpl.genericReturnWalk` walks `Type[X.type].methods.find(_.name == "resource")`
through `OnInstance(X)`, then `ApplyTypes(F := SomeF)`, then `ApplyValues(new Config / summoned implicit)`, then `Result`. At each step it records
`knownReturning`. A type is rendered as `=:= R` when it is `=:=` to the expected substituted return type `R`. Spec:
`hearth-tests/src/test/scala/hearth/typed/KindlingsFeedbackSpec.scala`, group `(1)`.

Expected (wanted, same on both platforms):

| step | Widget / Plain | Service |
|---|---|---|
| OnInstance | `<none>` | `<none>` |
| ApplyTypes | `<none>` | `<none>` |
| ApplyValues (after F applied) | - | **`=:= R`** |
| Result / `Returned` / `build()` type | **`=:= R`** | **`=:= R`** |

## Actual (copied from the test run)

Scala 2.13 (`hearthTests2/testOnly hearth.typed.KindlingsFeedback*`). All 4 tests in group (1) fail:

```
==> X ...(1) ... / Widget.resource[F[_]]: Res[F, Widget] - Result.Returned is the substituted return type
[2].knownReturning:
  - "=:= R"
  + "scala.Nothing"
[3].Result.Returned:
  - "=:= R"
  + "scala.Nothing"
[3].Returned <:< String:
  - false
  + true
[3].built:
  - "built: =:= R"
  + "built: scala.Nothing"

==> X ...(1) ... / Plain.resource[F[_]]: Option[Plain] - Result.Returned must not be Nothing (it would pass any <:< check)
  (same diff: Returned = scala.Nothing, Returned <:< String = true)

==> X ...(1) ... / Service.resource[F[_]: SyncLike](config: Config): Res[F, Service] - known as soon as F is applied
[2].knownReturning:
  - "=:= R"
  + "<none>"
[3].knownReturning:
  - "=:= R"
  + "scala.Nothing"
[4].Result.Returned:
  - "=:= R"
  + "scala.Nothing"
[4].Returned <:< String:
  - false
  + true
```

Scala 3 (`hearthTests/testOnly hearth.typed.KindlingsFeedback*`). Widget and Plain pass. The `Result` is correct:

```
==> X ...(1) ... / Service.resource[F[_]: SyncLike](config: Config): Res[F, Service] - known as soon as F is applied
[2].knownReturning:
  - "=:= R"
  + "<none>"
```

Two things are **not** a bug and pass on both platforms. First, `expectations` after `ApplyTypes` *are* substituted
(`NeedsValues(config: Config)(evidence$1: SyncLike[SomeF])`, which is hearth#331). Second, both platforms report the same
clause grouping: a single `NeedsValues` with 2 `ListMap`s, and the already-applied `NeedsInstance` and `NeedsTypes[F]`
are still listed. The kindlings note said that "on Scala 2 `expectations` lists one NeedsValues containing all clauses
and still includes already-applied steps". That is true, but Scala 3 does exactly the same, so it is not a platform
difference. `expectations` is the static shape of the whole chain, not the remaining steps. It may be worth a
sentence in the `expectations` scaladoc.

Consequence in kindlings (di-cats): `Plain.resource[F]: Option[Plain]` is accepted as a `Resource[F, Plain]` factory on
Scala 2, because the `Nothing` result passes the `<:< Resource[F, Plain]` check. The generated code then fails to compile.

## Root cause

`UntypedMethodsScala2.scala` ~L618 and `UntypedMethodsScala3.scala` ~L904:
`returnType = if (hasUnresolvedTypeParams) None else ...`. `Method.buildChain` (`Methods.scala` ~L753) never
re-resolves it after `ApplyTypes`. The terminal `Result` falls back to the type of the built tree:
`buildExpr(...).toOption.map(expr => UntypedExpr.as_??(expr).Underlying.as_??)`. On Scala 3 that `Term` is typed, so it
works. On Scala 2 the tree built by `unsafeApplyWithTypes` is not type-checked (`tpe == null`), so the fallback gives
`Nothing`, and `build()`'s `Expr[Returned]` claims `Nothing` too.

## Proposed fix

Mirror the hearth#331 mechanism (`resolveExpectationsForTypeArgs`) for the return type:

- Add a `resolveReturnTypeForTypeArgs: UntypedTypeArguments => Option[() => ??]` parameter to `Method.buildChain`, and
  thread the current `returnType` through `buildFromStep` as a parameter instead of closing over the original. In
  `ApplyTypes.next`, replace it with `resolveReturnTypeForTypeArgs(allTypeArgs)` once every method type parameter has an
  argument.
- Scala 2: the same substitution `resolveTypeParamClauseValues` already does:
  `resultType.substituteTypes(methodTypeParamSyms, toReprs)`, where `resultType` comes from
  `typeSignatureIn(instanceTpe).finalResultType`.
- Scala 3: `safeMemberType(instanceTpe, sym)` is a `PolyType`, so use `pt.appliedTo(args)` and take its `finalResultType`
  (or reuse `resolvedParamTypesByName`'s substitution).
- Never fall back to an untyped tree's type on Scala 2. If the return type really cannot be resolved, `Result` should
  carry `Option[??]`/`None`, or the doc should say "`Nothing` = unknown". The scaladoc already says "possibly `Nothing`
  when it cannot be determined statically", but here it *can* be determined. A `Nothing` that is silently `<:` everything
  is a trap.

### 1b (cosmetic, Scala 3): `Type.Ctor1.of[F].asUntyped` used as a type argument prints eta-expanded

When `F := Type.Ctor1.of[SomeF].asUntyped.as_??` is used (that is what kindlings does), Scala 3 substitutes an
eta-expanded lambda. The types are `=:=`-equal and implicit search works, but every printed type shows it:

```
==> X ...(1) ... / Service.resource with F passed as Type.Ctor1.of[SomeF].asUntyped (the kindlings way)
[2].expectations[2]:
  - "NeedsValues(config: ...Config)(evidence$1: ...SyncLike[...SomeF])"
  + "NeedsValues(config: ...Config)(evidence$1: ...SyncLike[[A >: scala.Nothing <: scala.Any] => ...SomeF[A]])"
```

On Scala 3, `Result.Returned` prints as `Res[[A >: Nothing <: Any] => SomeF[A], Widget]`. Error messages that users see
contain this. Suggested fix: eta-reduce `Ctor1.of[F].asUntyped` (return the `TypeRef` of `F`, as
`UntypedType.typeConstructor(Type.of[F[Any]])` does), or eta-reduce when printing.
