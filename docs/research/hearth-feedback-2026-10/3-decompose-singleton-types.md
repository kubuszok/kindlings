# `Type.decompose1/2`, `typeArguments` (and on Scala 3 `typeConstructor`/`hasSameTypeConstructor`) don't see through singleton types

**Verdict:** confirmed bug on both platforms. The extent differs per platform (table below). `Type.CtorN.of[...].unapply`
is **not** affected; it already handles singletons on both platforms.

**Platforms:** Scala 2.13.16 and Scala 3.3.8 (JVM). Tested with Hearth `0.4.3-4-gaded1a31`.

(This corrects the earlier draft. Its "probably" and "not confirmed" points are now measured.)

## Reproducer

`hearth-tests/src/main/scala/hearth/examples/kindlings_feedback.scala`:

```scala
final class Box[+A, +B](val a: A, val b: B)
object SingletonVals {
  val box: Box[Int, String] = new Box(1, "a")
  val opt: Option[Int] = Some(1)
  val str: String = "a"
}
```

The fixture is `KindlingsFeedbackFixturesImpl.renderDecomposition[A, B]`. For each probe it records `Type.decompose1/2[A]`,
`UntypedType.typeConstructor/typeArguments`, `Type.typeArguments[A]`, `Type.hasSameTypeConstructor[A, B]`,
`Type.Ctor2.of[Box].unapply(Type[A])`, `Type.Ctor1.of[Option].unapply(Type[A])` and `A <:< B`. It is called as:

- `testDecompose[SingletonVals.box.type, Box[Any, Any]]`, an explicit singleton type argument,
- `testDecomposeExprType[Box[Any, Any]](SingletonVals.box)`, which uses `Expr.typeOf` of the argument (the kindlings
  case),
- baselines on the declared types `Box[Int, String]` / `Option[Int]`, which pass on both platforms.

Spec: `KindlingsFeedbackSpec`, group `(3)`.

## Results (measured)

| probe on `SingletonVals.box.type` | expected | Scala 2.13 | Scala 3 |
|---|---|---|---|
| `Type.decompose2` | `Some((Box, Int, String))` | **`None`** | **`None`** |
| `UntypedType.typeArguments` / `Type.typeArguments` | `[Int, String]` | **`[]`** | **`[]`** |
| `UntypedType.typeConstructor` | `Box` | `Box` (ok) | **`SingletonVals.box.type`** |
| `Type.hasSameTypeConstructor[_, Box[Any, Any]]` | `true` | `true` (ok) | `true` (ok)* |
| `Type.Ctor2.of[Box].unapply` | `Some((Int, String))` | ok | ok |
| `Type.Ctor1.of[Option].unapply` on `opt.type` | `Some(Int)` | ok | ok |
| `Type.decompose1` on `opt.type` | `Some((Option, Int))` | **`None`** | **`None`** |
| `Expr.typeOf(SingletonVals.box)` is a singleton? | - | no (already widened, so decompose works) | yes, so decompose fails |
| `UntypedType.typeConstructor` on `str.type` | `String` | ok | **`str.type`** |
| `hasSameTypeConstructor[str.type, String]` | `true` | ok | **`false`** |

\* For `box.type` vs `Box[Any, Any]`, Scala 3's `hasSameTypeConstructor` happens to be `true`. For `str.type` vs
`String` it is `false`, so `sameTypeConstructorAs` is also inconsistent on singletons.

Scala 2.13 test output:

```
==> X ...(3) ... / singleton SingletonVals.box.type (explicit type argument)
Type.typeArguments:
  - "[scala.Int, java.lang.String]"
  + "[]"
UntypedType.typeArguments:
  - "[scala.Int, java.lang.String]"
  + "[]"
decompose2:
  - "Some((hearth.examples.kindlingsfeedback.Box, scala.Int, java.lang.String))"
  + "None"

==> X ...(3) ... / singleton SingletonVals.opt.type (explicit type argument)
Type.typeArguments:
  - "[scala.Int]"
  + "[]"
UntypedType.typeArguments:
  - "[scala.Int]"
  + "[]"
decompose1:
  - "Some((scala.Option, scala.Int))"
  + "None"
```

Scala 3 test output:

```
==> X ...(3) ... / singleton SingletonVals.box.type (explicit type argument)
Type.typeArguments:
  - "[scala.Int, java.lang.String]"
  + "[]"
UntypedType.typeArguments:
  - "[scala.Int, java.lang.String]"
  + "[]"
UntypedType.typeConstructor:
  - "hearth.examples.kindlingsfeedback.Box"
  + "hearth.examples.kindlingsfeedback.SingletonVals.box.type"
decompose2:
  - "Some((hearth.examples.kindlingsfeedback.Box, scala.Int, java.lang.String))"
  + "None"

==> X ...(3) ... / static type of the argument expression SingletonVals.box
  (identical diff - this is the kindlings di-cats case: `wireResource[F, T](dbResource)`)

==> X ...(3) ... / singleton SingletonVals.opt.type (explicit type argument)
  (typeArguments [] / typeConstructor opt.type / decompose1 None)

==> X ...(3) ... / singleton of a non-applied type (SingletonVals.str.type) still decomposes to None
UntypedType.typeConstructor:
  - "java.lang.String"
  + "hearth.examples.kindlingsfeedback.SingletonVals.str.type"
hasSameTypeConstructor[A, B]:
  - true
  + false
```

On both platforms `decompose1/2` already returns `None` for `str.type`, as it should.

## Root cause

`Types.scala` `decompose1/2` do `UntypedType.dealias` and then `UntypedType.typeArguments`, and neither widens.

- Scala 2 (`UntypedTypesScala2.scala` ~L528): `SingleType.typeArgs` is `Nil`. `SingleType.typeConstructor` evidently
  delegates to the underlying type, which is why `typeConstructor` is fine but `typeArguments` is not.
- Scala 3 (`UntypedTypesScala3.scala` ~L1029): `TermRef.typeArgs` is `Nil`, and `typeConstructor` returns non-`AppliedType`
  types unchanged.

## Proposed fix

Widen singleton types (`TermRef`/`SingleType`; decide whether `ConstantType` is included) before dealiasing in
`typeConstructor`, `typeArguments`, `sameTypeConstructorAs`, and the dealias step of `decompose1/2`. Do not change
`=:=`/`<:<`. Consider a public `Type.widen[A]: ??` / `UntypedType.widen`. kindlings had to hand-roll a Scala 3-only
`term.tpe.widen` in a bridge. `Expr.typeOf` is also asymmetric: it is already widened on Scala 2 for a stable
reference, but a singleton on Scala 3. That is probably worth documenting.

(The uncommitted work in the main Hearth checkout, in `Types.scala`/`UntypedTypes*.scala` "Singleton types are widened
first...", appears to implement exactly this. These tests can serve as its acceptance tests.)
