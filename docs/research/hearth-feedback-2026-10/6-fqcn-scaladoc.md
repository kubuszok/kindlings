# `Type.fqcn` has no scaladoc and should warn that it is not a type identity

**Verdict:** not a bug, but a documentation gap. `fqcn` behaves as designed. It has no scaladoc at all, and users
misuse it as a type key.

**Platforms:** both.

## Facts

`hearth/src/main/scala/hearth/typed/Types.scala:67`:

```scala
final def fqcn[A: Type]: String = plainPrint[A].takeWhile(_ != '[')
```

It has no scaladoc. The only doc is one table row in `docs/user-guide/basic-utilities.md:417`: "fully-qualified class
name, no type parameters".

Test `KindlingsFeedbackSpec` group `(6)` passes on 2.13 and 3:

```
testFqcnIdentity[Box[Int, String], Box[String, Int]] <==>
  fqcn[A] = "hearth.examples.kindlingsfeedback.Box", fqcn[B] = same, fqcn[A] == fqcn[B] = true, A =:= B = false
```

kindlings `di` used `Type.fqcn` as the key of its wiring graph, so `Box[A]` and `Box[B]` collided. It also used
`fqcn.startsWith("scala.Function")` as a function-type test (see item 5).

Other things worth stating in the doc. They follow from `plainPrint`, but only the first was observed in a test here:

- a singleton type gives `pkg.Obj.x.type` (it does not widen; observed, item 3);
- a type lambda or eta-expanded constructor would give `""`, because `takeWhile(_ != '[')` stops at the first
  character. Scala 3 `Ctor1.of[F].asUntyped` prints `[A >: ...] => F[A]`, see item 1b. This was not tested directly;
- a refinement or intersection is not a class name.

## Suggested scaladoc

```scala
/** The fully-qualified name of `A`'s type constructor, as printed by [[plainPrint]] up to the first `[`
  * (e.g. `"scala.collection.immutable.List"` for `List[Int]`).
  *
  * Intended for messages, logging and coarse name-based heuristics. It is NOT a type identity: types that differ only
  * in their type arguments (`Box[Int]` vs `Box[String]`) share an `fqcn`, and it is not normalized (aliases, singleton
  * types like `x.type`, refinements and type lambdas print as written). To compare or key by
  * type, use `=:=` / `Type.Cache`, or `Type.hasSameTypeConstructor` / `Type.CtorN` to compare constructors.
  *
  * @since 0.1.0
  */
```
