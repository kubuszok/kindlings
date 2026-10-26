# Cross-quotes (Scala 3): `Type.of` before a later `import x.Underlying as D` in the same block fails with "Not found: type D"

**Status:** RESOLVED by kubuszok/hearth#389 (Hearth 0.4.4); see `../hearth-gap-cross-quotes-import-order.md`.

**Verdict:** confirmed bug, in the Scala 3 cross-quotes compiler plugin. A one-line-scope fix has been verified: it makes
the reproducer pass and causes no regression in the 1386 Scala 3 hearth-tests.

**Platforms:** Scala 3 only (tested 3.3.8). Scala 2.13 compiles and passes.

## Reproducer

`hearth-tests/src/main/scala/hearth/crossquotes/ImportOrderReproFixturesImpl.scala`:

```scala
trait ImportOrderReproFixturesImpl { this: MacroCommons =>

  def importBeforeTypeOf(expr: Expr[Any]): Expr[String] = {   // workaround: compiles on both
    val tpe: ?? = expr.tpe.as_??
    import tpe.Underlying as D
    implicit val anyType: Type[Any] = Type.of[Any]
    Expr(s"${Type[D].plainPrint} / ${Type[Any].plainPrint}")
  }

  def importAfterTypeOf(expr: Expr[Any]): Expr[String] = {    // reproducer
    implicit val anyType: Type[Any] = Type.of[Any]
    val tpe: ?? = expr.tpe.as_??
    import tpe.Underlying as D
    Expr(s"${Type[D].plainPrint} / ${Type[Any].plainPrint}")
  }
}
```

The bridges are `hearth-tests/src/main/scala-{2,3}/hearth/crossquotes/ImportOrderReproFixtures.scala`, and the spec is
`hearth-tests/src/test/scala/hearth/crossquotes/ImportOrderReproSpec.scala`:
`importAfterTypeOf(List(1)) ==> "scala.collection.immutable.List[scala.Int] / scala.Any"`.

## Actual

Scala 2.13 (`hearthTests2/testOnly hearth.crossquotes.ImportOrderReproSpec`):

```
Test run hearth.crossquotes.ImportOrderReproSpec finished: 0 failed, 0 ignored, 2 total
```

Scala 3 (`hearthTests/Test/compile`):

```
[error] -- [E006] Not Found Error: .../hearth-tests/src/main/scala/hearth/crossquotes/ImportOrderReproFixturesImpl.scala:20:46
[error] 20 |    implicit val anyType: Type[Any] = Type.of[Any]
[error]    |                                              ^
[error]    |                                              Not found: type D
```

## Expected

Both methods compile on both platforms and return `"scala.collection.immutable.List[scala.Int] / scala.Any"`.

## Root cause

`hearth-cross-quotes/src/main/scala-3/hearth/cq/CrossQuotesPlugin.scala`, `case block: Block =>` (~L970):

```scala
// Imports and (implicit) defs can legally be forward-referenced from earlier statements in a block,
// so their candidates are visible to every expansion in the block.
givenCandidates = oldGivenCandidates ++ blockGivenCandidates(
  block.stats.filterNot(_.isInstanceOf[untpd.ValDef])
)
```

The premise is wrong for imports. An imported name is in scope only *after* the import statement. The
`given casted$D: scala.quoted.Type[D] = ...` candidate built from `import tpe.Underlying as D` is therefore injected into
the expansion of the *earlier* `Type.of[Any]`, where `D` does not exist yet. hearth#285 already handles the same problem
for implicit `val`s, by adding their candidates only after their defining statement. Imports need the same treatment.

## Fix (verified)

`kindlings-feedback/2-cross-quotes-import-order.fix.patch`:

```scala
givenCandidates = oldGivenCandidates ++ blockGivenCandidates(
  block.stats.filterNot(stat => stat.isInstanceOf[untpd.ValDef] || stat.isInstanceOf[untpd.Import])
)
val newStats = block.stats.map { stat =>
  val transformed = transform(stat)
  stat match {
    case vd: untpd.ValDef  => givenCandidates = givenCandidates ++ blockGivenCandidates(vd :: Nil)
    case im: untpd.Import  => givenCandidates = givenCandidates ++ blockGivenCandidates(im :: Nil)
    case _                 => ()
  }
  transformed
}
```

With the patch applied (`hearthCrossQuotes/clean ; hearthTests/clean ; hearthTests/test`):

```
Test run hearth.crossquotes.ImportOrderReproSpec finished: 0 failed, 0 ignored, 2 total
[error] Failed: Total 1386, Failed 7, Errors 0, Passed 1379
[error] Failed tests:
[error]     hearth.typed.KindlingsFeedbackSpec      <- only the other kindlings-feedback reproducers
```

The patch was then reverted in this worktree, so the reproducer stays red.

The `case tpl: Template =>` branch has the same whole-body visibility for imports. Class-body imports are also scoped from
the import onwards, but it is harmless there more often, because members can see each other. It is worth applying the
same ordering there, or at least checking it.

NOTE for anyone running the other reproducers on Scala 3: this file breaks `hearthTests` compilation on purpose. Move
the three `ImportOrderRepro*` files (Impl, the scala-3 and scala-2 bridges, and the spec) away to run the rest.
