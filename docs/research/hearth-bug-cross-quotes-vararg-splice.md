# Hearth bug: Scala 2 cross-quotes keep only the head of a vararg splice that is not a `Seq(...)` call

Found by kindlings' scalacheck-derivation (parity work against scalacheck-shapeless / scalacheck-derived).

## Symptom

On Scala 2.13, `Arbitrary` derived for a sealed trait, Scala `Enumeration` or enum with more than three subtypes
generated only the first three. The macro built

```scala
val restGens: Expr[List[Gen[A]]] =
  rest.foldRight(Expr.quote(List.empty[Gen[A]])) { (genExpr, accExpr) =>
    Expr.quote(Expr.splice(genExpr) :: Expr.splice(accExpr))
  }
Expr.quote(Gen.oneOf(Expr.splice(first), Expr.splice(second), Expr.splice(restGens)*))
```

and for a 7-value `Enumeration` the expansion was (logged with `logDerivationForScalaCheckDerivation`):

```scala
Gen.oneOf[UpWeekDay.Value](genMon, genTue, genWed)   // genThu ... genSun are gone
```

Scala 3 is not affected.

## Cause

`hearth-cross-quotes/src/main/scala-2/hearth/cq/CrossQuotesMacros.scala` rewrites `Expr.splice[Seq[A]](a): _*` to
`..${a.tree.children.tail}`, assuming the spliced tree is an application `Seq(x1, x2, ...)` whose `children.tail` are
the elements. For any other tree of type `Seq[A]` that is wrong: the `::` chain above is
`Apply(Select(<List of the others>, ::), List(genWed))`, whose `children.tail` is `List(genWed)` - only the head
survives, silently. A spliced `val`/method call of type `Seq[A]` would lose everything (its `children.tail` is empty or
unrelated).

## Reproducer

```scala
// in a Scala 2 macro bundle using Hearth cross-quotes
def demo: Expr[List[Int]] = {
  val xs: Expr[List[Int]] = Expr.quote(1 :: 2 :: 3 :: Nil)
  Expr.quote(List(Expr.splice(xs)*))   // Scala 3: List(1, 2, 3); Scala 2: List(1)
}
```

The expected fix in Hearth: apply the `children.tail` shortcut only when the spliced tree is an `Apply` of a known
`Seq`/`List`/`Vector` factory `apply` (or a `VarArgs` value), and otherwise emit `(${a}): _*`.

## Workaround in Kindlings

`ArbitraryHandleAsEnumRule.oneOf` passes the list to a runtime helper
(`ScalaCheckUtils.oneOfGens(first, second, rest: List[Gen[A]])`), which calls `Gen.oneOf(first, second, rest*)` at
runtime: no vararg splice in the quote. Tested by `UpstreamParitySpec` ("Enumeration values are picked like
Gen.oneOf(values)", 7 values) in the shared test directory, which failed on Scala 2 before the workaround.
