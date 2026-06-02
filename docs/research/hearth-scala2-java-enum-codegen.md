# Hearth Scala 2: Java enum `Expr.quote` generates broken `rassoc` references

## Summary

Deriving any type class for a Java enum (e.g. `java.time.Month`) on Scala 2.13 fails at
compile time with:

```
error: not found: value rassoc$1
```

The same derivation works correctly on Scala 3.

## Reproduction

In `kindlings`, branch `re-audition-tests`, the file
`scalacheck-derivation/src/test/scalajvm-3/hearth/kindlings/scalacheckderivation/JavaEnumSpec.scala`
contains working Scala 3 tests. Moving any of those tests to the shared `scalajvm/`
directory (so Scala 2.13 also compiles them) triggers the error.

Minimal reproduction:

```scala
// file: src/test/scalajvm/Repro.scala
import org.scalacheck.Arbitrary
import hearth.kindlings.scalacheckderivation.DeriveArbitrary

val arb: Arbitrary[java.time.Month] = DeriveArbitrary.derived[java.time.Month]
// Scala 2.13: error: not found: value rassoc$1
// Scala 3:    compiles and runs correctly
```

## How the macro expands

Enable derivation logging:

```scala
import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation
```

The derivation pipeline is:

1. `Enum.parse[java.time.Month]` succeeds (Hearth sees the type as sealed/isJavaEnum
   with 12 children: `JANUARY.type` through `DECEMBER.type`)
2. For each child, `SingletonValue.parse` succeeds → `Gen.const(singleton)` is produced
3. The 12 `Gen` expressions are combined with `Gen.oneOf(first, second, rest*)`
4. The `rest` list is built by `foldRight` producing nested `Expr.quote(elem :: acc)`

## The generated tree (Scala 2.13)

The final tree printed by the macro (trimmed for readability):

```scala
org.scalacheck.Arbitrary.apply[java.time.Month](
  org.scalacheck.Gen.oneOf[java.time.Month](
    org.scalacheck.Gen.const[...](java.time.Month.JANUARY).asInstanceOf[Gen[java.time.Month]],
    org.scalacheck.Gen.const[...](java.time.Month.FEBRUARY).asInstanceOf[Gen[java.time.Month]],
    ({
      val rassoc: Gen[java.time.Month] = Gen.const[...](java.time.Month.APRIL).asInstanceOf[...];
      ({
        val rassoc: Gen[java.time.Month] = Gen.const[...](java.time.Month.MAY).asInstanceOf[...];
        ({
          val rassoc: Gen[java.time.Month] = Gen.const[...](java.time.Month.JUNE).asInstanceOf[...];
          ({
            val rassoc: Gen[java.time.Month] = Gen.const[...](java.time.Month.JULY).asInstanceOf[...];
            ({
              val rassoc: Gen[java.time.Month] = Gen.const[...](java.time.Month.AUGUST).asInstanceOf[...];
              ({
                val rassoc: Gen[java.time.Month] = Gen.const[...](java.time.Month.SEPTEMBER).asInstanceOf[...];
                ({
                  val rassoc: Gen[java.time.Month] = Gen.const[...](java.time.Month.OCTOBER).asInstanceOf[...];
                  ({
                    val rassoc: Gen[java.time.Month] = Gen.const[...](java.time.Month.NOVEMBER).asInstanceOf[...];
                    scala.List.apply[...](
                      Gen.const[...](java.time.Month.DECEMBER).asInstanceOf[...]
                    ).::[...](rassoc)
                  }).::[...](rassoc)
                }).::[...](rassoc)
              }).::[...](rassoc)
            }).::[...](rassoc)
          }).::[...](rassoc)
        }).::[...](rassoc)
      }).::[...](rassoc)
    }).::[...](rassoc)   // <── error: not found: value rassoc$1
  )
)
```

Each `{ val rassoc = ...; inner.::(rassoc) }` block is produced by one iteration of
`Expr.quote(Expr.splice(elem) :: Expr.splice(acc))`. Each nested block shadows the outer
`rassoc`, which is correct Scala semantics. But the Scala 2.13 compiler loses track of the
binding at some nesting depth and emits `not found: value rassoc$1`.

## Kindlings-side code that builds the list

The problematic `foldRight` is in `ArbitraryHandleAsEnumRule.scala`, line ~64:

```scala
val restGens: Expr[List[Gen[A]]] =
  reversedRest.tail.foldLeft(
    Expr.quote(List(Expr.splice(reversedRest.head)))
  ) { (accExpr, genExpr) =>
    Expr.quote(Expr.splice(genExpr) :: Expr.splice(accExpr))
  }
```

This is the standard `Expr.quote` list-building pattern used elsewhere in kindlings
(e.g. `ArbitraryHandleAsCaseClassRule` line ~90). It works with smaller lists (e.g. a
3-case sealed trait) but breaks at 12 elements on Scala 2.

## Hypothesis

The `::` method on `List` is synthetic/bridge on the Scala 2 compiler. Each
`Expr.quote(elem :: acc)` emits a `Block(ValDef(rassoc, elem), Apply(acc, rassoc))` tree.
On Scala 2, the compiler's name-mangling for these synthetic `rassoc` vals may collide when
nesting depth exceeds a threshold, or the tree's `Symbol` owner chain may not be set up
correctly for the deeply nested blocks emitted by the macro.

This does NOT happen on Scala 3 because `scala.quoted.Expr` generates different tree shapes.

## Workaround in kindlings

Java enum tests are placed in `scalajvm-3/` (Scala 3 + JVM only). The feature parity
documentation notes Java enum support as Scala 3 only.

## Potential fix directions

1. **In Hearth**: When emitting `::` on Scala 2, use unique names for the synthetic val
   (e.g. `rassoc$0`, `rassoc$1`, ... with proper Symbol creation) instead of re-using the
   same name `rassoc`.
2. **In Hearth**: Provide a `Expr.listOf[A](elems: List[Expr[A]]): Expr[List[A]]` helper
   that builds the list tree without nested `::` blocks (e.g. using `List.apply` with
   varargs).
3. **In kindlings**: Build the list differently, e.g. using `List.apply(elem1, elem2, ...)`
   instead of `foldRight` with `::`. But the current foldRight pattern is idiomatic and
   works for reasonable list sizes — the fix should be in Hearth.

## Environment

- Hearth: 0.2.0-SNAPSHOT (as of 2026-04-24)
- Scala 2.13.18
- Java enum: `java.time.Month` (12 constants)
- Works with ≤ ~3 cases (sealed traits with case objects work fine)
