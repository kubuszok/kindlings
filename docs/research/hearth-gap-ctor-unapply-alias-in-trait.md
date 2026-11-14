# Hearth gap: `Type.CtorN.unapply` with an alias declared in a trait (Scala 2, regression)

Status: **RESOLVED** in Hearth `0.4.3-90-gc3f9223-SNAPSHOT` (kubuszok/hearth#402, to be released as 0.4.4). Kindlings'
workaround is removed. Scala 2.13 only.

Found 2026-11-13 while bumping Kindlings to Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT`, which added the prefix comparison of
kubuszok/hearth#396. It worked with `0.4.3-24-gc0c6e43-SNAPSHOT`.

## Symptom

cats' `NonEmptyLazyList` stopped being a collection on Scala 2.13 (`CatsJsoniterSpec` "NonEmptyLazyList round-trip":
"The type cats.data.NonEmptyLazyList.Type[scala.Int] was not handled by any codec derivation rule"), because
`Type.Ctor1.of[cats.data.NonEmptyLazyList].unapply` returned `None`. `NonEmptyChain`, defined the same way, still
matched.

cats defines (2.13.0):

```scala
package cats.data
trait ScalaVersionSpecificPackage {           // the `cats.data` package object extends it
  type NonEmptyLazyList[+A] = NonEmptyLazyList.Type[A]
}
object NonEmptyLazyList extends NonEmptyLazyListInstances {
  private[data] type Base
  private[data] trait Tag extends Any
  type Type[+A] <: Base & Tag
}
// package.scala: type NonEmptyChain[+A] = NonEmptyChainImpl.Type[A]   (in the package object itself)
```

Only the DEALIASED type failed. `unapply(Type.of[NonEmptyLazyList[Int]])`, with the alias as written, still matched
through the `A0.typeConstructor == HKT` fallback. Kindlings hands the dealiased type to the provider
(`NonEmptyLazyList.Type[Int]`), so it failed there.

## Cause

The generated `unapply` (`CrossQuotesMacrosCtorMethods`) takes `A0.dealias.widen.baseType(HKT.typeSymbol)`.
`HKT.typeSymbol` is the DEALIASED symbol `NonEmptyLazyList.Type`, so the result is
`TypeRef(NonEmptyLazyList.type, Type, List(Int))`. It then checked `prefixMatches(pre)` against the prefix of the
NON-dealiased `HKT.typeConstructor`, the alias `cats.data.NonEmptyLazyList`. The alias is owned by the trait
`ScalaVersionSpecificPackage` (a class, not a module or package), so the check ran and compared
`NonEmptyLazyList.type =:= cats.data.package.type`, which failed. For `NonEmptyChain` the alias is owned by the package
object (a module class), so the check was skipped.

Hearth reproducer: `CrossQuotesEdgeCasesSpec` "for Type.CtorN.of / should unapply an alias that a package object
inherits from a trait" (types `hearth.examples.newtypealiases`).

## Former workaround in Kindlings (removed)

`CatsCollectionAndMapProviders` built the constructor from the newtype itself,
`Type.Ctor1.of[cats.data.NonEmptyLazyList.Type]`. With 0.4.3-90 it uses `Type.Ctor1.of[cats.data.NonEmptyLazyList]`
again, and `CatsJsoniterSpec` "NonEmptyLazyList" passes on 2.13 and 3.
