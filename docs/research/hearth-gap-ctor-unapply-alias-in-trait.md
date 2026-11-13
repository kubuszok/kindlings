# Hearth gap: `Type.CtorN.unapply` with an alias declared in a trait (Scala 2, regression)

Status: **OPEN** (found 2026-11-13 while bumping Kindlings to Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT`, which added the
prefix comparison of kubuszok/hearth#396; it worked with `0.4.3-24-gc0c6e43-SNAPSHOT`). Scala 2.13 only. Worked around
in Kindlings.

## Symptom

cats' `NonEmptyLazyList` stopped being a collection on Scala 2.13 (`CatsJsoniterSpec` "NonEmptyLazyList round-trip":
"The type cats.data.NonEmptyLazyList.Type[scala.Int] was not handled by any codec derivation rule"), because
`Type.Ctor1.of[cats.data.NonEmptyLazyList].unapply(Type.of[NonEmptyLazyList[Int]])` is `None`. `NonEmptyChain`, defined
the same way, still matches.

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

## Cause

The generated `unapply` (`CrossQuotesMacrosCtorMethods`) takes `A0.dealias.widen.baseType(HKT.typeSymbol)` -
`HKT.typeSymbol` is the DEALIASED symbol `NonEmptyLazyList.Type`, so the result is
`TypeRef(NonEmptyLazyList.type, Type, List(Int))` - and then checks `prefixMatches(pre)`, which reads the prefix of
the NON-dealiased `HKT.typeConstructor`, the alias `cats.data.NonEmptyLazyList`. Its owner is the trait
`ScalaVersionSpecificPackage` (a class, not a module or package), so the check runs and compares
`NonEmptyLazyList.type =:= cats.data.package.type`, which fails. For `NonEmptyChain` the alias is owned by the package
object (a module class), so the check is skipped.

Suggested fix: compare against the prefix of `HKT.dealias.typeConstructor` (the symbol whose base type is taken).

## Reproducer

```scala
package p
trait AliasesInTrait { type Foo[+A] = Foo.Type[A] }
package object p extends AliasesInTrait
object Foo { type Base; type Type[+A] <: Base }
// Scala 2: Type.Ctor1.of[p.Foo].unapply(Type.of[p.Foo[Int]]) == None (expected Some(Int))
```

## Workaround in Kindlings

`CatsCollectionAndMapProviders` builds the constructor from the newtype itself:
`Type.Ctor1.of[cats.data.NonEmptyLazyList.Type]`.
