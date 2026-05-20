package hearth.kindlings.benchmarks

import hearth.kindlings.catsderivation.extensions.*

object KindlingsCatsInstances {

  val simpleCCShow: cats.Show[SimpleCC] = cats.Show.derived[SimpleCC]
  val simpleCCEq: cats.kernel.Eq[SimpleCC] = cats.kernel.Eq.derived[SimpleCC]
  val simpleCCOrder: cats.kernel.Order[SimpleCC] = cats.kernel.Order.derived[SimpleCC]
  val simpleCCHash: cats.kernel.Hash[SimpleCC] = cats.kernel.Hash.derived[SimpleCC]
  val intPairSemigroup: cats.kernel.Semigroup[IntPair] = cats.kernel.Semigroup.derived[IntPair]
  val intPairMonoid: cats.kernel.Monoid[IntPair] = cats.kernel.Monoid.derived[IntPair]
  val intPairEmpty: alleycats.Empty[IntPair] = alleycats.Empty.derived[IntPair]
  val personShow: cats.Show[Person] = cats.Show.derived[Person]

  val simpleADTShow: cats.Show[SimpleADT] = cats.Show.derived[SimpleADT]
  val simpleADTEq: cats.kernel.Eq[SimpleADT] = cats.kernel.Eq.derived[SimpleADT]

  val eventShow: cats.Show[Event] = cats.Show.derived[Event]

  val simpleCCShowPretty: hearth.kindlings.catsderivation.ShowPretty[SimpleCC] =
    hearth.kindlings.catsderivation.ShowPretty.derived[SimpleCC]
  val personShowPretty: hearth.kindlings.catsderivation.ShowPretty[Person] =
    hearth.kindlings.catsderivation.ShowPretty.derived[Person]

  val simpleCCBoxFunctor: cats.Functor[SimpleCCBox] = cats.Functor.derived[SimpleCCBox]
  val simpleCCBoxFoldable: cats.Foldable[SimpleCCBox] = cats.Foldable.derived[SimpleCCBox]
  val simpleCCBoxTraverse: cats.Traverse[SimpleCCBox] = cats.Traverse.derived[SimpleCCBox]
}
