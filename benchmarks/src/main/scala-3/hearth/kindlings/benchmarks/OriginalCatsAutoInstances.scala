package hearth.kindlings.benchmarks

import cats.derived.auto.show.given

object OriginalCatsAutoInstances {
  val simpleCCShow: cats.Show[SimpleCC] = summon
  val personShow: cats.Show[Person] = summon
  val simpleADTShow: cats.Show[SimpleADT] = summon
  val eventShow: cats.Show[Event] = summon

  val simpleCCEq: cats.kernel.Eq[SimpleCC] = OriginalCatsSemiAutoInstances.simpleCCEq
  val simpleCCOrder: cats.kernel.Order[SimpleCC] = OriginalCatsSemiAutoInstances.simpleCCOrder
  val simpleCCHash: cats.kernel.Hash[SimpleCC] = OriginalCatsSemiAutoInstances.simpleCCHash
  val intPairSemigroup: cats.kernel.Semigroup[IntPair] = OriginalCatsSemiAutoInstances.intPairSemigroup
  val intPairMonoid: cats.kernel.Monoid[IntPair] = OriginalCatsSemiAutoInstances.intPairMonoid
  val intPairEmpty: alleycats.Empty[IntPair] = OriginalCatsSemiAutoInstances.intPairEmpty
  val simpleADTEq: cats.kernel.Eq[SimpleADT] = OriginalCatsSemiAutoInstances.simpleADTEq
  val simpleCCShowPretty: cats.derived.ShowPretty[SimpleCC] = OriginalCatsSemiAutoInstances.simpleCCShowPretty
  val personShowPretty: cats.derived.ShowPretty[Person] = OriginalCatsSemiAutoInstances.personShowPretty
  val simpleCCBoxFunctor: cats.Functor[SimpleCCBox] = OriginalCatsSemiAutoInstances.simpleCCBoxFunctor
  val simpleCCBoxFoldable: cats.Foldable[SimpleCCBox] = OriginalCatsSemiAutoInstances.simpleCCBoxFoldable
  val simpleCCBoxTraverse: cats.Traverse[SimpleCCBox] = OriginalCatsSemiAutoInstances.simpleCCBoxTraverse
}
