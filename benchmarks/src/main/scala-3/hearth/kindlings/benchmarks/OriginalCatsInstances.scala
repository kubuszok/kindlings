package hearth.kindlings.benchmarks

import cats.derived.semiauto

object OriginalCatsSemiAutoInstances {

  given simpleCCShow: cats.Show[SimpleCC] = semiauto.show
  given simpleCCEq: cats.kernel.Eq[SimpleCC] = semiauto.eq
  given simpleCCOrder: cats.kernel.Order[SimpleCC] = semiauto.order
  given simpleCCHash: cats.kernel.Hash[SimpleCC] = semiauto.hash
  given intPairSemigroup: cats.kernel.Semigroup[IntPair] = semiauto.semigroup
  given intPairMonoid: cats.kernel.Monoid[IntPair] = semiauto.monoid
  given intPairEmpty: alleycats.Empty[IntPair] = semiauto.empty

  given addressShow: cats.Show[Address] = semiauto.show
  given personShow: cats.Show[Person] = semiauto.show

  given simpleADTShow: cats.Show[SimpleADT] = semiauto.show
  given simpleADTEq: cats.kernel.Eq[SimpleADT] = semiauto.eq

  given eventShow: cats.Show[Event] = semiauto.show

  given simpleCCShowPretty: cats.derived.ShowPretty[SimpleCC] = semiauto.showPretty
  given addressShowPretty: cats.derived.ShowPretty[Address] = semiauto.showPretty
  given personShowPretty: cats.derived.ShowPretty[Person] = semiauto.showPretty

  given simpleCCBoxFunctor: cats.Functor[SimpleCCBox] = semiauto.functor
  given simpleCCBoxFoldable: cats.Foldable[SimpleCCBox] = semiauto.foldable
  given simpleCCBoxTraverse: cats.Traverse[SimpleCCBox] = semiauto.traverse
}
