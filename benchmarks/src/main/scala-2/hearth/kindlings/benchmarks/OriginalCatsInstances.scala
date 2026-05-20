package hearth.kindlings.benchmarks

import cats.derived.semiauto

object OriginalCatsSemiAutoInstances {

  implicit val simpleCCShow: cats.Show[SimpleCC] = semiauto.show
  implicit val simpleCCEq: cats.kernel.Eq[SimpleCC] = semiauto.eq
  implicit val simpleCCOrder: cats.kernel.Order[SimpleCC] = semiauto.order
  implicit val simpleCCHash: cats.kernel.Hash[SimpleCC] = semiauto.hash
  implicit val intPairSemigroup: cats.kernel.Semigroup[IntPair] = semiauto.semigroup
  implicit val intPairMonoid: cats.kernel.Monoid[IntPair] = semiauto.monoid
  implicit val intPairEmpty: alleycats.Empty[IntPair] = semiauto.empty

  implicit val addressShow: cats.Show[Address] = semiauto.show
  implicit val personShow: cats.Show[Person] = semiauto.show

  implicit val simpleADTShow: cats.Show[SimpleADT] = semiauto.show
  implicit val simpleADTEq: cats.kernel.Eq[SimpleADT] = semiauto.eq

  implicit val eventShow: cats.Show[Event] = semiauto.show

  implicit val simpleCCBoxFunctor: cats.Functor[SimpleCCBox] = semiauto.functor
}
