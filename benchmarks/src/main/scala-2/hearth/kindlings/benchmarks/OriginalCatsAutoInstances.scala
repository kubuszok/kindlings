package hearth.kindlings.benchmarks

import cats.derived.auto.show.*

object OriginalCatsAutoInstances {
  val simpleCCShow: cats.Show[SimpleCC] = implicitly
  val personShow: cats.Show[Person] = implicitly
  val simpleADTShow: cats.Show[SimpleADT] = implicitly
  val eventShow: cats.Show[Event] = implicitly

  val simpleCCEq: cats.kernel.Eq[SimpleCC] = OriginalCatsSemiAutoInstances.simpleCCEq
  val simpleCCOrder: cats.kernel.Order[SimpleCC] = OriginalCatsSemiAutoInstances.simpleCCOrder
  val simpleCCHash: cats.kernel.Hash[SimpleCC] = OriginalCatsSemiAutoInstances.simpleCCHash
  val simpleADTEq: cats.kernel.Eq[SimpleADT] = OriginalCatsSemiAutoInstances.simpleADTEq
}
