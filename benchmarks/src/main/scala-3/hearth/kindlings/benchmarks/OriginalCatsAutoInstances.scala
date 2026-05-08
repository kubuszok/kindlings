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
  val simpleADTEq: cats.kernel.Eq[SimpleADT] = OriginalCatsSemiAutoInstances.simpleADTEq
}
