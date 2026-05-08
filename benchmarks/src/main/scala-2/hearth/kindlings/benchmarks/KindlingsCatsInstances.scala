package hearth.kindlings.benchmarks

import hearth.kindlings.catsderivation.extensions._

object KindlingsCatsInstances {

  val simpleCCShow: cats.Show[SimpleCC] = cats.Show.derived[SimpleCC]
  val simpleCCEq: cats.kernel.Eq[SimpleCC] = cats.kernel.Eq.derived[SimpleCC]
  val simpleCCOrder: cats.kernel.Order[SimpleCC] = cats.kernel.Order.derived[SimpleCC]
  val simpleCCHash: cats.kernel.Hash[SimpleCC] = cats.kernel.Hash.derived[SimpleCC]
  val personShow: cats.Show[Person] = cats.Show.derived[Person]

  val simpleADTShow: cats.Show[SimpleADT] = cats.Show.derived[SimpleADT]
  val simpleADTEq: cats.kernel.Eq[SimpleADT] = cats.kernel.Eq.derived[SimpleADT]

  val eventShow: cats.Show[Event] = cats.Show.derived[Event]
}
