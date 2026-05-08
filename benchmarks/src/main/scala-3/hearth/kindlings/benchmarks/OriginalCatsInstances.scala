package hearth.kindlings.benchmarks

import cats.derived.semiauto

object OriginalCatsSemiAutoInstances {

  given simpleCCShow: cats.Show[SimpleCC] = semiauto.show
  given simpleCCEq: cats.kernel.Eq[SimpleCC] = semiauto.eq
  given simpleCCOrder: cats.kernel.Order[SimpleCC] = semiauto.order
  given simpleCCHash: cats.kernel.Hash[SimpleCC] = semiauto.hash

  given addressShow: cats.Show[Address] = semiauto.show
  given personShow: cats.Show[Person] = semiauto.show

  given simpleADTShow: cats.Show[SimpleADT] = semiauto.show
  given simpleADTEq: cats.kernel.Eq[SimpleADT] = semiauto.eq

  given eventShow: cats.Show[Event] = semiauto.show
}
