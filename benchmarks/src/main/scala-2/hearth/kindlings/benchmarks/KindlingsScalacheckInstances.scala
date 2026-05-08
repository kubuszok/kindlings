package hearth.kindlings.benchmarks

import hearth.kindlings.scalacheckderivation.extensions._
import org.scalacheck.{Arbitrary, Cogen, Shrink}

object KindlingsScalacheckInstances {

  val simpleCCArbitrary: Arbitrary[SimpleCC] = Arbitrary.derived[SimpleCC]
  val simpleCCCogen: Cogen[SimpleCC] = Cogen.derived[SimpleCC]
  val simpleCCShrink: Shrink[SimpleCC] = Shrink.derived[SimpleCC]

  val personArbitrary: Arbitrary[Person] = Arbitrary.derived[Person]
  val personCogen: Cogen[Person] = Cogen.derived[Person]
  val personShrink: Shrink[Person] = Shrink.derived[Person]

  val simpleADTArbitrary: Arbitrary[SimpleADT] = Arbitrary.derived[SimpleADT]
  val simpleADTCogen: Cogen[SimpleADT] = Cogen.derived[SimpleADT]
  val simpleADTShrink: Shrink[SimpleADT] = Shrink.derived[SimpleADT]
}
