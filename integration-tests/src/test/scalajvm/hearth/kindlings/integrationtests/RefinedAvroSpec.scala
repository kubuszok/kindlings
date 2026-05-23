package hearth.kindlings.integrationtests

import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import hearth.MacroSuite
import hearth.kindlings.avroderivation.{AvroDecoder, AvroEncoder}

final class RefinedAvroSpec extends MacroSuite {

  private val alice = refineV[NonEmpty]("Alice").toOption.get
  private val thirty = refineV[Positive](30).toOption.get

  group("Refined + Avro") {

    test("round-trip") {
      val encoder = AvroEncoder.derive[RefinedPerson]
      val decoder = AvroDecoder.derive[RefinedPerson]
      val v = RefinedPerson(alice, thirty)
      val decoded = decoder.decode(encoder.encode(v))
      decoded.name ==> alice
      decoded.age ==> thirty
    }
  }
}
