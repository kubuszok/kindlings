package hearth.kindlings.integrationtests

import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import hearth.MacroSuite
import hearth.kindlings.pureconfigderivation.{KindlingsConfigReader, KindlingsConfigWriter}
import pureconfig.*

final class RefinedPureconfigSpec extends MacroSuite {

  private val alice = refineV[NonEmpty]("Alice").toOption.get
  private val thirty = refineV[Positive](30).toOption.get

  group("Refined + PureConfig") {

    test("round-trip") {
      val reader: ConfigReader[RefinedPerson] = KindlingsConfigReader.derive[RefinedPerson]
      val writer: ConfigWriter[RefinedPerson] = KindlingsConfigWriter.derive[RefinedPerson]
      val v = RefinedPerson(alice, thirty)
      val config = writer.to(v)
      val result = ConfigSource.fromConfig(config.atKey("root")).at("root").load[RefinedPerson](reader)
      assert(result.isRight, s"Expected Right but got $result")
    }
  }
}
