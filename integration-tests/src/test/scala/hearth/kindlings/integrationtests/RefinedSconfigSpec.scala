package hearth.kindlings.integrationtests

import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import hearth.MacroSuite
import hearth.kindlings.sconfigderivation.{ConfigReader, ConfigWriter}

final class RefinedSconfigSpec extends MacroSuite {

  private val alice = refineV[NonEmpty]("Alice").toOption.get
  private val thirty = refineV[Positive](30).toOption.get

  group("Refined + sconfig") {

    test("round-trip") {
      implicit val reader: ConfigReader[RefinedPerson] = ConfigReader.derived[RefinedPerson]
      implicit val writer: ConfigWriter[RefinedPerson] = ConfigWriter.derived[RefinedPerson]
      val v = RefinedPerson(alice, thirty)
      val config = writer.to(v)
      val result = reader.from(config)
      assert(result.isRight, s"Expected Right but got $result")
    }

    test("rejects invalid refined value") {
      implicit val reader: ConfigReader[RefinedPerson] = ConfigReader.derived[RefinedPerson]
      val config = org.ekrich.config.ConfigFactory.parseString("""name = "Alice"
age = -1""")
      val result = reader.from(config.root)
      assert(result.isLeft, s"Expected Left but got $result")
    }
  }
}
