package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.sconfigderivation.{ConfigReader, ConfigWriter}

final class NeotypeSconfigSpec extends MacroSuite {

  group("Neotype + sconfig") {

    test("round-trip") {
      val reader: ConfigReader[NeotypePerson] = ConfigReader.derived[NeotypePerson]
      val writer: ConfigWriter[NeotypePerson] = ConfigWriter.derived[NeotypePerson]
      val person = NeotypePerson(NeotypeEmail("alice@example.com"), NeotypeAge(30), NeotypeNickname("ally"))
      reader.from(writer.to(person)) ==> Right(person)
    }

    test("rejects invalid neotype value") {
      val reader: ConfigReader[NeotypePerson] = ConfigReader.derived[NeotypePerson]
      val config = org.ekrich.config.ConfigFactory.parseString("""email = "alice@example.com"
age = -1
nickname = "ally"""")
      val result = reader.from(config.root)
      assert(result.isLeft, s"Expected Left but got $result")
    }
  }
}
