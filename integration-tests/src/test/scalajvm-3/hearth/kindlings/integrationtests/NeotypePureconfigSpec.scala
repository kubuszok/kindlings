package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.pureconfigderivation.{KindlingsConfigReader, KindlingsConfigWriter}
import pureconfig.*

final class NeotypePureconfigSpec extends MacroSuite {

  group("Neotype + PureConfig") {

    test("round-trip") {
      val reader: ConfigReader[NeotypePerson] = KindlingsConfigReader.derived[NeotypePerson]
      val writer: ConfigWriter[NeotypePerson] = KindlingsConfigWriter.derived[NeotypePerson]
      val person = NeotypePerson(NeotypeEmail("alice@example.com"), NeotypeAge(30), NeotypeNickname("ally"))
      val config = writer.to(person)
      ConfigSource.fromConfig(config.atKey("root")).at("root").load[NeotypePerson](reader) ==> Right(person)
    }

    test("rejects invalid neotype value") {
      val reader: ConfigReader[NeotypePerson] = KindlingsConfigReader.derived[NeotypePerson]
      val config = ConfigSource.string("""root { email = "not-an-email", age = 30, nickname = "ally" }""")
      val result = config.at("root").load[NeotypePerson](reader)
      assert(result.isLeft, s"Expected Left but got $result")
    }
  }
}
