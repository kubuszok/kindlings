package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.sconfigderivation.{ConfigReader, ConfigWriter}

final class NewtypeSconfigSpec extends MacroSuite {

  group("scala-newtype + sconfig") {

    test("round-trip") {
      val reader: ConfigReader[NewtypeUser] = ConfigReader.derived[NewtypeUser]
      val writer: ConfigWriter[NewtypeUser] = ConfigWriter.derived[NewtypeUser]
      reader.from(writer.to(NewtypeUser.example)) ==> Right(NewtypeUser.example)
    }
  }
}
