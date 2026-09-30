package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.pureconfigderivation.{KindlingsConfigReader, KindlingsConfigWriter}
import pureconfig.*

final class NewtypePureconfigSpec extends MacroSuite {

  group("scala-newtype + PureConfig") {

    test("round-trip") {
      val reader: ConfigReader[NewtypeUser] = KindlingsConfigReader.derived[NewtypeUser]
      val writer: ConfigWriter[NewtypeUser] = KindlingsConfigWriter.derived[NewtypeUser]
      val config = writer.to(NewtypeUser.example)
      ConfigSource.fromConfig(config.atKey("root")).at("root").load[NewtypeUser](reader) ==> Right(NewtypeUser.example)
    }
  }
}
