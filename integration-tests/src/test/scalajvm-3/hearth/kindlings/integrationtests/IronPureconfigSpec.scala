package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.*
import hearth.MacroSuite
import hearth.kindlings.pureconfigderivation.{KindlingsConfigReader, KindlingsConfigWriter}
import pureconfig.*

final class IronPureconfigSpec extends MacroSuite {

  group("Iron + PureConfig") {

    test("round-trip") {
      val reader: ConfigReader[IronPerson] = KindlingsConfigReader.derive[IronPerson]
      val writer: ConfigWriter[IronPerson] = KindlingsConfigWriter.derive[IronPerson]
      val v = IronPerson("Alice", 30)
      val config = writer.to(v)
      val result = ConfigSource.fromConfig(config.atKey("root")).at("root").load[IronPerson](reader)
      assert(result.isRight, s"Expected Right but got $result")
    }
  }
}
