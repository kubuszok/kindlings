package hearth.kindlings.integrationtests

import cats.data.{Chain, Const, NonEmptyList}
import hearth.MacroSuite
import hearth.kindlings.pureconfigderivation.{KindlingsConfigReader, KindlingsConfigWriter}
import pureconfig.*

final class CatsPureconfigSpec extends MacroSuite {

  group("Cats + PureConfig") {

    group("NonEmptyList") {

      test("round-trip") {
        val reader: ConfigReader[WithNEL] = KindlingsConfigReader.derived[WithNEL]
        val writer: ConfigWriter[WithNEL] = KindlingsConfigWriter.derived[WithNEL]
        val v = WithNEL(NonEmptyList.of(1, 2, 3))
        val config = writer.to(v)
        val result = ConfigSource.fromConfig(config.atKey("root")).at("root").load[WithNEL](reader)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("Chain") {

      test("round-trip") {
        val reader: ConfigReader[WithChain] = KindlingsConfigReader.derived[WithChain]
        val writer: ConfigWriter[WithChain] = KindlingsConfigWriter.derived[WithChain]
        val v = WithChain(Chain(1, 2, 3))
        val config = writer.to(v)
        val result = ConfigSource.fromConfig(config.atKey("root")).at("root").load[WithChain](reader)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("Const") {

      test("round-trip") {
        val reader: ConfigReader[WithConst] = KindlingsConfigReader.derived[WithConst]
        val writer: ConfigWriter[WithConst] = KindlingsConfigWriter.derived[WithConst]
        val v = WithConst(Const("hello"))
        val config = writer.to(v)
        val result = ConfigSource.fromConfig(config.atKey("root")).at("root").load[WithConst](reader)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }
  }
}
