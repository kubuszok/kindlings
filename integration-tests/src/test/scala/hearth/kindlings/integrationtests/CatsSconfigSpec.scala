package hearth.kindlings.integrationtests

import cats.data.{Chain, Const, NonEmptyList, NonEmptyVector}
import hearth.MacroSuite
import hearth.kindlings.sconfigderivation.{ConfigReader, ConfigWriter}

final class CatsSconfigSpec extends MacroSuite {

  group("Cats + sconfig") {

    group("NonEmptyList") {

      test("round-trip") {
        implicit val reader: ConfigReader[WithNEL] = ConfigReader.derived[WithNEL]
        implicit val writer: ConfigWriter[WithNEL] = ConfigWriter.derived[WithNEL]
        val v = WithNEL(NonEmptyList.of(1, 2, 3))
        val config = writer.to(v)
        val result = reader.from(config)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("NonEmptyVector") {

      test("round-trip") {
        implicit val reader: ConfigReader[WithNEV] = ConfigReader.derived[WithNEV]
        implicit val writer: ConfigWriter[WithNEV] = ConfigWriter.derived[WithNEV]
        val v = WithNEV(NonEmptyVector.of(10, 20))
        val config = writer.to(v)
        val result = reader.from(config)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("Chain") {

      test("round-trip") {
        implicit val reader: ConfigReader[WithChain] = ConfigReader.derived[WithChain]
        implicit val writer: ConfigWriter[WithChain] = ConfigWriter.derived[WithChain]
        val v = WithChain(Chain(1, 2, 3))
        val config = writer.to(v)
        val result = reader.from(config)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("Const") {

      test("round-trip") {
        implicit val reader: ConfigReader[WithConst] = ConfigReader.derived[WithConst]
        implicit val writer: ConfigWriter[WithConst] = ConfigWriter.derived[WithConst]
        val v = WithConst(Const("hello"))
        val config = writer.to(v)
        val result = reader.from(config)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }
  }
}
