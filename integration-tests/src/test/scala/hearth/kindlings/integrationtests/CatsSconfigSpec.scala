package hearth.kindlings.integrationtests

import cats.data.{Chain, Const, NonEmptyList, NonEmptyVector}
import hearth.MacroSuite
import hearth.kindlings.sconfigderivation.{ConfigReader, ConfigWriter}

final class CatsSconfigSpec extends MacroSuite {

  group("Cats + sconfig") {

    group("NonEmptyList") {

      test("round-trip") {
        implicit val reader: ConfigReader[WithNEL] = ConfigReader.derive[WithNEL]
        implicit val writer: ConfigWriter[WithNEL] = ConfigWriter.derive[WithNEL]
        val v = WithNEL(NonEmptyList.of(1, 2, 3))
        val config = writer.to(v)
        val result = reader.from(config)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("NonEmptyVector") {

      test("round-trip") {
        implicit val reader: ConfigReader[WithNEV] = ConfigReader.derive[WithNEV]
        implicit val writer: ConfigWriter[WithNEV] = ConfigWriter.derive[WithNEV]
        val v = WithNEV(NonEmptyVector.of(10, 20))
        val config = writer.to(v)
        val result = reader.from(config)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("Chain") {

      test("round-trip") {
        implicit val reader: ConfigReader[WithChain] = ConfigReader.derive[WithChain]
        implicit val writer: ConfigWriter[WithChain] = ConfigWriter.derive[WithChain]
        val v = WithChain(Chain(1, 2, 3))
        val config = writer.to(v)
        val result = reader.from(config)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }

    group("Const") {

      test("round-trip") {
        implicit val reader: ConfigReader[WithConst] = ConfigReader.derive[WithConst]
        implicit val writer: ConfigWriter[WithConst] = ConfigWriter.derive[WithConst]
        val v = WithConst(Const("hello"))
        val config = writer.to(v)
        val result = reader.from(config)
        assert(result.isRight, s"Expected Right but got $result")
      }
    }
  }
}
