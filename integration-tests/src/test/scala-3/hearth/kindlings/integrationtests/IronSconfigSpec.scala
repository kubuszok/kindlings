package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.*
import hearth.MacroSuite
import hearth.kindlings.sconfigderivation.{ConfigReader, ConfigWriter}

final class IronSconfigSpec extends MacroSuite {

  group("Iron + sconfig") {

    test("round-trip") {
      implicit val reader: ConfigReader[IronPerson] = ConfigReader.derive[IronPerson]
      implicit val writer: ConfigWriter[IronPerson] = ConfigWriter.derive[IronPerson]
      val v = IronPerson("Alice", 30)
      val config = writer.to(v)
      val result = reader.from(config)
      assert(result.isRight, s"Expected Right but got $result")
    }

    test("rejects invalid iron value") {
      implicit val reader: ConfigReader[IronPerson] = ConfigReader.derive[IronPerson]
      val config = org.ekrich.config.ConfigFactory.parseString("""name = "Alice"
age = -1""")
      val result = reader.from(config.root)
      assert(result.isLeft, s"Expected Left but got $result")
    }
  }
}
