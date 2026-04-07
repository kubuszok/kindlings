package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import org.ekrich.config.ConfigValueFactory

final class InstancesSpec extends MacroSuite {

  test("doubleReader is summonable") {
    val r: ConfigReader[Double] = implicitly[ConfigReader[Double]]
    r.from(ConfigValueFactory.fromAnyRef(java.lang.Double.valueOf(3.14))) ==> Right(3.14)
  }

  test("intReader is summonable") {
    val r: ConfigReader[Int] = implicitly[ConfigReader[Int]]
    r.from(ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(42))) ==> Right(42)
  }

  test("stringReader is summonable") {
    val r: ConfigReader[String] = implicitly[ConfigReader[String]]
    r.from(ConfigValueFactory.fromAnyRef("hello")) ==> Right("hello")
  }
}
