package hearth.kindlings.optics

import hearth.MacroSuite
import hearth.kindlings.optics._

final class BareImportSpec extends MacroSuite {

  import BareImportSpec._

  test("import hearth.kindlings.optics._ enables modify directly (package object DSL)") {
    Person("a", 1).modify(_.name).setTo("b") ==> Person("b", 1)
    Person("a", 1).modify(_.age).using(_ + 1) ==> Person("a", 2)
  }
}

object BareImportSpec {
  final case class Person(name: String, age: Int)
}
