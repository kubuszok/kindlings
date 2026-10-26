// Deliberately OUTSIDE `hearth.kindlings.optics`: inside that package its package-object DSL is visible without any
// import, so the bare import below would not be exercised at all.
package hearth.kindlings.opticsbareimport

import hearth.MacroSuite
import hearth.kindlings.optics.*

final class BareImportSpec extends MacroSuite {

  import BareImportSpec.*

  test("import hearth.kindlings.optics._ enables modify directly (package object DSL)") {
    Person("a", 1).modify(_.name).setTo("b") ==> Person("b", 1)
    Person("a", 1).modify(_.age).using(_ + 1) ==> Person("a", 2)
  }
}

object BareImportSpec {
  final case class Person(name: String, age: Int)
}
