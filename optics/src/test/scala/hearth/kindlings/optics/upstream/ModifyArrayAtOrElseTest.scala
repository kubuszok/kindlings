package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/ModifyArrayAtOrElseTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import TestData.*

class ModifyArrayAtOrElseTest extends QuicklensSuite {

  it should "modify an existing element using atOrElse" in {
    modify(ar1)(_.atOrElse(2, A3(A4(A5("default")))).a4.a5.name).using(duplicate) should be(l1at2dup)
  }

  it should "append the modified default for a missing index" in {
    modify(ar1)(_.atOrElse(10, A3(A4(A5("def")))).a4.a5.name).using(duplicate) should be(
      l1 :+ A3(A4(A5("defdef")))
    )
  }
}
