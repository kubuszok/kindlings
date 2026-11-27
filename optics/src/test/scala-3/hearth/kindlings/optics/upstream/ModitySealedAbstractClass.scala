package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala-3/com/softwaremill/quicklens/test/ModitySealedAbstractClass.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import TestData.*

class ModitySealedAbstractClass extends QuicklensSuite {
  it should "Modify abstract class hierarchy" in {
    invInt.modify(_.typ).setTo(Type("Long")) should be(invLong)
  }
}
