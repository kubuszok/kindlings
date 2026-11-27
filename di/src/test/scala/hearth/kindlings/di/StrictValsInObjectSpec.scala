package hearth.kindlings.di

/** macwire's most common style: strict `val`s in an object, wired from each other while the object is initialized.
  * Mirrors macwire's `simpleValsInObject` test case on a top-level object (the ported case uses a local one).
  */
object StrictValsInObjectSpec {
  final class SA()
  final class SB()
  final class SC(val a: SA, val b: SB)

  object Module {
    val a: SA = new SA()
    val b: SB = new SB()
    val c: SC = DI.wire[SC]
  }
}

final class StrictValsInObjectSpec extends hearth.MacroSuite {
  import StrictValsInObjectSpec.*

  test("wires strict vals of an object during its initialization") {
    assert(Module.c.a eq Module.a)
    assert(Module.c.b eq Module.b)
  }
}
