package hearth.kindlings.mock

import hearth.MacroSuite

/** A minimal mock + expectation, kept deliberately tiny so it also runs on Scala.js and Scala Native (where the larger
  * [[MockSpec]] is slower / heavier). Proves the pure-macro engine works on every platform with no bytecode generation
  * or JVM proxies.
  */
final class MockSmokeSpec extends MacroSuite {

  test("a single mock + expectation works on every platform (JVM / JS / Native)") {
    implicit val ctx: MockContext = new MockContext
    val greeter = Mock.mock[MockSmokeSpec.Greeter]
    val _ = ctx.expecting("greet", "world").returning("hello, world")

    greeter.greet("world") ==> "hello, world"
    ctx.verifyExpectations()
  }

  test("a strict expectation with no `returning` yields the default on every platform") {
    implicit val ctx: MockContext = new MockContext
    val m = Mock.mock[MockSmokeSpec.Counter]
    val _ = ctx.expecting("count", 1)
    m.count(1) ==> 0
    ctx.verifyExpectations()
  }
}

object MockSmokeSpec {
  trait Greeter { def greet(name: String): String }
  trait Counter { def count(x: Int): Int }
}
