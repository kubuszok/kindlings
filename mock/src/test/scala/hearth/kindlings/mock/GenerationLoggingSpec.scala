package hearth.kindlings.mock

import hearth.MacroSuite

/** Smoke test for the opt-in generation logging: importing `hearth.kindlings.mock.debug.*` puts the `LogGeneration`
  * marker in scope, so `Mock.mock`/`Mock.stub` emit the overridden members + generated code as a compiler-info message.
  * Exercising a mock with the import in scope proves the logging path compiles and still generates a working mock.
  */
final class GenerationLoggingSpec extends MacroSuite {

  import hearth.kindlings.mock.debug.* // enables the generation log for macros in this scope

  group("mock with generation logging enabled") {

    test("still generates a working mock") {
      implicit val ctx: MockContext = new MockContext
      val greeter = Mock.mock[GenerationLoggingSpec.Greeter]
      val _ = ctx.expecting("greet", "world").returning("hello, world")
      greeter.greet("world") ==> "hello, world"
      ctx.verifyExpectations()
    }
  }
}

object GenerationLoggingSpec {
  trait Greeter { def greet(name: String): String }
}
