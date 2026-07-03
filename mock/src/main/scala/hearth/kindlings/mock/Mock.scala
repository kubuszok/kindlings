package hearth.kindlings.mock

/** Pure-macro mocking, in the spirit of [[https://scalamock.org ScalaMock]], built on Hearth's macro-agnostic
  * `AnonymousInstance` API so the same implementation works on Scala 2.13 and Scala 3 across the JVM, Scala.js and
  * Scala Native — with no bytecode generation or JVM proxies.
  *
  * {{{
  * import hearth.kindlings.mock.*
  *
  * trait Greeter { def greet(name: String): String }
  *
  * implicit val ctx: MockContext = new MockContext
  * val greeter = Mock.mock[Greeter]
  * ctx.expecting("greet", "world").returning("hello, world")
  *
  * greeter.greet("world") // "hello, world"
  * ctx.verifyExpectations()
  * }}}
  */
object Mock extends MockCompanionCompat {

  /** Special marker type — if its implicit is in scope, the mock macros log the generated code and the logic
    * (overridden members) that produced it. Import `hearth.kindlings.mock.debug.*` to enable, or set
    * `-Xmacro-settings:mock.logGeneration=true`. Kept outside auto-summoned scope.
    */
  sealed trait LogGeneration
  object LogGeneration extends LogGeneration
}
