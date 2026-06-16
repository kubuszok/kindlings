package hearth.kindlings.mock

/** Mix-in for test suites that provides an ambient [[MockContext]] plus ScalaMock-style scoped auto-verification.
  *
  * Framework-agnostic by design — it has NO ScalaTest / munit / specs2 dependency, so it can be mixed into a suite of
  * any framework. Create mocks/stubs inside a [[withExpectations]] block and verification happens automatically at the
  * end of the block: there is no need for a manual `mockContext.verifyExpectations()` call (the gap this closes versus
  * the bare [[MockContext]] API). The implicit [[mockContext]] is exactly the one the `Mock.mock`/`Mock.stub` macros and
  * the `(m.method _).expects/.when/.verify` DSL resolve at the call site.
  *
  * Mirrors ScalaMock's `MockFactoryBase.withExpectations`: a non-fatal failure inside the block is rethrown unmasked
  * (ScalaMock issue #72) and the context is reset between blocks, so a single suite instance can run many tests.
  *
  * {{{
  * class MySpec extends munit.FunSuite with MockSuite {
  *   test("greets") {
  *     withExpectations {
  *       val greeter = Mock.mock[Greeter]
  *       val _ = (greeter.greet _).expects("world").returning("hi")
  *       assertEquals(greeter.greet("world"), "hi")
  *       // no manual verifyExpectations() — withExpectations verifies on block exit
  *     }
  *   }
  * }
  * }}}
  *
  * For a per-test auto-verify hook (no explicit `withExpectations` block), a thin framework-specific mix-in can call
  * [[MockContext.reset]] before and [[MockContext.verifyExpectations]] after each test; that lives outside this module
  * to keep it dependency-free.
  */
trait MockSuite {

  /** The ambient context resolved by `Mock.mock`/`Mock.stub` and the `.expects`/`.when`/`.verify` DSL. */
  protected implicit val mockContext: MockContext = new MockContext

  /** Run `body` with the ambient [[mockContext]], automatically verifying expectations at the end of the block. See
    * [[MockContext.withExpectations]].
    */
  protected def withExpectations[T](body: => T): T = mockContext.withExpectations(body)
}
