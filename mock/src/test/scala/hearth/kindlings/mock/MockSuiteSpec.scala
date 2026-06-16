package hearth.kindlings.mock

import hearth.MacroSuite

/** End-to-end demonstration of [[MockSuite]]: the ambient `mockContext` is resolved by `Mock.mock`, and
  * `withExpectations` auto-verifies on block exit (no manual `verifyExpectations()` call), mirroring ScalaMock's
  * `MockFactory`.
  */
final class MockSuiteSpec extends MacroSuite with MockSuite {

  group("MockSuite (ScalaMock MockFactory-style auto-verify)") {

    test("a mock created inside withExpectations resolves the ambient context and auto-verifies") {
      withExpectations {
        val m = Mock.mock[MockSuiteSpec.Calc] // resolves the implicit mockContext from MockSuite
        val _ = mockContext.expecting("add", 2, 3).returning(5)
        m.add(2, 3) ==> 5
      }
    }

    test("withExpectations fails the test when an expectation is left unsatisfied") {
      intercept[MockExpectationException] {
        withExpectations {
          val _ = Mock.mock[MockSuiteSpec.Calc]
          val _ = mockContext.expecting("add", 2, 3).returning(5)
          () // never call add -> unsatisfied -> auto-verify fails
        }
      }
    }
  }
}

object MockSuiteSpec {
  trait Calc {
    def add(a: Int, b: Int): Int
  }
}
