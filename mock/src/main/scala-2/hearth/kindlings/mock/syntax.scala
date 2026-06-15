package hearth.kindlings.mock

import scala.language.experimental.macros

/** Faithful ScalaMock-style expectation DSL: `(m.method _).expects(args...).returning(value)`.
  *
  * Import `hearth.kindlings.mock.syntax._` to enable it. The expectation is registered on the [[MockContext]] that is
  * implicitly in scope (the same one the mock was created with), so it composes with the name-keyed
  * [[MockContext.expecting]] API.
  */
object syntax {

  implicit class MockFunctionOps(val f: Any) {
    def expects(args: Any*): CallHandler = macro internal.compiletime.MockMacros.expectsImpl
    def when(args: Any*): CallHandler = macro internal.compiletime.MockMacros.whenImpl
    def verify(args: Any*): VerifyTarget = macro internal.compiletime.MockMacros.verifyImpl
  }
}
