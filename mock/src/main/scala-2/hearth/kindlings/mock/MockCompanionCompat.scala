package hearth.kindlings.mock

import scala.language.experimental.macros

private[mock] trait MockCompanionCompat { this: Mock.type =>

  /** Create a mock of `A`. Every abstract member forwards into the implicit [[MockContext]]. */
  def mock[A](implicit ctx: MockContext): A = macro internal.compiletime.MockMacros.mockImpl[A]

  /** Create a stub of `A`. Every abstract member returns a default until preset via `when(...)`; calls are recorded for
    * post-hoc `verify(...)`.
    */
  def stub[A](implicit ctx: MockContext): A = macro internal.compiletime.MockMacros.stubImpl[A]
}
