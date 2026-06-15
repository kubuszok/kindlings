package hearth.kindlings.mock

private[mock] trait MockCompanionCompat { this: Mock.type =>

  /** Create a mock of `A`. Every abstract member forwards into the given [[MockContext]]. */
  inline def mock[A](using ctx: MockContext): A = ${ internal.compiletime.MockMacros.mockImpl[A]('ctx) }

  /** Create a stub of `A`. Every abstract member returns a default until preset via `when(...)`; calls are recorded for
    * post-hoc `verify(...)`.
    */
  inline def stub[A](using ctx: MockContext): A = ${ internal.compiletime.MockMacros.stubImpl[A]('ctx) }
}
