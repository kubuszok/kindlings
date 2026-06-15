package hearth.kindlings.mock

/** Faithful ScalaMock-style expectation DSL: `(m.method _).expects(args...).returning(value)`.
  *
  * Import `hearth.kindlings.mock.syntax.*` to enable it. The expectation is registered on the [[MockContext]] that is
  * implicitly in scope (the same one the mock was created with), so it composes with the name-keyed
  * [[MockContext.expecting]] API.
  */
object syntax {

  extension (inline f: Any) {
    inline def expects(inline args: Any*): CallHandler =
      ${ internal.compiletime.MockMacros.expectsImpl('f, 'args) }

    inline def when(inline args: Any*): CallHandler =
      ${ internal.compiletime.MockMacros.whenImpl('f, 'args) }

    inline def verify(inline args: Any*): VerifyTarget =
      ${ internal.compiletime.MockMacros.verifyImpl('f, 'args) }
  }
}
