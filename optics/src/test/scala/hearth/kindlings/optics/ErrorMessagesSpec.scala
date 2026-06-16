package hearth.kindlings.optics

import hearth.MacroSuite

final class ErrorMessagesSpec extends MacroSuite {

  group("modify error messages") {

    test("modifying a field of a non-case-class is rejected") {
      compileErrors(
        """
        import hearth.kindlings.optics.syntax.*
        import hearth.kindlings.optics.ErrorMessagesSpec.*
        new NotACaseClass("x").modify(_.value).setTo("y")
        """
      ).check(
        "can only descend into case classes"
      )
    }

    test("a non-field-access path is rejected") {
      compileErrors(
        """
        import hearth.kindlings.optics.syntax.*
        import hearth.kindlings.optics.ErrorMessagesSpec.*
        Wrapper("x").modify(w => w.value + "!").setTo("y")
        """
      ).check(
        "field-access path"
      )
    }

    // NOTE: the `.each`/`.at`/`.when`/... markers carry `@compileTimeOnly` + a `sys.error` body so that using them
    // outside a `modify(...)` path is guarded. We deliberately do NOT assert the compile-time message here, because
    // `@compileTimeOnly` fires on the Scala 3 `extension` marker but not on the Scala 2 `implicit class` one (a genuine
    // platform difference), so the error text is not uniform across platforms; the runtime `sys.error` is the floor.
  }
}

object ErrorMessagesSpec {

  final class NotACaseClass(val value: String)

  final case class Wrapper(value: String)
}
