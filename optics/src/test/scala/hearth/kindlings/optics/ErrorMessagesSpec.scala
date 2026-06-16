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
  }
}

object ErrorMessagesSpec {

  final class NotACaseClass(val value: String)

  final case class Wrapper(value: String)
}
