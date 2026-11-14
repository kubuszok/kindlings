package hearth.kindlings.derivation.compiletime

final class DerivationErrorRenderingSpec extends hearth.MacroSuite {

  final private class NoMessage extends RuntimeException(null: String)

  group("DerivationErrorRendering.messageOf") {

    test("returns the message when there is one") {
      DerivationErrorRendering.messageOf(new RuntimeException("boom")) ==> "boom"
    }

    test("falls back to toString (the class name) when the message is null") {
      val e = new NoMessage
      assert(e.getMessage == null)
      DerivationErrorRendering.messageOf(e) ==> e.toString
      assert(DerivationErrorRendering.messageOf(e).contains("NoMessage"))
    }
  }

  group("DerivationErrorRendering.renderError") {

    test("renders a single-line message as one bullet") {
      DerivationErrorRendering.renderError(new RuntimeException("boom")) ==> "  - boom"
    }

    test("indents continuation lines of a multi-line message") {
      DerivationErrorRendering.renderError(new RuntimeException("first\nsecond\nthird")) ==>
        "  - first\n    second\n    third"
    }

    test("renders an error whose message is null by its class name instead of throwing") {
      val e = new NoMessage
      DerivationErrorRendering.renderError(e) ==> s"  - ${e.toString}"
    }

    test("renders an empty message as an empty bullet") {
      DerivationErrorRendering.renderError(new RuntimeException("")) ==> "  - "
    }

    test("renders a message made only of line breaks (which splits into no lines) as a bullet") {
      DerivationErrorRendering.renderError(new RuntimeException("\n\n")) ==> "  - \n\n"
    }
  }

  group("DerivationErrorRendering.renderErrors") {

    test("joins every rendered error with newlines, including ones without a message") {
      val e = new NoMessage
      DerivationErrorRendering.renderErrors(List(new RuntimeException("a\nb"), e)) ==>
        s"  - a\n    b\n  - ${e.toString}"
    }
  }
}
