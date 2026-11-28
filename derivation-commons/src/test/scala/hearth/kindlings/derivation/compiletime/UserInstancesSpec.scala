package hearth.kindlings.derivation.compiletime

final class UserInstancesSpec extends hearth.MacroSuite {

  // the explanation of a Scala 3 implicit search whose candidate's macro threw
  private val crashExplanation =
    """Exception occurred while executing macro expansion.
      |java.lang.IllegalStateException: the instance macro crashed
      |	at example.CrashingMacros$.crash(Crashing.scala:14)
      |	at example.CrashingMacros$.inline$crash(Crashing.scala:10)
      |""".stripMargin

  group("UserInstances.isCrashExplanation") {

    test("recognizes a crashed macro expansion") {
      UserInstances.isCrashExplanation(crashExplanation) ==> true
    }

    test("does not take a missing instance for a crash") {
      UserInstances.isCrashExplanation("no implicit values were found that match type Encoder[Item]") ==> false
    }
  }

  group("UserInstances.crashReason") {

    test("keeps the exception and its message, without the stack trace") {
      UserInstances.crashReason(crashExplanation) ==>
        "Exception occurred while executing macro expansion. " +
        "java.lang.IllegalStateException: the instance macro crashed"
    }

    test("keeps a message of several lines") {
      UserInstances.crashReason(
        """Exception occurred while executing macro expansion.
          |java.lang.RuntimeException: first line
          |second line
          |	at example.Macros$.impl(Macros.scala:1)
          |""".stripMargin
      ) ==> "Exception occurred while executing macro expansion. java.lang.RuntimeException: first line second line"
    }

    test("starts at the crash when the explanation has a prefix") {
      UserInstances.crashReason("Some context\n" + crashExplanation) ==>
        "Exception occurred while executing macro expansion. " +
        "java.lang.IllegalStateException: the instance macro crashed"
    }
  }
}
