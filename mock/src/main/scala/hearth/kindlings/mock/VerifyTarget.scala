package hearth.kindlings.mock

/** Post-hoc call-count assertions for a stubbed method (ScalaMock's `(s.method _).verify(args).once()`).
  *
  * Created by [[MockContext.verify]]; each terminal method checks the number of recorded calls to the method with
  * matching arguments and throws a [[MockExpectationException]] on a mismatch.
  */
final class VerifyTarget private[mock] (ctx: MockContext, methodName: String, expectedArgs: Seq[Any]) {

  private def count: Int = ctx.callCountFor(methodName, expectedArgs)

  private def fail(expectation: String, actual: Int): Nothing =
    throw new MockExpectationException(
      s"Expected $methodName(${expectedArgs.mkString(", ")}) to be called $expectation, but it was called $actual time(s)."
    )

  /** Assert the method was called exactly `n` times. */
  def wasCalled(n: Int): Unit = { val c = count; if (c != n) fail(s"$n time(s)", c) }

  /** Assert the method was called exactly once. */
  def once(): Unit = wasCalled(1)

  /** Assert the method was called exactly twice. */
  def twice(): Unit = wasCalled(2)

  /** Assert the method was never called. */
  def never(): Unit = wasCalled(0)

  /** Assert the method was called at least once. */
  def atLeastOnce(): Unit = { val c = count; if (c < 1) fail("at least once", c) }

  /** Assert the method was called at least `n` times. */
  def atLeast(n: Int): Unit = { val c = count; if (c < n) fail(s"at least $n time(s)", c) }

  /** Assert the method was called at most `n` times. */
  def atMost(n: Int): Unit = { val c = count; if (c > n) fail(s"at most $n time(s)", c) }
}
