package hearth.kindlings.mock

/** A single expectation: which arguments it accepts, how many times it may be called, and what it does when called.
  *
  * Mirrors the relevant slice of ScalaMock's `CallHandler` — return value (`returning`), thrown exception
  * (`throwing`), computed result (`onCall`), and a call-count range (`once`/`twice`/`anyNumberOfTimes`/`repeat`).
  */
final class CallHandler private[mock] (val methodName: String, expectedArgs: Option[Seq[Any]]) {

  private var onCallHandler: Seq[Any] => Any = _ => ()
  private var expectedCalls: Range = 1 to 1
  private var actualCalls: Int = 0

  /** When this handler belongs to an ordered `inSequence` group: the group's id and this handler's position within it.
    * Cross-method (a single sequence may span several methods), so ordering is enforced by [[MockContext]], not here.
    */
  private[mock] var sequenceId: Option[Int] = None
  private[mock] var sequencePos: Int = 0

  /** Number of times this handler has actually been called (used by [[MockContext]] for sequence eligibility). */
  private[mock] def callCount: Int = actualCalls

  /** Does this handler accept a call with `args`? */
  private[mock] def matches(args: Seq[Any]): Boolean = expectedArgs match {
    case None          => true
    case Some(matchers) =>
      matchers.length == args.length && matchers.zip(args).forall {
        case (m: ArgMatcher, a) => m.matches(a)
        case (expected, actual) => expected == actual
      }
  }

  private[mock] def isExhausted: Boolean = actualCalls >= expectedCalls.last
  private[mock] def isSatisfied: Boolean = expectedCalls.contains(actualCalls)

  private[mock] def call(args: Seq[Any]): Any = {
    actualCalls += 1
    // Record any captured arguments now that this handler has actually fired (not merely matched a candidate scan).
    expectedArgs.foreach { matchers =>
      matchers.iterator.zip(args.iterator).foreach {
        case (c: Capture[_], actual) => c.record(actual)
        case _                       => ()
      }
    }
    onCallHandler(args)
  }

  /** Return `value` whenever this expectation matches. */
  def returning(value: Any): CallHandler = { onCallHandler = _ => value; this }

  /** Throw `throwable` whenever this expectation matches. */
  def throwing(throwable: Throwable): CallHandler = { onCallHandler = _ => throw throwable; this }

  /** Compute the result from the actual arguments. */
  def onCall(handler: Seq[Any] => Any): CallHandler = { onCallHandler = handler; this }

  /** Expect exactly one call. */
  def once(): CallHandler = repeat(1)

  /** Expect exactly two calls. */
  def twice(): CallHandler = repeat(2)

  /** Expect that this is never called. */
  def never(): CallHandler = repeat(0)

  /** Expect any number of calls (including zero). */
  def anyNumberOfTimes(): CallHandler = repeat(0 to Int.MaxValue)

  /** Expect at least one call. */
  def atLeastOnce(): CallHandler = repeat(1 to Int.MaxValue)

  /** Expect exactly `n` calls. */
  def repeat(n: Int): CallHandler = repeat(n to n)

  /** Expect a number of calls within `range`. */
  def repeat(range: Range): CallHandler = { expectedCalls = range; this }

  override def toString: String = {
    val argsStr = expectedArgs.fold("(*)")(_.mkString("(", ", ", ")"))
    val countStr =
      if (expectedCalls == (1 to 1)) "once"
      else if (expectedCalls.last == Int.MaxValue) s"at least ${expectedCalls.start} times"
      else s"${expectedCalls.start}..${expectedCalls.last} times"
    s"$methodName$argsStr $countStr (called $actualCalls times${if (isSatisfied) "" else " - UNSATISFIED"})"
  }
}

/** A custom argument matcher usable in [[MockContext.expecting]] in place of a literal expected value. */
trait ArgMatcher {
  def matches(actual: Any): Boolean
}
object ArgMatcher {

  /** Matches any argument. */
  val any: ArgMatcher = (_: Any) => true

  /** Matches an argument satisfying `predicate`. */
  def where[A](predicate: A => Boolean): ArgMatcher = (actual: Any) => predicate(actual.asInstanceOf[A])

  /** Alias for [[where]], mirroring ScalaMock's `argThat`. */
  def argThat[A](predicate: A => Boolean): ArgMatcher = where(predicate)

  /** A matcher that accepts any argument and records it for later inspection (ScalaMock's `capture`). */
  def capture[A](c: Capture[A]): ArgMatcher = c

  /** Matches a numeric argument within `tolerance` of `target` (ScalaMock's `epsilon`). */
  def epsilon(target: Double, tolerance: Double = 0.001): ArgMatcher =
    (actual: Any) =>
      actual match {
        case n: Double => math.abs(n - target) <= tolerance
        case n: Float  => math.abs(n.toDouble - target) <= tolerance
        case n: Int    => math.abs(n.toDouble - target) <= tolerance
        case n: Long   => math.abs(n.toDouble - target) <= tolerance
        case _         => false
      }
}

/** Captures the argument(s) a matching expectation was called with, for later inspection (ScalaMock's `capture` /
  * `CaptureOne` / `CaptureAll`). Use via `ArgMatcher.capture(c)` in an `expecting`/`expects` argument position; after the
  * call, read [[value]] (the most recent) or [[values]] (all captured, in call order).
  */
final class Capture[A] extends ArgMatcher {

  private var captured: Vector[A] = Vector.empty

  /** Always matches; the actual argument is recorded by the handler when it fires. */
  def matches(actual: Any): Boolean = true

  private[mock] def record(value: Any): Unit = captured = captured :+ value.asInstanceOf[A]

  /** Whether any value has been captured yet. */
  def hasValue: Boolean = captured.nonEmpty

  /** The most recently captured value; throws if nothing was captured. */
  def value: A =
    captured.lastOption.getOrElse(throw new MockExpectationException("No argument was captured yet."))

  /** All captured values, in call order. */
  def values: Seq[A] = captured
}

/** Thrown when a mock is called unexpectedly or an expectation is left unsatisfied. */
final class MockExpectationException(message: String) extends RuntimeException(message)
