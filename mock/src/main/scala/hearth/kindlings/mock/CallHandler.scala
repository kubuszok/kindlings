package hearth.kindlings.mock

/** A single expectation: which arguments it accepts, how many times it may be called, and what it does when called.
  *
  * Mirrors the relevant slice of ScalaMock's `CallHandler` — return value (`returning`), thrown exception (`throwing`),
  * computed result (`onCall`), and a call-count range (`once`/`twice`/`anyNumberOfTimes`/`repeat`).
  */
final class CallHandler private[mock] (val methodName: String, expectedArgs: Option[Seq[Any]]) {

  /** Number of value parameters of the overload this handler targets, or `-1` when unknown (matches any arity). Used by
    * [[MockContext]] to disambiguate overloaded methods that share a name.
    */
  private[mock] var arity: Int = -1

  // `None` until any of returning/throwing/onCall is set: a strict expectation with no behaviour yields the
  // per-method default supplied at the call site (mirrors ScalaMock returning null/0 for un-stubbed return types).
  private var onCallHandler: Option[Seq[Any] => Any] = None
  private var expectedCalls: Range = 1 to 1
  private var actualCalls: Int = 0

  /** Ordering-tree node this handler was registered under, plus its position among that node's children. Eligibility of
    * ordered handlers is enforced by walking the [[OrderingNode]] tree in [[MockContext]], not here.
    */
  private[mock] var orderingNode: OrderingNode = null
  private[mock] var orderingPos: Int = 0

  /** Number of times this handler has actually been called (used by [[MockContext]] for sequence eligibility). */
  private[mock] def callCount: Int = actualCalls

  /** Does this handler target the overload with `n` value parameters? (`arity == -1` matches any.) */
  private[mock] def matchesArity(n: Int): Boolean = arity < 0 || arity == n

  /** Does this handler accept a call with `args`? */
  private[mock] def matches(args: Seq[Any]): Boolean = expectedArgs match {
    case None           => true
    case Some(matchers) =>
      matchers.length == args.length && matchers.zip(args).forall {
        case (m: ArgMatcher, a) => m.matches(a)
        case (expected, actual) => expected == actual
      }
  }

  private[mock] def isExhausted: Boolean = actualCalls >= expectedCalls.last
  private[mock] def isSatisfied: Boolean = expectedCalls.contains(actualCalls)

  /** Run this handler for `args`, falling back to `default` when no behaviour (returning/throwing/onCall) was set. */
  private[mock] def call(args: Seq[Any], default: => Any): Any = {
    actualCalls += 1
    // Record any captured arguments now that this handler has actually fired (not merely matched a candidate scan).
    expectedArgs.foreach { matchers =>
      matchers.iterator.zip(args.iterator).foreach {
        case (c: Capture[?], actual) => c.record(actual)
        case _                       => ()
      }
    }
    onCallHandler match {
      case Some(h) => h(args)
      case None    => default
    }
  }

  /** Return `value` whenever this expectation matches. */
  def returning(value: Any): CallHandler = { onCallHandler = Some(_ => value); this }

  /** Alias for [[returning]] (ScalaMock's `returns`). */
  def returns(value: Any): CallHandler = returning(value)

  /** Throw `throwable` whenever this expectation matches. */
  def throwing(throwable: Throwable): CallHandler = { onCallHandler = Some(_ => throw throwable); this }

  /** Alias for [[throwing]] (ScalaMock's `throws`). */
  def throws(throwable: Throwable): CallHandler = throwing(throwable)

  /** Compute the result from the actual arguments (untyped). */
  def onCall(handler: Seq[Any] => Any): CallHandler = { onCallHandler = Some(handler); this }

  /** Compute the result from a single typed argument (ScalaMock's `onCall { (x: A) => ... }`). */
  def onCall1[A](handler: A => Any): CallHandler =
    onCall(args => handler(args.head.asInstanceOf[A]))

  /** Compute the result from two typed arguments. */
  def onCall2[A, B](handler: (A, B) => Any): CallHandler =
    onCall(args => handler(args(0).asInstanceOf[A], args(1).asInstanceOf[B]))

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

  /** Expect at least `n` calls. */
  def atLeast(n: Int): CallHandler = repeat(n to Int.MaxValue)

  /** Expect at most `n` calls (including zero). */
  def atMost(n: Int): CallHandler = repeat(0 to n)

  /** Expect no more than one call (zero or one). */
  def noMoreThanOnce(): CallHandler = repeat(0 to 1)

  /** Expect no more than two calls (zero, one or two). */
  def noMoreThanTwice(): CallHandler = repeat(0 to 2)

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

  /** Matches any argument (including `null`). */
  val any: ArgMatcher = (_: Any) => true

  /** Matches an argument satisfying `predicate`. */
  def where[A](predicate: A => Boolean): ArgMatcher = (actual: Any) => predicate(actual.asInstanceOf[A])

  /** A pair of matchers that, splatted side by side into an argument list, accept a 2-arg call iff `predicate` holds
    * over both actual arguments (ScalaMock's `where { (x, y) => ... }`). The matchers are stateful and share a cell, so
    * a given pair must not be reused across expectations.
    */
  def where2[A, B](predicate: (A, B) => Boolean): Seq[ArgMatcher] = {
    var first: Any = null
    // Matched left-to-right: the first records arg0 (always matches), the second checks the predicate over (arg0, arg1).
    val recordFirst: ArgMatcher = (a: Any) => { first = a; true }
    val checkBoth: ArgMatcher = (b: Any) => predicate(first.asInstanceOf[A], b.asInstanceOf[B])
    Seq(recordFirst, checkBoth)
  }

  /** Alias for the single-argument [[where]], mirroring ScalaMock's `argThat`. */
  def argThat[A](predicate: A => Boolean): ArgMatcher = where(predicate)

  /** [[argThat]] carrying a human-readable description (used in failure messages). Mirrors ScalaMock's
    * `argThat("desc") { ... }`; named distinctly to keep [[argThat]]'s single-argument form free of overload-resolution
    * ambiguity for placeholder lambdas (`_ > 100.0`) on Scala 2.
    */
  def argThatDescribed[A](description: String)(predicate: A => Boolean): ArgMatcher = new ArgMatcher {
    def matches(actual: Any): Boolean = predicate(actual.asInstanceOf[A])
    override def toString: String = description
  }

  /** A matcher that accepts any argument and records it for later inspection (ScalaMock's `capture`). */
  def capture[A](c: Capture[A]): ArgMatcher = c

  /** Build a matcher from a [[Matcher]]. */
  def from[A](m: Matcher[A]): ArgMatcher = (actual: Any) => m.matches(actual.asInstanceOf[A])

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

/** A typed argument matcher (ScalaMock's `org.scalamock.matchers.Matcher`). Implement [[matches]] to decide whether an
  * actual argument of type `A` is accepted; use via `ArgMatcher.from(myMatcher)` in an argument position.
  */
trait Matcher[A] {
  def matches(actual: A): Boolean
}

/** Captures the argument(s) a matching expectation was called with, for later inspection (ScalaMock's `capture` /
  * `CaptureOne` / `CaptureAll`). Use via `ArgMatcher.capture(c)` in an `expecting`/`expects` argument position; after
  * the call, read [[value]] (the most recent) or [[values]] (all captured, in call order).
  */
final class Capture[A] extends ArgMatcher {

  private var captured: Vector[A] = Vector.empty

  /** Always matches; the actual argument is recorded by the handler when it fires. */
  def matches(actual: Any): Boolean = true

  private[mock] def record(value: Any): Unit = captured = captured :+ value.asInstanceOf[A]

  /** Whether any value has been captured yet. */
  def hasValue: Boolean = captured.nonEmpty

  /** The most recently captured value; throws if nothing was captured (mirrors ScalaMock's `NoSuchElementException`).
    */
  def value: A =
    captured.lastOption.getOrElse(throw new NoSuchElementException("No argument was captured yet."))

  /** All captured values, in call order. */
  def values: Seq[A] = captured
}

/** Thrown when a mock is called unexpectedly or an expectation is left unsatisfied. */
final class MockExpectationException(message: String) extends RuntimeException(message)
