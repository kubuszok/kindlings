package hearth.kindlings.mock

import scala.collection.mutable

/** Runtime engine that backs every macro-generated mock.
  *
  * A mock produced by [[Mock.mock]] overrides each abstract member of the mocked type with a body that forwards the
  * call into the [[MockContext]] that was in scope when the mock was created (`handle`). Expectations are registered up
  * front (`expecting`) and matched against incoming calls; at the end of a test [[verifyExpectations]] checks that every
  * expectation was satisfied.
  *
  * This is the cross-platform, pure-Scala (no reflection, no bytecode generation) core that works identically on the
  * JVM, Scala.js and Scala Native.
  */
final class MockContext {

  private val handlers: mutable.Map[String, mutable.ListBuffer[CallHandler]] = mutable.Map.empty
  private val callLog: mutable.ListBuffer[(String, Seq[Any])] = mutable.ListBuffer.empty

  /** Ordered `inSequence` groups: each id maps to the handlers registered inside that block, in registration order
    * (possibly spanning several methods). Eligibility for sequenced handlers is enforced in [[handle]].
    */
  private val sequences: mutable.Map[Int, mutable.ListBuffer[CallHandler]] = mutable.Map.empty
  private var nextSequenceId: Int = 0
  // The sequence id (if any) that expectations registered right now belong to; `None` means unordered.
  private var currentSequence: Option[Int] = None

  /** Register an expectation for a call to `methodName` with arguments matching `expectedArgs`.
    *
    * Passing no `expectedArgs` matches a call with any arguments (including none).
    */
  def expecting(methodName: String, expectedArgs: Any*): CallHandler =
    expectingSeq(methodName, expectedArgs)

  /** Like [[expecting]] but takes the expected arguments as a `Seq` rather than as varargs. Used by the faithful
    * `(m.method _).expects(...)` DSL macro, which already has the arguments as a `Seq[Any]`.
    *
    * Passing an empty `Seq` matches a call with any arguments (including none).
    */
  def expectingSeq(methodName: String, expectedArgs: Seq[Any]): CallHandler = {
    val handler = new CallHandler(methodName, if (expectedArgs.isEmpty) None else Some(expectedArgs))
    val _ = handlers.getOrElseUpdate(methodName, mutable.ListBuffer.empty).append(handler)
    currentSequence.foreach { sid =>
      val group = sequences.getOrElseUpdate(sid, mutable.ListBuffer.empty)
      handler.sequenceId = Some(sid)
      handler.sequencePos = group.length
      val _ = group.append(handler)
    }
    handler
  }

  /** Register the expectations created inside `block` as an ordered group: calls must arrive in the order the
    * expectations were declared (mirrors ScalaMock's `inSequence`). Sequences may span several methods and may be
    * nested with [[inAnyOrder]].
    */
  def inSequence[T](block: => T): T = {
    val previous = currentSequence
    val sid = nextSequenceId
    nextSequenceId += 1
    currentSequence = Some(sid)
    try block
    finally currentSequence = previous
  }

  /** Register the expectations created inside `block` without ordering constraints, even when nested inside an
    * [[inSequence]] block (mirrors ScalaMock's `inAnyOrder`). This is the default at the top level.
    */
  def inAnyOrder[T](block: => T): T = {
    val previous = currentSequence
    currentSequence = None
    try block
    finally currentSequence = previous
  }

  /** A sequenced handler may only fire once every earlier handler in its sequence is satisfied and no later handler has
    * fired yet; unsequenced handlers are always eligible.
    */
  private def sequenceEligible(handler: CallHandler): Boolean = handler.sequenceId match {
    case None => true
    case Some(sid) =>
      val group = sequences.getOrElse(sid, mutable.ListBuffer.empty)
      group.forall { other =>
        if (other.sequencePos < handler.sequencePos) other.isSatisfied
        else if (other.sequencePos > handler.sequencePos) other.callCount == 0
        else true
      }
  }

  /** Dispatch a call coming from a generated mock body. Used by macro-generated code only. */
  def handle(methodName: String, args: Seq[Any]): Any = {
    callLog.append(methodName -> args)
    val candidates = handlers.getOrElse(methodName, mutable.ListBuffer.empty)
    candidates.find(h => !h.isExhausted && sequenceEligible(h) && h.matches(args)) match {
      case Some(handler) => handler.call(args)
      case None          =>
        throw new MockExpectationException(
          s"Unexpected call: $methodName(${args.mkString(", ")})" +
            (if (candidates.isEmpty) "" else s"\n\nExpected:\n${candidates.map("  " + _).mkString("\n")}")
        )
    }
  }

  /** Assert that every registered expectation has been satisfied; throws otherwise. Call at the end of a test. */
  def verifyExpectations(): Unit = {
    val unsatisfied = handlers.values.flatten.filterNot(_.isSatisfied).toList
    if (unsatisfied.nonEmpty)
      throw new MockExpectationException(
        s"Unsatisfied expectation(s):\n${unsatisfied.map("  " + _).mkString("\n")}"
      )
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Stub support (lenient record-and-verify mode used by `Mock.stub`)
  // -------------------------------------------------------------------------------------------------------------------

  /** Preset a stub's return behaviour for `methodName` with arguments matching `expectedArgs` (ScalaMock's `when`). A
    * preset is lenient: it matches any number of calls (including zero), so it never fails [[verifyExpectations]].
    */
  def when(methodName: String, expectedArgs: Any*): CallHandler = whenSeq(methodName, expectedArgs)

  /** [[when]] taking the expected arguments as a `Seq` (used by the faithful `(s.method _).when(...)` DSL macro). */
  def whenSeq(methodName: String, expectedArgs: Seq[Any]): CallHandler = {
    val handler = new CallHandler(methodName, if (expectedArgs.isEmpty) None else Some(expectedArgs))
    val _ = handler.anyNumberOfTimes()
    val _ = handlers.getOrElseUpdate(methodName, mutable.ListBuffer.empty).append(handler)
    handler
  }

  /** Dispatch a call from a generated `stub` body: record it, run a matching preset if any, otherwise return `default`
    * (never throws on an unexpected call). Used by macro-generated code only.
    */
  def handleStub(methodName: String, args: Seq[Any], default: => Any): Any = {
    callLog.append(methodName -> args)
    handlers.getOrElse(methodName, mutable.ListBuffer.empty).find(h => !h.isExhausted && h.matches(args)) match {
      case Some(handler) => handler.call(args)
      case None          => default
    }
  }

  /** How many recorded calls to `methodName` matched `expectedArgs` (empty `expectedArgs` matches any). */
  def callCountFor(methodName: String, expectedArgs: Seq[Any]): Int = {
    val probe = new CallHandler(methodName, if (expectedArgs.isEmpty) None else Some(expectedArgs))
    callLog.count { case (name, args) => name == methodName && probe.matches(args) }
  }

  /** Post-hoc verification of a stubbed call (ScalaMock's `(s.method _).verify(args).once()`). */
  def verify(methodName: String, expectedArgs: Any*): VerifyTarget = verifySeq(methodName, expectedArgs)

  /** [[verify]] taking the expected arguments as a `Seq` (used by the faithful `(s.method _).verify(...)` DSL macro). */
  def verifySeq(methodName: String, expectedArgs: Seq[Any]): VerifyTarget =
    new VerifyTarget(this, methodName, expectedArgs)
}
