package hearth.kindlings.mock

import scala.collection.mutable

/** Runtime engine that backs every macro-generated mock.
  *
  * A mock produced by [[Mock.mock]] overrides each abstract member of the mocked type with a body that forwards the
  * call into the [[MockContext]] that was in scope when the mock was created (`handle`). Expectations are registered up
  * front (`expecting`) and matched against incoming calls; at the end of a test [[verifyExpectations]] checks that
  * every expectation was satisfied.
  *
  * This is the cross-platform, pure-Scala (no reflection, no bytecode generation) core that works identically on the
  * JVM, Scala.js and Scala Native.
  *
  * Overloaded methods (same name, different arity) are disambiguated by the actual argument count of each call;
  * call-ordering across arbitrarily nested `inSequence`/`inAnyOrder` blocks is enforced by an ordering tree.
  */
final class MockContext {

  private val handlers: mutable.Map[String, mutable.ListBuffer[CallHandler]] = mutable.Map.empty
  private val callLog: mutable.ListBuffer[(String, Seq[Any])] = mutable.ListBuffer.empty

  // Root of the ordering tree (unordered). `currentNode` is the node new expectations attach to while building.
  private val rootNode: OrderingNode = new OrderingNode(ordered = false, parent = null)
  private var currentNode: OrderingNode = rootNode

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
  def expectingSeq(methodName: String, expectedArgs: Seq[Any]): CallHandler =
    register(methodName, expectedArgs, arity = if (expectedArgs.isEmpty) -1 else expectedArgs.length, stub = false)

  /** Faithful-DSL registration that also pins the overload arity, so an expectation matching "any args" still targets
    * the right overload. Used by the `(m.method _).expects(...)` macro.
    */
  def expectingArity(methodName: String, arity: Int, expectedArgs: Seq[Any]): CallHandler =
    register(methodName, expectedArgs, arity = arity, stub = false)

  private def register(methodName: String, expectedArgs: Seq[Any], arity: Int, stub: Boolean): CallHandler = {
    val handler = new CallHandler(methodName, if (expectedArgs.isEmpty) None else Some(expectedArgs))
    handler.arity = arity
    if (stub) { val _ = handler.anyNumberOfTimes() }
    val _ = handlers.getOrElseUpdate(methodName, mutable.ListBuffer.empty).append(handler)
    if (!stub) currentNode.addChild(Right(handler))
    handler
  }

  /** Register the expectations created inside `block` as an ordered group: calls must arrive in the order the
    * expectations were declared (mirrors ScalaMock's `inSequence`). Sequences may span several methods and may be
    * arbitrarily nested with [[inAnyOrder]].
    */
  def inSequence[T](block: => T): T = withNode(ordered = true)(block)

  /** Register the expectations created inside `block` without ordering constraints, even when nested inside an
    * [[inSequence]] block (mirrors ScalaMock's `inAnyOrder`). This is the default at the top level.
    */
  def inAnyOrder[T](block: => T): T = withNode(ordered = false)(block)

  private def withNode[T](ordered: Boolean)(block: => T): T = {
    val previous = currentNode
    val node = new OrderingNode(ordered, previous)
    previous.addChild(Left(node))
    currentNode = node
    try block
    finally currentNode = previous
  }

  /** Dispatch a call coming from a generated mock body. The `default` is the type-appropriate fallback for the method's
    * return type, returned when the matching expectation set no `returning`/`throwing`/`onCall` (mirrors ScalaMock).
    * Used by macro-generated code only.
    */
  def handle(methodName: String, args: Seq[Any], default: => Any): Any = {
    callLog.append(methodName -> args)
    val candidates = handlers.getOrElse(methodName, mutable.ListBuffer.empty)
    candidates.find(h =>
      !h.isExhausted && h.matchesArity(args.length) && OrderingNode.eligible(h) && h.matches(args)
    ) match {
      case Some(handler) => handler.call(args, default)
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
  def whenSeq(methodName: String, expectedArgs: Seq[Any]): CallHandler =
    register(methodName, expectedArgs, arity = if (expectedArgs.isEmpty) -1 else expectedArgs.length, stub = true)

  /** [[when]] that also pins the overload arity (faithful DSL). */
  def whenArity(methodName: String, arity: Int, expectedArgs: Seq[Any]): CallHandler =
    register(methodName, expectedArgs, arity = arity, stub = true)

  /** Dispatch a call from a generated `stub` body: record it, run a matching preset if any, otherwise return `default`
    * (never throws on an unexpected call). Used by macro-generated code only.
    */
  def handleStub(methodName: String, args: Seq[Any], default: => Any): Any = {
    callLog.append(methodName -> args)
    handlers
      .getOrElse(methodName, mutable.ListBuffer.empty)
      .find(h => !h.isExhausted && h.matchesArity(args.length) && h.matches(args)) match {
      case Some(handler) => handler.call(args, default)
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

  /** [[verify]] taking the expected arguments as a `Seq` (used by the faithful `(s.method _).verify(...)` DSL macro).
    */
  def verifySeq(methodName: String, expectedArgs: Seq[Any]): VerifyTarget =
    new VerifyTarget(this, methodName, expectedArgs)

  /** Post-hoc assert that the given `(methodName, expectedArgs)` calls were recorded in this exact relative order
    * (ScalaMock's stub verify-in-sequence). Calls in between are allowed; the listed calls must appear as a subsequence
    * of the recorded call log, in order. Each expected-args `Seq` may contain [[ArgMatcher]]s. Throws on a mismatch.
    */
  def verifyInSequence(calls: (String, Seq[Any])*): Unit = {
    val recorded = callLog.toList
    var cursor = 0
    calls.foreach { case (name, expectedArgs) =>
      val probe = new CallHandler(name, if (expectedArgs.isEmpty) None else Some(expectedArgs))
      val found = recorded.indexWhere({ case (n, args) => n == name && probe.matches(args) }, cursor)
      if (found < 0)
        throw new MockExpectationException(
          s"Expected call $name(${expectedArgs.mkString(", ")}) in sequence after position $cursor, " +
            s"but it was not found.\nRecorded:\n${recorded.map { case (n, a) => s"  $n(${a.mkString(", ")})" }.mkString("\n")}"
        )
      cursor = found + 1
    }
  }
}
