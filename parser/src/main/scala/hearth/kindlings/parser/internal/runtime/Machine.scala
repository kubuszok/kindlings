package hearth.kindlings.parser
package internal.runtime

import scala.collection.mutable.ListBuffer

/** A resumable, table-driven LR parser.
  *
  * The parse stack lives in arrays on the heap, so nesting depth is limited only by memory, never by the JVM thread
  * stack. [[run]] shifts and reduces until one of:
  *   - [[Machine.Done]]: the input is accepted ([[result]]),
  *   - [[Machine.Error]]: a syntax error ([[error]]),
  *   - [[Machine.Effect]]: an effectful action returned its `F[R]` ([[pendingEffect]]); the engine obtains the result
  *     in `F` and continues with [[resume]],
  *   - [[Machine.NeedInput]]: the input buffer is exhausted; the engine calls [[refill]] (which may block) and runs
  *     again,
  *   - [[Machine.Yield]]: the step budget is exhausted; the engine may yield to other tasks and runs again.
  *
  * Pure actions, lexing, shifts and reductions run inline. A machine is used by one parse only.
  */
final class Machine private[parser] (grammar: CompiledGrammar, input: Input) {

  import CompiledGrammar.*

  private val tables = grammar.tables
  private val literals = tables.literals
  private val text: String = input match {
    case s: StringInput => s.text
    case _              => null
  }

  /** Generated reductions (`Grammar.grammar`), `null` for interpreted grammars. */
  private val reductions: GeneratedReductions = grammar match {
    case g: GeneratedGrammar => g.reductions
    case _                   => null
  }

  /** The generated `String` driver (the parse loop with the lexer inlined), when the input is a `String` and the
    * grammar has one.
    */
  private val stringDriver: GeneratedReductions =
    if (text != null && reductions != null && reductions.hasStringDriver) reductions else null
  private val tokenCount = tables.tokenCount
  private val actionTable = tables.action
  private val gotoTable = tables.goto
  private val nonTerminalCount = tables.nonTerminalCount

  private var states = new Array[Int](64)
  private var values = new Array[Any](64)
  private var sp = 0 // index of the top of the stack

  private var pos = 0L
  private var lookahead = -1 // -1: not lexed yet
  private var tokenStart = 0L
  private var tokenEnd = 0L

  private var pendingLhs = -1
  private var _pendingEffect: Any = null
  private var _result: Any = null
  private var _error: ParseError = null

  /** The `F[R]` returned by the effectful action that stopped the machine ([[Machine.Effect]]). */
  def pendingEffect: Any = _pendingEffect

  /** The parse result, after [[Machine.Done]]. */
  def result: Any = _result

  /** The syntax error, after [[Machine.Error]]. */
  def error: ParseError = _error

  /** Continues after [[Machine.Effect]] with the effect's result, which becomes the value of the reduced production. */
  def resume(value: Any): Unit = {
    val lhs = pendingLhs
    pendingLhs = -1
    _pendingEffect = null
    push(tables.goto(states(sp) * nonTerminalCount + lhs), value)
  }

  /** Reads more input after [[Machine.NeedInput]] (may block). For push machines this does nothing: use [[feed]]. */
  def refill(): Unit = input.refill()

  /** Push machines (see `Parser.pushMachine`): appends a chunk of input. */
  def feed(chunk: String): Unit = input match {
    case push: PushInput => push.feed(chunk)
    case _               => throw new UnsupportedOperationException("feed is only available on push machines")
  }

  /** Push machines (see `Parser.pushMachine`): no more input will be fed. */
  def endOfInput(): Unit = input match {
    case push: PushInput => push.endOfInput()
    case _               => throw new UnsupportedOperationException("endOfInput is only available on push machines")
  }

  /** Runs until the input is accepted, a syntax error occurs, an effectful action has to be sequenced, more input is
    * needed, or `budget` shifts and reductions were made.
    */
  def run(budget: Int = Int.MaxValue): Int = {
    if (stringDriver != null) return stringDriver.runString(this, budget)
    var steps = 0
    while (steps < budget) {
      if (lookahead < 0) {
        val lexed = lex()
        if (lexed != Machine.Done) return lexed
      }
      val act = actionTable(states(sp) * tokenCount + lookahead)
      if (act > 0) {
        val literal = literals(lookahead)
        push(
          act - 1,
          if (literal != null) literal
          else if (text != null) text.substring(tokenStart.toInt, tokenEnd.toInt)
          else input.slice(tokenStart, tokenEnd)
        )
        lookahead = -1
        if (text == null) input.release(pos)
      } else if (act < 0) {
        val p = -act - 1
        if (p == 0) {
          _result = values(sp)
          return Machine.Done
        }
        try
          if (reductions != null) {
            if (sp + 1 >= states.length) grow()
            val top = reductions.reduce(p, states, values, sp, gotoTable, this)
            if (top >= 0) sp = top
            else {
              sp = -top - 1
              return Machine.Effect
            }
          } else if (reduce(p)) return Machine.Effect
        catch {
          case r: RejectedValue =>
            _error = rejected(r.getMessage)
            return Machine.Error
        }
      } else {
        _error = syntaxError("Unexpected token")
        return Machine.Error
      }
      steps += 1
    }
    Machine.Yield
  }

  // --- used by generated code (`GeneratedReductions`), not part of the driving API ---------------------------------
  // The generated `String` driver loads the machine's state into locals ([[stringText]] ... [[positionIndex]]), runs,
  // stores it back with [[restore]] and ends with [[finish]].

  def stringText: String = text
  def actionTableArray: Array[Int] = actionTable
  def gotoTableArray: Array[Int] = gotoTable
  def literalTable: Array[String] = literals
  def generatedReductions: GeneratedReductions = reductions
  def stackStates: Array[Int] = states

  /** The value stack; the value of the top is at [[stackTop]]. Values above the top may be stale. */
  def stackValues: Array[Any] = values

  /** The index of the top of the stack. */
  def stackTop: Int = sp
  def lookaheadToken: Int = lookahead
  def tokenStartIndex: Int = tokenStart.toInt
  def tokenEndIndex: Int = tokenEnd.toInt
  def positionIndex: Int = pos.toInt

  def restore(
      states: Array[Int],
      values: Array[Any],
      sp: Int,
      lookahead: Int,
      tokenStart: Int,
      tokenEnd: Int,
      pos: Int
  ): Unit = {
    this.states = states
    this.values = values
    this.sp = sp
    this.lookahead = lookahead
    this.tokenStart = tokenStart.toLong
    this.tokenEnd = tokenEnd.toLong
    this.pos = pos.toLong
  }

  /** Doubles the stacks (after [[restore]]; reload them with [[stackStates]] / [[stackValues]]). */
  def grow(): Unit = {
    states = java.util.Arrays.copyOf(states, states.length * 2)
    values = java.util.Arrays.copyOf(values.asInstanceOf[Array[AnyRef]], values.length * 2).asInstanceOf[Array[Any]]
  }

  /** An effectful action's reduction: the machine will stop with `effect` ([[Machine.Effect]]) and [[resume]] pushes
    * its result in the goto state of `lhs`.
    */
  def suspendEffect(lhs: Int, effect: Any): Unit = {
    _pendingEffect = effect
    pendingLhs = lhs
  }

  /** Ends a run of the generated driver (after [[restore]]): turns its `status` (a machine signal or one of
    * `Machine.Accept`, `Machine.Unexpected`, `Machine.LexFailure`, `Machine.Rejected`) into the signal to return.
    */
  def finish(status: Int, message: String): Int = status match {
    case Machine.Accept =>
      _result = values(sp)
      Machine.Done
    case Machine.Unexpected =>
      _error = syntaxError("Unexpected token")
      Machine.Error
    case Machine.LexFailure =>
      tokenEnd = tokenStart + 1
      pos = tokenStart
      _error = syntaxError("Unexpected character")
      Machine.Error
    case Machine.Rejected =>
      _error = rejected(message)
      Machine.Error
    case other => other
  }

  private def push(state: Int, value: Any): Unit = {
    sp += 1
    if (sp == states.length) grow()
    states(sp) = state
    values(sp) = value
  }

  /** Reduces production `p`; returns `true` if an effectful action stopped the machine. */
  private def reduce(p: Int): Boolean = {
    val len = tables.prodLen(p)
    val lhs = tables.prodLhs(p)
    val base = sp - len + 1
    val kind = tables.prodKind(p)
    val value: Any = (kind: @scala.annotation.switch) match {
      case ActPure | ActEffect => grammar.userAction(p, values, base)
      case ActPass             => grammar.argument(p, 0, values(base))
      case ActConst            => tables.constants(tables.prodArg(p))
      case ActOptNone          => None
      case ActOptSome          => Some(grammar.argument(p, 0, values(base)))
      case ActListEmpty        => ListBuffer.empty[Any]
      case ActListOne          => ListBuffer[Any](grammar.argument(p, 0, values(base)))
      case _                   =>
        val index = tables.prodArg(p)
        values(base).asInstanceOf[ListBuffer[Any]] += grammar.argument(p, index, values(base + index))
    }
    var i = base
    while (i <= sp) { values(i) = null; i += 1 }
    sp -= len
    if (kind == ActEffect) {
      _pendingEffect = value
      pendingLhs = lhs
      true
    } else {
      push(tables.goto(states(sp) * nonTerminalCount + lhs), value)
      false
    }
  }

  /** Reads the next non-skipped token into `lookahead`: [[Machine.Done]] on success, [[Machine.Error]] on a lexical
    * error, [[Machine.NeedInput]] if the input buffer ran out in the middle of a token (lexing restarts from the token
    * start after a refill).
    */
  private def lex(): Int = if (text != null) lexString() else lexInput()

  /** [[lex]] specialised for `String` inputs: direct `charAt`, no `Input` calls. */
  private def lexString(): Int = {
    val ascii = tables.ascii
    val accept = tables.lexAccept
    val skip = tables.skip
    val selfLoop = tables.selfLoop
    val hasSelfLoop = tables.hasSelfLoop
    val length = text.length
    var p = pos.toInt
    while (true) {
      if (p >= length) {
        lookahead = 0
        tokenStart = p.toLong
        tokenEnd = p.toLong
        pos = p.toLong
        return Machine.Done
      }
      var state = 0
      var i = p
      var accepted = -1
      var acceptedEnd = p
      while (state >= 0 && i < length) {
        val c = text.charAt(i)
        state = if (c < 128) ascii(state * 128 + c) else tables.lexStep(state, c)
        if (state >= 0) {
          i += 1
          if (hasSelfLoop(state)) {
            val row = state * 128
            var continue = true
            while (continue && i < length) {
              val d = text.charAt(i)
              if (d < 128 && selfLoop(row + d)) i += 1 else continue = false
            }
          }
          val a = accept(state)
          if (a >= 0) { accepted = a; acceptedEnd = i }
        }
      }
      if (accepted < 0) {
        pos = p.toLong
        tokenStart = p.toLong
        tokenEnd = p.toLong + 1
        _error = syntaxError("Unexpected character")
        return Machine.Error
      }
      if (skip(accepted)) p = acceptedEnd
      else {
        lookahead = accepted
        tokenStart = p.toLong
        tokenEnd = acceptedEnd.toLong
        pos = acceptedEnd.toLong
        return Machine.Done
      }
    }
    Machine.Error
  }

  private def lexInput(): Int = {
    while (true) {
      input.ensure(pos) match {
        case Input.End =>
          lookahead = 0
          tokenStart = pos
          tokenEnd = pos
          return Machine.Done
        case Input.NeedMore => return Machine.NeedInput
        case _              => ()
      }
      var state = 0
      var i = pos
      var accepted = -1
      var acceptedEnd = pos
      var scanning = true
      while (scanning && state >= 0)
        input.ensure(i) match {
          case Input.Available =>
            state = tables.lexStep(state, input.charAt(i))
            if (state >= 0) {
              i += 1
              val a = tables.lexAccept(state)
              if (a >= 0) { accepted = a; acceptedEnd = i }
            }
          case Input.NeedMore => return Machine.NeedInput
          case _              => scanning = false
        }
      if (accepted < 0) {
        tokenStart = pos
        tokenEnd = pos + 1
        _error = syntaxError("Unexpected character")
        return Machine.Error
      }
      if (tables.skip(accepted)) {
        pos = acceptedEnd
        input.release(pos)
      } else {
        lookahead = accepted
        tokenStart = pos
        tokenEnd = acceptedEnd
        pos = acceptedEnd
        return Machine.Done
      }
    }
    Machine.Error
  }

  /** A value rejected by a reduction (see [[RejectedValue]]), reported at the current token. */
  private def rejected(detail: String): ParseError = {
    val found =
      if (lookahead == 0 || input.ensure(tokenStart) != Input.Available) "end of input"
      else Machine.quote(input.slice(tokenStart, math.max(tokenEnd, tokenStart + 1)))
    val (line, column) = input.lineColumn(tokenStart)
    new ParseError(tokenStart, line, column, Nil, found, detail, endOfInput = false)
  }

  private def syntaxError(detail: String): ParseError = {
    val state = states(sp)
    val expected = (0 until tokenCount)
      .filter { t =>
        !tables.skip(t) && tables.action(state * tokenCount + t) != 0
      }
      .map(tables.tokenNames(_))
      .sorted
      .toList
    val atEnd = lookahead == 0 && detail == "Unexpected token"
    val found =
      if (atEnd || input.ensure(tokenStart) != Input.Available) "end of input"
      else {
        val end = if (input.ensure(tokenEnd - 1) == Input.Available) tokenEnd else tokenStart + 1
        val text = Machine.quote(input.slice(tokenStart, end))
        if (lookahead > 0 && detail == "Unexpected token" && tables.tokenNames(lookahead) != text)
          s"${tables.tokenNames(lookahead)} $text"
        else text
      }
    val (line, column) = input.lineColumn(tokenStart)
    new ParseError(tokenStart, line, column, expected, found, detail, atEnd)
  }
}
object Machine {

  final val Done = 0
  final val Error = 1
  final val Effect = 2
  final val NeedInput = 3
  final val Yield = 4

  // statuses of the generated driver, turned into the signals above by `finish`
  final val Accept = 5
  final val Unexpected = 6
  final val LexFailure = 7
  final val Rejected = 8

  private[parser] def quote(text: String): String = {
    val sb = new StringBuilder("\"")
    text.foreach {
      case '"'  => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case c    => sb.append(c)
    }
    sb.append('"').toString
  }
}
