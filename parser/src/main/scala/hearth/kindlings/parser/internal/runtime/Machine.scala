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
  private val tokenCount = tables.tokenCount
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
    var steps = 0
    while (steps < budget) {
      if (lookahead < 0) {
        val lexed = lex()
        if (lexed != Machine.Done) return lexed
      }
      val act = tables.action(states(sp) * tokenCount + lookahead)
      if (act > 0) {
        push(act - 1, input.slice(tokenStart, tokenEnd))
        lookahead = -1
        input.release(pos)
      } else if (act < 0) {
        val p = -act - 1
        if (p == 0) {
          _result = values(sp)
          return Machine.Done
        }
        if (reduce(p)) return Machine.Effect
      } else {
        _error = syntaxError("Unexpected token")
        return Machine.Error
      }
      steps += 1
    }
    Machine.Yield
  }

  private def push(state: Int, value: Any): Unit = {
    sp += 1
    if (sp == states.length) {
      states = java.util.Arrays.copyOf(states, states.length * 2)
      values = java.util.Arrays.copyOf(values.asInstanceOf[Array[AnyRef]], values.length * 2).asInstanceOf[Array[Any]]
    }
    states(sp) = state
    values(sp) = value
  }

  /** Reduces production `p`; returns `true` if an effectful action stopped the machine. */
  private def reduce(p: Int): Boolean = {
    val len = tables.prodLen(p)
    val lhs = tables.prodLhs(p)
    val converters = grammar.converters(p)
    val args = new Array[Any](len)
    val base = sp - len + 1
    var i = 0
    while (i < len) {
      val raw = values(base + i)
      args(i) = converters(i) match {
        case null    => raw
        case Listify => listify(raw)
        case convert => convert.asInstanceOf[String => Any](raw.asInstanceOf[String])
      }
      values(base + i) = null
      i += 1
    }
    sp -= len
    (grammar.actionKind(p): @scala.annotation.switch) match {
      case ActPure   => push(tables.goto(states(sp) * nonTerminalCount + lhs), grammar.actionFn(p)(args)); false
      case ActEffect =>
        _pendingEffect = grammar.actionFn(p)(args)
        pendingLhs = lhs
        true
      case ActPass      => push(tables.goto(states(sp) * nonTerminalCount + lhs), args(0)); false
      case ActConst     => push(tables.goto(states(sp) * nonTerminalCount + lhs), grammar.actionArg(p)); false
      case ActOptNone   => push(tables.goto(states(sp) * nonTerminalCount + lhs), None); false
      case ActOptSome   => push(tables.goto(states(sp) * nonTerminalCount + lhs), Some(args(0))); false
      case ActListEmpty => push(tables.goto(states(sp) * nonTerminalCount + lhs), ListBuffer.empty[Any]); false
      case ActListOne   => push(tables.goto(states(sp) * nonTerminalCount + lhs), ListBuffer[Any](args(0))); false
      case _            =>
        val buffer = args(0).asInstanceOf[ListBuffer[Any]]
        buffer += args(grammar.actionArg(p).asInstanceOf[Int])
        push(tables.goto(states(sp) * nonTerminalCount + lhs), buffer)
        false
    }
  }

  /** Reads the next non-skipped token into `lookahead`: [[Machine.Done]] on success, [[Machine.Error]] on a lexical
    * error, [[Machine.NeedInput]] if the input buffer ran out in the middle of a token (lexing restarts from the token
    * start after a refill).
    */
  private def lex(): Int = {
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
