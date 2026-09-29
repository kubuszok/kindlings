package hearth.kindlings.parser
package internal.runtime

import scala.collection.mutable.ListBuffer

/** A resumable, table-driven LR parser over a `String`.
  *
  * The parse stack lives in arrays on the heap, so nesting depth is limited only by memory, never by the JVM thread
  * stack. [[run]] shifts and reduces until the input is accepted ([[Machine.Done]]), a syntax error is found
  * ([[Machine.Error]]) or an effectful action returns its `F[R]` ([[Machine.Effect]]): the engine then obtains the
  * action's result in `F` and continues with [[resume]]. Pure actions run inline.
  */
final class Machine private[parser] (grammar: CompiledGrammar, input: String) {

  import CompiledGrammar.*

  private val tables = grammar.tables
  private val tokenCount = tables.tokenCount
  private val nonTerminalCount = tables.nonTerminalCount
  private val length = input.length

  private var states = new Array[Int](64)
  private var values = new Array[Any](64)
  private var sp = 0 // index of the top of the stack

  private var pos = 0
  private var lookahead = -1 // -1: not lexed yet
  private var tokenStart = 0
  private var tokenEnd = 0

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

  /** Runs until the input is accepted, a syntax error occurs, or an effectful action has to be sequenced. */
  def run(): Int = {
    while (true) {
      if (lookahead < 0 && !lex()) return Machine.Error
      val act = tables.action(states(sp) * tokenCount + lookahead)
      if (act > 0) {
        push(act - 1, input.substring(tokenStart, tokenEnd))
        lookahead = -1
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
    }
    Machine.Error
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

  /** Reads the next non-skipped token into `lookahead`; returns `false` on a lexical error. */
  private def lex(): Boolean = {
    while (true) {
      if (pos >= length) {
        lookahead = 0
        tokenStart = pos
        tokenEnd = pos
        return true
      }
      var state = 0
      var i = pos
      var accepted = -1
      var acceptedEnd = pos
      while (state >= 0 && i < length) {
        state = tables.lexStep(state, input.charAt(i))
        if (state >= 0) {
          i += 1
          val a = tables.lexAccept(state)
          if (a >= 0) { accepted = a; acceptedEnd = i }
        }
      }
      if (accepted < 0) {
        tokenStart = pos
        tokenEnd = pos + 1
        _error = syntaxError("Unexpected character")
        return false
      }
      if (tables.skip(accepted)) pos = acceptedEnd
      else {
        lookahead = accepted
        tokenStart = pos
        tokenEnd = acceptedEnd
        pos = acceptedEnd
        return true
      }
    }
    false
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
    val found =
      if (tokenStart >= length) "end of input"
      else {
        val text = Machine.quote(input.substring(tokenStart, math.min(tokenEnd, length)))
        if (lookahead > 0 && detail == "Unexpected token" && tables.tokenNames(lookahead) != text)
          s"${tables.tokenNames(lookahead)} $text"
        else text
      }
    var line = 1
    var lineStart = 0
    var i = 0
    while (i < tokenStart && i < length) {
      if (input.charAt(i) == '\n') { line += 1; lineStart = i + 1 }
      i += 1
    }
    new ParseError(tokenStart, line, tokenStart - lineStart + 1, expected, found, detail)
  }
}
object Machine {

  final val Done = 0
  final val Error = 1
  final val Effect = 2

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
