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
  private val sliced = tables.sliced
  private val text: String = input match {
    case s: StringInput => s.text
    case _              => null
  }

  /** Generated reductions (`Grammar.grammar`), `null` for interpreted grammars. */
  private val reductions: GeneratedReductions = grammar match {
    case g: GeneratedGrammar => g.reductions
    case _                   => null
  }

  /** The generated `String` lexer, when the input is a `String` and the grammar has one. */
  private val stringLexer: GeneratedReductions =
    if (text != null && reductions != null && reductions.hasStringLexer) reductions else null

  /** Parsing with the generated LL(1) program (`String` inputs of LL(1) grammars) instead of the LALR tables. */
  private val llMode: Boolean = text != null && reductions != null && reductions.hasLL && tables.hasLL
  private var llState: Int = tables.llStart
  private var frames = new Array[Int](64)
  private var fp = -1

  /** Whether the generated recursive-descent parser is tried first (`String` inputs of LL(1) grammars, until the first
    * `run`).
    */
  private var descentPending: Boolean = text != null && reductions != null && reductions.hasDescent

  /** Whether the result came from the recursive-descent parser (for tests). */
  private[parser] var descended: Boolean = false

  /** Parses with the machine only (for tests comparing both parsers). */
  private[parser] def skipDescent(): Unit = descentPending = false

  private val simpleSkip = tables.simpleSkip
  private val tokenCount = tables.tokenCount
  private val actionTable = tables.action
  private val nonTerminalCount = tables.nonTerminalCount

  private var states = new Array[Int](64)
  private var values = new Array[Any](64)
  private var prims = new Array[Long](64)
  private val ntPrim = tables.ntPrim
  private var sp = 0 // index of the top of the stack

  private var pos = 0L
  private var lookahead = -1 // -1: not lexed yet
  private var tokenStart = 0L
  private var tokenEnd = 0L

  // Retain a buffered token's DFA continuation across refills, including its last accept for maximal-munch rollback.
  private var lexing = false
  private var savedLexState = 0
  private var savedLexPos = 0L
  private var savedLexAccepted = -1
  private var savedLexAcceptedEnd = 0L

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
    val kind = ntPrim(lhs)
    val state = if (llMode) 0 else tables.goto(states(sp) * nonTerminalCount + lhs)
    if (kind == Prims.Boxed) push(state, value)
    else {
      push(state, null)
      prims(sp) = Prims.encode(kind, value)
    }
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
    * needed, or `budget` shifts and reductions (LL mode: program steps) were made.
    */
  def run(budget: Int = Int.MaxValue): Int = {
    require(budget > 0, "parser step budget must be positive")
    if (descentPending && budget == Int.MaxValue) {
      descentPending = false
      try {
        _result = reductions.descend(this, text)
        descended = true
        return Machine.Done
      } catch {
        // syntax errors, rejected values and deep nesting are left to the machine; so are exceptions thrown by
        // actions, since the machine may report a syntax error before running the action that threw
        case e: Throwable if e == DescentBail || scala.util.control.NonFatal(e) || e.isInstanceOf[StackOverflowError] =>
          pos = 0L
          lookahead = -1
          tokenStart = 0L
          tokenEnd = 0L
          _error = null
      }
    }
    descentPending = false
    if (llMode) return reductions.runLL(this, budget)
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
          else if (sliced(lookahead)) {
            if (text != null) reductions.slice(lookahead, text, tokenStart.toInt, tokenEnd.toInt)
            else {
              val token = input.slice(tokenStart, tokenEnd)
              reductions.slice(lookahead, token, 0, token.length)
            }
          } else if (text != null) text.substring(tokenStart.toInt, tokenEnd.toInt)
          else input.slice(tokenStart, tokenEnd)
        )
        lookahead = -1
        if (text == null) input.release(pos)
      } else if (act < 0) {
        val p = -act - 1
        if (p == 0) {
          val kind = ntPrim(nonTerminalCount - 1)
          _result = if (kind == Prims.Boxed) values(sp) else Prims.decode(kind, prims(sp))
          return Machine.Done
        }
        try
          if (reductions != null) {
            if (reductions.reduce(p, this)) return Machine.Effect
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

  /** The value stack; the value of the top is at [[stackTop]]. Values above the top may be stale. */
  def stackValues: Array[Any] = values

  /** The primitive stack: the bits of the values of non-terminals with a primitive type (see [[Prims]]). */
  def stackPrims: Array[Long] = prims

  /** The index of the top of the stack. */
  def stackTop: Int = sp

  /** Completes a reduction of a non-terminal with a primitive type: its value's `bits` go to the primitive stack. */
  def reducedPrim(top: Int, lhs: Int, bits: Long): Unit = {
    sp = top
    push(if (llMode) 0 else tables.goto(states(top) * nonTerminalCount + lhs), null)
    prims(sp) = bits
  }

  /** [[reducedPrim]] whose goto state is always `state`. */
  def reducedToPrim(top: Int, state: Int, bits: Long): Unit = {
    sp = top
    push(state, null)
    prims(sp) = bits
  }

  /** Completes a reduction: the stack is cut back to `top` and `value` is pushed in the goto state of `lhs` (LL mode:
    * just pushed).
    */
  def reduced(top: Int, lhs: Int, value: Any): Unit = {
    sp = top
    if (llMode) pushValue(value) else push(tables.goto(states(top) * nonTerminalCount + lhs), value)
  }

  /** Completes a reduction whose goto state is always `state`. */
  def reducedTo(top: Int, state: Int, value: Any): Unit = {
    sp = top
    if (llMode) pushValue(value) else push(state, value)
  }

  /** Completes the reduction of an effectful action: the machine stops with `effect` ([[Machine.Effect]]) and
    * [[resume]] pushes its result in the goto state of `lhs`.
    */
  def suspend(top: Int, lhs: Int, effect: Any): Unit = {
    sp = top
    _pendingEffect = effect
    pendingLhs = lhs
  }

  /** The lexer found token `id` at `[start, end)`. */
  def token(id: Int, start: Int, end: Int): Unit = {
    lookahead = id
    tokenStart = start.toLong
    tokenEnd = end.toLong
    pos = end.toLong
  }

  // --- used by the generated recursive-descent parser (`GeneratedReductions.descend`) -----------------------------

  private var cursor = 0
  private val textLength = if (text != null) text.length else 0
  private var lexedAt = -1
  private var depth = 0

  /** Where the recursive-descent parser is in the text. */
  def descentPos: Int = cursor

  /** Moves the recursive-descent parser to `p`. */
  def descentMoveTo(p: Int): Unit = cursor = p

  /** Skips simple skipped text (whitespace) and returns the next char, `-1` at the end of the text. */
  def descentPeek(): Int = {
    val t = text
    var p = cursor
    if (p >= textLength) -1
    else {
      var c = t.charAt(p)
      while (c < 128 && simpleSkip(c.toInt)) {
        p += 1
        c = if (p < textLength) t.charAt(p) else 0xffff
      }
      cursor = p
      if (p < textLength) c.toInt else -1
    }
  }

  /** Lexes the token at the cursor (once per position) and returns its id. The cursor moves to the token's start (past
    * skipped text such as comments).
    */
  def descentLex(): Int = {
    if (lexedAt != cursor) {
      if (reductions.lexString(this, text, cursor) != Machine.Done) throw DescentBail
      cursor = tokenStart.toInt
      lexedAt = cursor
    }
    lookahead
  }

  /** Skips simple skipped text (whitespace) and returns the cursor: where a token scanner starts. */
  def descentScanStart(): Int = {
    val _ = descentPeek()
    cursor
  }

  /** A generated token scanner read the token at `[start, end)` ([[tokenStartIndex]], [[tokenEndIndex]]). */
  def descentTokenAt(start: Int, end: Int): Unit = {
    tokenStart = start.toLong
    tokenEnd = end.toLong
    cursor = end
  }

  /** Consumes the `n`-char literal a decision found at the cursor. */
  def descentSkip(n: Int): Unit = cursor += n

  /** Reads the literal `token` = `word` whose first char a decision found at the cursor. */
  def descentRest(word: String, token: Int): Unit =
    if (restMatches(word)) cursor += word.length else descentToken(token)

  /** Whether the text at the cursor continues with `word` after its first char (short words: a plain loop is faster
    * than `startsWith`).
    */
  private def restMatches(word: String): Boolean = {
    val t = text
    val n = word.length
    val at = cursor
    if (at + n > t.length) false
    else {
      var i = 1
      while (i < n && t.charAt(at + i) == word.charAt(i)) i += 1
      i == n
    }
  }

  /** Reads token `token` with the lexer ([[tokenStartIndex]], [[tokenEndIndex]]); gives up on another token. */
  def descentToken(token: Int): Unit = {
    if (descentLex() != token) throw DescentBail
    cursor = tokenEnd.toInt
  }

  /** Reads the one-char literal `token` = `c` (no other token starts with `c`). */
  def descentChar(c: Char, token: Int): Unit =
    if (descentPeek() == c) cursor += 1 else descentToken(token)

  /** Reads the literal `token` = `word` (no other token starts with its first char). */
  def descentWord(word: String, token: Int): Unit =
    if (descentPeek() == word.charAt(0) && restMatches(word)) cursor += word.length
    else descentToken(token)

  /** The whole text must have been read. */
  def descentEnd(): Unit =
    if (descentPeek() >= 0 && descentLex() != 0) throw DescentBail

  /** Enters a recursive non-terminal; gives up beyond [[Machine.MaxDescentDepth]] (the machine has no depth limit). */
  def descentEnter(): Unit = {
    depth += 1
    if (depth > Machine.MaxDescentDepth) throw DescentBail
  }

  /** Leaves a recursive non-terminal. */
  def descentExit(): Unit = depth -= 1

  /** Gives up: the machine parses the input instead. */
  def descentFail(): Nothing = throw DescentBail

  // --- used by the generated LL(1) program (`GeneratedReductions.runLL`) ------------------------------------------

  /** The generated code of the grammar (the LL program calls its `reduce`). */
  def generatedReductions: GeneratedReductions = reductions

  /** The LL state to continue at (after an effect or a yield). */
  def llResumeState: Int = llState

  /** Records the LL state to continue at. */
  def llSuspendAt(state: Int): Unit = llState = state

  /** The next token, or `-1` if it was not read yet. */
  def lookaheadToken: Int = lookahead

  /** Where the last token read starts. */
  def tokenStartIndex: Int = tokenStart.toInt

  /** Where the last token read ends (and lexing continues). */
  def tokenEndIndex: Int = tokenEnd.toInt

  /** Reads the next token (in LL state `state`, for error messages): `Machine.Done` or `Machine.Error`. */
  def readToken(state: Int): Int = {
    llState = state
    lex()
  }

  /** Pushes the value of the next token (already read) and consumes it. */
  def shiftToken(): Unit = {
    val literal = literals(lookahead)
    pushValue(
      if (literal != null) literal
      else if (sliced(lookahead)) reductions.slice(lookahead, text, tokenStart.toInt, tokenEnd.toInt)
      else text.substring(tokenStart.toInt, tokenEnd.toInt)
    )
    lookahead = -1
  }

  /** Fast path for a single-char literal token `token` = `c` that no longer token can start with: skips simple skipped
    * text (whitespace) and, if the next char is `c`, pushes the literal and returns `true`. Otherwise returns `false`,
    * and the token is read by the lexer.
    */
  def expectChar(c: Char, token: Int): Boolean = {
    val t = text
    val length = t.length
    var p = pos.toInt
    while (p < length && { val d = t.charAt(p); d < 128 && simpleSkip(d.toInt) }) p += 1
    pos = p.toLong
    if (p < length && t.charAt(p) == c) {
      tokenStart = p.toLong
      pos = p.toLong + 1
      tokenEnd = pos
      pushValue(literals(token))
      true
    } else false
  }

  def pushFrame(state: Int): Unit = {
    fp += 1
    if (fp == frames.length) frames = java.util.Arrays.copyOf(frames, frames.length * 2)
    frames(fp) = state
  }

  def popFrame(): Int = {
    val state = frames(fp)
    fp -= 1
    state
  }

  /** The next token does not fit LL state `state`: records the syntax error, returns `Machine.Error`. */
  def llUnexpected(state: Int): Int = {
    llState = state
    _error = syntaxError("Unexpected token")
    Machine.Error
  }

  /** The input was accepted: records the result, returns `Machine.Done`. */
  def llAccept(): Int = {
    val kind = ntPrim(nonTerminalCount - 1)
    _result = if (kind == Prims.Boxed) values(sp) else Prims.decode(kind, prims(sp))
    Machine.Done
  }

  /** A value was rejected by a reduction: records the error, returns `Machine.Error`. */
  def llRejected(message: String): Int = {
    _error = rejected(message)
    Machine.Error
  }

  /** No token matches at `start`. */
  def lexError(start: Int): Unit = lexicalError(start.toLong)

  /** LL mode: pushes a value (the LR state stack is not used). */
  private def pushValue(value: Any): Unit = {
    sp += 1
    if (sp == values.length) grow()
    values(sp) = value
  }

  private def push(state: Int, value: Any): Unit = {
    sp += 1
    if (sp == states.length) grow()
    states(sp) = state
    values(sp) = value
  }

  /** Doubles the stacks. */
  private def grow(): Unit = {
    states = java.util.Arrays.copyOf(states, states.length * 2)
    values = java.util.Arrays.copyOf(values.asInstanceOf[Array[AnyRef]], values.length * 2).asInstanceOf[Array[Any]]
    prims = java.util.Arrays.copyOf(prims, prims.length * 2)
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
    * error, [[Machine.NeedInput]] if the input buffer ran out in the middle of a token (lexing resumes after a refill).
    */
  private def lex(): Int =
    if (stringLexer != null) stringLexer.lexString(this, text, pos.toInt)
    else if (text != null) lexString()
    else lexInput()

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
        lexicalError(p.toLong)
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
      var state = if (lexing) savedLexState else 0
      var i = if (lexing) savedLexPos else pos
      var accepted = if (lexing) savedLexAccepted else -1
      var acceptedEnd = if (lexing) savedLexAcceptedEnd else pos
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
          case Input.NeedMore =>
            lexing = true
            savedLexState = state
            savedLexPos = i
            savedLexAccepted = accepted
            savedLexAcceptedEnd = acceptedEnd
            return Machine.NeedInput
          case _ => scanning = false
        }
      lexing = false
      if (accepted < 0) {
        lexicalError(pos)
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

  /** Error-only path shared by generated, table and buffered lexers. Re-scan a rejected lexeme to distinguish a dead
    * transition from EOF in a viable token prefix. No action or conversion is evaluated by this diagnostic check.
    */
  private def lexicalError(start: Long): Unit = {
    var state = 0
    var end = start
    while (state >= 0 && input.ensure(end) == Input.Available) {
      state = tables.lexStep(state, input.charAt(end))
      if (state >= 0) end += 1
    }
    val incomplete = state >= 0 && end > start && input.ensure(end) == Input.End && canCompleteToken(state)
    pos = start
    lookahead = -1
    tokenStart = if (incomplete) end else start
    tokenEnd = if (incomplete) end else start + 1
    _error = syntaxError(if (incomplete) "Incomplete token" else "Unexpected character", incomplete)
  }

  /** Whether appending input can finish a token usable here (or a skipped token). Checking the LR reductions on a copy
    * of the state stack avoids mistaking an unrelated incomplete token for a valid parse prefix.
    */
  private def canCompleteToken(from: Int): Boolean = {
    def accepts(token: Int): Boolean =
      if (llMode) tables.llExpected(llState).contains(token)
      else {
        @scala.annotation.tailrec
        def advance(stack: List[Int]): Boolean = {
          val action = actionTable(stack.head * tokenCount + token)
          if (action > 0) true
          else if (action == 0) false
          else {
            val production = -action - 1
            if (production == 0) token == 0
            else {
              val rest = stack.drop(tables.prodLen(production))
              val next = tables.goto(rest.head * nonTerminalCount + tables.prodLhs(production))
              advance(next :: rest)
            }
          }
        }
        advance(states.take(sp + 1).reverse.toList)
      }

    val viable = (0 until tokenCount).filter(t => !tables.skip(t) && accepts(t)).toSet
    val seen = scala.collection.mutable.BitSet.empty
    var pending = List(from)
    while (pending.nonEmpty) {
      val state = pending.head
      pending = pending.tail
      if (seen.add(state)) {
        val token = tables.lexAccept(state)
        if (token >= 0 && (viable(token) || (tables.skip(token) && viable.nonEmpty))) return true
        var transition = tables.transStart(state)
        while (transition < tables.transStart(state + 1)) {
          pending = tables.transTarget(transition) :: pending
          transition += 1
        }
      }
    }
    false
  }

  /** A value rejected by a reduction (see [[RejectedValue]]), reported at the current token. */
  private def rejected(detail: String): ParseError = {
    val found =
      if (lookahead == 0 || input.ensure(tokenStart) != Input.Available) "end of input"
      else Machine.quote(input.slice(tokenStart, math.max(tokenEnd, tokenStart + 1)))
    val (line, column) = input.lineColumn(tokenStart)
    new ParseError(tokenStart, line, column, Nil, found, detail, endOfInput = false)
  }

  private def syntaxError(detail: String, incompleteToken: Boolean = false): ParseError = {
    val state = states(sp)
    val expected = (if (llMode) tables.llExpected(llState) else (0 until tokenCount))
      .filter { t =>
        !tables.skip(t) && (llMode || tables.action(state * tokenCount + t) != 0)
      }
      .map(tables.tokenNames(_))
      .sorted
      .toList
    val atEnd = incompleteToken || (lookahead == 0 && detail == "Unexpected token")
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

  /** The nesting of recursive non-terminals up to which the generated recursive-descent parser runs on the JVM stack;
    * deeper inputs are parsed by the machine.
    */
  final val MaxDescentDepth = 1000

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
