package hearth.kindlings.parser
package internal.runtime

/** The compile-time computed tables of a grammar.
  *
  * Tokens: id 0 is end of input; ids `1 until tokenCount` are terminals (literals first, then regexes, then skipped
  * patterns). Lexer: a DFA over UTF-16 code units, state 0 is the start state; transitions of state `s` are
  * `transLo/transHi/transTarget` in `transStart(s) until transStart(s + 1)`. LR: `action(state * tokenCount + token)`
  * is `0` (error), `n > 0` (shift to state `n - 1`) or `n < 0` (reduce production `-n - 1`; production 0 is the
  * augmented start production, whose reduction accepts). `goto(state * nonTerminalCount + nt)` is the target state.
  *
  * Productions: `prodKind(p)` is one of the `CompiledGrammar.Act*` codes, `prodArg(p)` its argument (the index into
  * `constants` of an `ActConst`, the element index of an `ActListAppend`); they drive interpreted grammars, generated
  * ones have the code of each reduction generated instead.
  *
  * `ntPrim(nt)`: the [[Prims]] kind of non-terminal `nt`'s values (generated grammars keep primitive values unboxed in
  * `Machine.stackPrims`); the last entry is the augmented start symbol's, i.e. the parse result's.
  *
  * `sliced(token)`: the token's value is computed at shift time by the generated `GeneratedReductions.slice` (terminals
  * with `mapSlice` in generated grammars).
  *
  * `literals(token)` is the text of literal tokens (whose value is that constant, so it is not copied from the input)
  * and `null` for regex tokens.
  */
final private[parser] class Tables(
    val tokenCount: Int,
    val tokenNames: Array[String],
    val skip: Array[Boolean],
    val lexAccept: Array[Int],
    val transStart: Array[Int],
    val transLo: Array[Int],
    val transHi: Array[Int],
    val transTarget: Array[Int],
    val stateCount: Int,
    val nonTerminalCount: Int,
    val action: Array[Int],
    val goto: Array[Int],
    val prodLhs: Array[Int],
    val prodLen: Array[Int],
    val prodKind: Array[Int],
    val prodArg: Array[Int],
    val ntPrim: Array[Int],
    val constants: Array[String],
    val literals: Array[String],
    val sliced: Array[Boolean],
    val llStart: Int,
    val llExpectStart: Array[Int],
    val llExpect: Array[Int]
) {

  /** Whether the LL(1) program of the generated code exists (`llStart` is its start state). */
  def hasLL: Boolean = llStart >= 0

  /** The tokens LL state `s` accepts (for syntax errors). */
  def llExpected(s: Int): Seq[Int] = llExpect.slice(llExpectStart(s), llExpectStart(s + 1)).toSeq

  /** `simpleSkip(c)`: the ASCII char `c` starts a skipped token made only of such chars, which ends at the first other
    * char (e.g. `[ \t\r\n]+`), so runs of them can be skipped without the lexer.
    */
  lazy val simpleSkip: Array[Boolean] = Array.tabulate(128) { c =>
    val s = ascii(c) // from the start state
    s >= 0 && lexAccept(s) >= 0 && skip(lexAccept(s)) && {
      val targets = (transStart(s) until transStart(s + 1)).map(transTarget(_))
      targets.forall(_ == s) && (0 until 128).forall(d => ascii(d) != s || ascii(s * 128 + d) == s)
    }
  }

  /** `ascii(state * 128 + c)`: transition target on ASCII chars, `-1` if none. */
  val ascii: Array[Int] = {
    val lexStates = lexAccept.length
    val table = Array.fill(lexStates * 128)(-1)
    var s = 0
    while (s < lexStates) {
      var i = transStart(s)
      while (i < transStart(s + 1)) {
        var c = transLo(i)
        val hi = math.min(transHi(i), 127)
        while (c <= hi) { table(s * 128 + c) = transTarget(i); c += 1 }
        i += 1
      }
      s += 1
    }
    table
  }

  /** `selfLoop(state * 128 + c)`: the ASCII char `c` keeps the DFA in `state` (lets the lexer scan runs of such chars -
    * string bodies, whitespace, digits - without per-char transitions and accept checks).
    */
  val selfLoop: Array[Boolean] = {
    val table = new Array[Boolean](ascii.length)
    var i = 0
    while (i < ascii.length) {
      table(i) = ascii(i) == i / 128
      i += 1
    }
    table
  }

  /** Whether `state` has any ASCII self-loop. */
  val hasSelfLoop: Array[Boolean] = Array.tabulate(lexAccept.length) { s =>
    (0 until 128).exists(c => selfLoop(s * 128 + c))
  }

  def lexStep(state: Int, c: Char): Int =
    if (c < 128) ascii(state * 128 + c)
    else {
      var lo = transStart(state)
      var hi = transStart(state + 1) - 1
      while (lo <= hi) {
        val mid = (lo + hi) >>> 1
        if (c < transLo(mid)) hi = mid - 1
        else if (c > transHi(mid)) lo = mid + 1
        else return transTarget(mid)
      }
      -1
    }

  def encode: TableCodec.Writer = {
    val w = new TableCodec.Writer
    w.int(Tables.Version)
    w.int(tokenCount)
    tokenNames.foreach(w.string)
    w.ints(skip.map(b => if (b) 1 else 0))
    List(lexAccept, transStart, transLo, transHi, transTarget).foreach(w.ints)
    w.int(stateCount)
    w.int(nonTerminalCount)
    List(action, goto, prodLhs, prodLen, prodKind, prodArg, ntPrim).foreach(w.ints)
    w.int(constants.length)
    constants.foreach(w.string)
    w.int(literals.length)
    literals.foreach(l => if (l == null) w.int(-1) else { w.int(0); w.string(l) })
    w.ints(sliced.map(b => if (b) 1 else 0))
    w.int(llStart)
    w.ints(llExpectStart)
    w.ints(llExpect)
    w
  }
}
private[parser] object Tables {

  val Version: Int = 8

  def decode(text: String): Tables = {
    val r = new TableCodec.Reader(text)
    val version = r.int()
    if (version != Version)
      throw new IllegalStateException(s"Parser tables version $version, expected $Version: recompile the grammar")
    val tokenCount = r.int()
    val names = Array.fill(tokenCount)(r.string())
    val skip = r.ints().map(_ != 0)
    new Tables(
      tokenCount = tokenCount,
      tokenNames = names,
      skip = skip,
      lexAccept = r.ints(),
      transStart = r.ints(),
      transLo = r.ints(),
      transHi = r.ints(),
      transTarget = r.ints(),
      stateCount = r.int(),
      nonTerminalCount = r.int(),
      action = r.ints(),
      goto = r.ints(),
      prodLhs = r.ints(),
      prodLen = r.ints(),
      prodKind = r.ints(),
      prodArg = r.ints(),
      ntPrim = r.ints(),
      constants = Array.fill(r.int())(r.string()),
      literals = Array.fill(r.int())(if (r.int() < 0) null else r.string()),
      sliced = r.ints().map(_ != 0),
      llStart = r.int(),
      llExpectStart = r.ints(),
      llExpect = r.ints()
    )
  }
}
