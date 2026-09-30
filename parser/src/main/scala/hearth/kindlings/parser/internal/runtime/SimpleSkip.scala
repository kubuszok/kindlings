package hearth.kindlings.parser
package internal.runtime

/** The ASCII chars whose runs are always skipped text, so that a lexer can skip them without running the DFA: `c`
  * qualifies when, from the start state, it leads to a state that accepts a skipped token, has only self-loops, and
  * loops only on chars that qualify themselves. A run of such chars is then a sequence of complete skipped tokens,
  * whatever the rest of the grammar (e.g. with `skip("[ \\t\\n]+")`: space, tab, newline; with `skip("( |\\t)x*")`:
  * nothing, since `x` continues the skipped token without starting one).
  */
private[parser] object SimpleSkip {

  def compute(
      accept: Array[Int],
      transStart: Array[Int],
      lo: Array[Int],
      hi: Array[Int],
      target: Array[Int],
      isSkip: Int => Boolean
  ): Array[Boolean] = {
    def transitions(s: Int): Range = transStart(s) until transStart(s + 1)
    def fromStart(c: Int): Int =
      transitions(0).collectFirst { case t if lo(t) <= c && c <= hi(t) => target(t) }.getOrElse(-1)
    val state = Array.tabulate(128)(fromStart)
    val candidate = Array.tabulate(128) { c =>
      val s = state(c)
      s > 0 && accept(s) >= 0 && isSkip(accept(s)) && transitions(s).forall(t => target(t) == s && hi(t) < 128)
    }
    var changed = true
    while (changed) {
      changed = false
      (0 until 128).foreach { c =>
        if (candidate(c)) {
          val s = state(c)
          val loopsOnOthers = transitions(s).exists(t => (lo(t) to hi(t)).exists(d => !candidate(d)))
          if (loopsOnOthers) { candidate(c) = false; changed = true }
        }
      }
    }
    candidate
  }
}
