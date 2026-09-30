package hearth.kindlings.parser
package internal.runtime

/** The ASCII chars whose runs are always skipped text, so that a lexer can skip them without running the DFA: `c`
  * qualifies when every state the DFA can reach from the start state through `c` accepts a skipped token and moves only
  * on chars that qualify themselves (all ASCII). A run of such chars is then a sequence of complete skipped tokens,
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

    /** The states reachable from `s` (itself included). */
    def reachable(s: Int): Set[Int] = {
      var seen = Set(s)
      var queue = List(s)
      while (queue.nonEmpty) {
        val next = queue.head
        queue = queue.tail
        transitions(next).foreach { t =>
          if (!seen(target(t))) { seen += target(t); queue = target(t) :: queue }
        }
      }
      seen
    }
    val region = Array.tabulate(128)(c => if (state(c) > 0) reachable(state(c)) else Set.empty[Int])
    val candidate = Array.tabulate(128) { c =>
      state(c) > 0 && region(c).forall { r =>
        accept(r) >= 0 && isSkip(accept(r)) && transitions(r).forall(t => hi(t) < 128)
      }
    }
    var changed = true
    while (changed) {
      changed = false
      (0 until 128).foreach { c =>
        if (candidate(c)) {
          val movesOnOthers =
            region(c).exists(r => transitions(r).exists(t => (lo(t) to hi(t)).exists(d => !candidate(d))))
          if (movesOnOthers) { candidate(c) = false; changed = true }
        }
      }
    }
    candidate
  }
}
