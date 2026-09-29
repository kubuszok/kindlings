package hearth.kindlings.parser
package internal.runtime

/** Implemented by the code the `Grammar.grammar` macro generates for each grammar: the reduction of every production
  * (actions, terminal conversions and collection building inlined, stack effects compiled in) and, for `String` inputs,
  * the whole parse loop with the lexer inlined. Not meant to be implemented by hand.
  */
abstract class GeneratedReductions {

  /** Reduces production `p`: computes its value from `values` (whose top is at `top`), pops its right-hand side and
    * pushes the value with its goto state (`states(top)` and `values(top + 1)` must exist). Returns the new top, or
    * `-1 - newTop` when an effectful action suspended the machine (`Machine.suspendEffect`).
    */
  def reduce(p: Int, states: Array[Int], values: Array[Any], top: Int, goto: Array[Int], m: Machine): Int

  /** Whether [[runString]] is generated (grammars with large lexers use the table-driven machine). */
  def hasStringDriver: Boolean

  /** `Machine.run` for `String` inputs: the parse loop over locals, with the lexer inlined. */
  def runString(m: Machine, budget: Int): Int

  /** The `Factory` of each repetition collection, by collection id. */
  protected def factories(): Array[Any]

  /** [[factories]], evaluated once. */
  final protected val collectionFactories: Array[Any] = factories()
}
