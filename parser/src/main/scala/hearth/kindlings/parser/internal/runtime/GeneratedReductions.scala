package hearth.kindlings.parser
package internal.runtime

/** Implemented by code the `Grammar.grammar` macro generates for each grammar: the grammar's actions and terminal
  * conversions, inlined into `switch`es. Not meant to be implemented by hand.
  */
abstract class GeneratedReductions {

  /** The value of user production `p` (for effectful actions: the action's `F[R]`); its right-hand side values are
    * `values(base)`, `values(base + 1)`, ... (terminals as matched text, non-terminals as their values).
    */
  def action(p: Int, values: Array[Any], base: Int): Any

  /** Applies terminal converter `id` (a chain of `.map` functions) to the matched text. */
  def convert(id: Int, raw: String): Any

  /** Turns the mutable builder of repetition collection `id` into the collection. */
  def collect(id: Int, builder: Any): Any

  /** The `Factory` of each repetition collection, by collection id. */
  protected def factories(): Array[Any]

  /** [[factories]], evaluated once. */
  final protected val collectionFactories: Array[Any] = factories()
}
