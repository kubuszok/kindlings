package hearth.kindlings.parser
package internal.runtime

/** Implemented by the code the `Grammar.grammar` macro generates for each grammar: the reduction of every production
  * (actions, terminal conversions and collection building inlined, stack effects compiled in) and a lexer for `String`
  * inputs. Not meant to be implemented by hand.
  */
abstract class GeneratedReductions {

  /** Reduces production `p` on `m`'s stack (see `Machine.stackValues`, `Machine.reduced`); `true` if an effectful
    * action suspended the machine (`Machine.suspend`).
    */
  def reduce(p: Int, m: Machine): Boolean

  /** Converts token `token` (one of the terminals with `mapSlice`) matched at `[start, end)` of `input`. */
  def slice(token: Int, input: String, start: Int, end: Int): Any

  /** Whether [[runLL]] is generated: the grammar is LL(1) and is parsed top down for `String` inputs. */
  def hasLL: Boolean

  /** Runs the LL(1) program on `m` (see `Machine`'s LL API) for at most `budget` steps; returns a `Machine` signal. */
  def runLL(m: Machine, budget: Int): Int

  /** Whether [[descend]] is generated: the grammar is LL(1) (see `DescentPlan`). */
  def hasDescent: Boolean

  /** Parses all of `text` by recursive descent (see `Machine`'s descent API) and returns the value; throws
    * [[DescentBail]] when it cannot (the machine then parses the input itself).
    */
  def descend(m: Machine, text: String): Any

  /** Whether [[lexString]] is generated (large lexers stay table-driven). */
  def hasStringLexer: Boolean

  /** Lexes the next non-skipped token of `text` from `from` into `m` (`Machine.token` / `Machine.lexError`); returns
    * `Machine.Done` or `Machine.Error`.
    */
  def lexString(m: Machine, text: String, from: Int): Int

  /** The `Factory` of each repetition collection, by collection id. */
  protected def factories(): Array[Any]

  /** [[factories]], evaluated once. */
  final protected val collectionFactories: Array[Any] = factories()
}
