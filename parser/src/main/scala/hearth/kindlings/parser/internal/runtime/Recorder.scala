package hearth.kindlings.parser
package internal.runtime

import scala.collection.mutable

/** Records the productions added while a grammar block is evaluated at run time (once per `Parser` construction). */
final private[parser] class Recorder {

  private var nonTerminals: Int = 0
  private val productions: mutable.ListBuffer[(Int, Alt[Any])] = mutable.ListBuffer.empty

  def newNonTerminal[A](): NonTerminal[A] = {
    val nt = new NonTerminal[A](nonTerminals, this)
    nonTerminals += 1
    nt
  }

  def production(lhs: Int, alts: Alt[Any]): Unit = productions += (lhs -> alts)

  def nonTerminalCount: Int = nonTerminals

  def recorded: List[(Int, Alt[Any])] = productions.toList
}
