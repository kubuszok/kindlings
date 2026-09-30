package hearth.kindlings.parser
package internal.compiletime

import hearth.kindlings.parser.internal.runtime.Flatten
import GrammarIR.{Alternative, Term}

import scala.collection.mutable

/** Top-down analysis of the flattened grammar shared by the LL(1) program ([[LLProgram]]) and the recursive-descent
  * parser ([[DescentPlan]]): nullable non-terminals, first and follow sets (of token ids), and which non-terminals can
  * be inlined at their only use.
  *
  * Productions are identified by their table index `p` (`prods(p - 1)`).
  */
final private[parser] class FlatAnalysis(
    root: Int,
    ntCount: Int,
    prods: Vector[Flatten.Prod[Term, Alternative]],
    tokenOf: Term => Int
) {
  type Rhs = Flatten.Rhs[Term]

  /** Production table indices by non-terminal. */
  val prodsOf: Vector[Vector[Int]] = {
    val by = Array.fill(ntCount)(Vector.newBuilder[Int])
    prods.zipWithIndex.foreach { case (prod, i) => by(prod.lhs) += i + 1 }
    by.toVector.map(_.result())
  }
  def prod(p: Int): Flatten.Prod[Term, Alternative] = prods(p - 1)

  val nullable: Array[Boolean] = Array.fill(ntCount)(false)
  val first: Array[Set[Int]] = Array.fill(ntCount)(Set.empty[Int])
  def symFirst(r: Rhs): Set[Int] = r match {
    case Flatten.RNt(id, _)  => first(id)
    case Flatten.RTerm(term) => Set(tokenOf(term))
  }
  def symNullable(r: Rhs): Boolean = r match {
    case Flatten.RNt(id, _) => nullable(id)
    case _                  => false
  }
  def seqFirst(rhs: Seq[Rhs]): Set[Int] =
    rhs
      .foldRight((Set.empty[Int], true)) { case (r, (acc, restNullable)) =>
        (if (symNullable(r)) symFirst(r) ++ acc else symFirst(r), symNullable(r) && restNullable)
      }
      ._1
  def seqNullable(rhs: Seq[Rhs]): Boolean = rhs.forall(symNullable)

  private var changed = true
  while (changed) {
    changed = false
    prods.foreach { prod =>
      if (!nullable(prod.lhs) && seqNullable(prod.rhs)) { nullable(prod.lhs) = true; changed = true }
      val f = seqFirst(prod.rhs)
      if (!f.subsetOf(first(prod.lhs))) { first(prod.lhs) = first(prod.lhs) ++ f; changed = true }
    }
  }
  val follow: Array[Set[Int]] = Array.fill(ntCount)(Set.empty[Int])
  follow(root) = Set(0)
  changed = true
  while (changed) {
    changed = false
    prods.foreach { prod =>
      prod.rhs.indices.foreach { i =>
        prod.rhs(i) match {
          case Flatten.RNt(id, _) =>
            val rest = prod.rhs.drop(i + 1)
            val f = seqFirst(rest) ++ (if (seqNullable(rest)) follow(prod.lhs) else Set.empty)
            if (!f.subsetOf(follow(id))) { follow(id) = follow(id) ++ f; changed = true }
          case _ => ()
        }
      }
    }
  }

  /** The tokens that select production `p` of `nt`: those that can start it, and those that can follow `nt` when `p` can
    * be empty.
    */
  def predict(nt: Int, p: Int): Set[Int] = {
    val rhs = prod(p).rhs
    seqFirst(rhs) ++ (if (seqNullable(rhs)) follow(nt) else Set.empty)
  }

  // which non-terminals can be inlined: used at one place (the self-reference of a loop helper, `H ::= H x`, is the
  // loop, not a use)
  def callees(nt: Int): Seq[Int] = prodsOf(nt).flatMap { p =>
    prod(p).rhs.zipWithIndex.collect { case (Flatten.RNt(id, _), i) if !(i == 0 && id == nt) => id }
  }
  private val callSites = Array.fill(ntCount)(0)
  callSites(root) += 1
  (0 until ntCount).foreach(nt => callees(nt).foreach(id => callSites(id) += 1))

  /** Whether `nt` can (indirectly) call itself: only such non-terminals make the call stack grow with the input. */
  val recursive: IndexedSeq[Boolean] = (0 until ntCount).map { nt =>
    val seen = mutable.Set.empty[Int]
    val queue = mutable.Queue.from(callees(nt))
    var found = false
    while (queue.nonEmpty && !found) {
      val next = queue.dequeue()
      if (next == nt) found = true
      else if (seen.add(next)) queue ++= callees(next)
    }
    found
  }
  /** Whether `nt` is inlined at its only use. This terminates: a cycle of non-terminals reachable from the root is
    * entered from outside, so one of its non-terminals has two uses and is not inlined.
    */
  def inlined(nt: Int): Boolean = callSites(nt) == 1
}
