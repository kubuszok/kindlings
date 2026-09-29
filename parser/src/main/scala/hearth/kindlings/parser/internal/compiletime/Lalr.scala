package hearth.kindlings.parser
package internal.compiletime

import scala.collection.mutable

/** LALR(1) table construction: LR(0) automaton + lookahead propagation (Dragon Book, algorithm 4.63), with yacc-style
  * precedence/associativity resolution of shift/reduce conflicts.
  *
  * Symbols: terminals are `0 until terminals` (0 = end of input), non-terminal `n` is `terminals + n`. Production 0
  * must be the augmented start production `S' -> start`.
  */
final private[parser] class Lalr(
    terminals: Int,
    nonTerminals: Int,
    lhs: Array[Int],
    rhs: Array[Array[Int]],
    tokenPrec: Int => Option[(Int, GrammarIR.Assoc)],
    prodPrec: Int => Option[Int]
) {

  import Lalr.*

  private val prodCount = lhs.length
  private val itemBase: Array[Int] = {
    val base = new Array[Int](prodCount + 1)
    (0 until prodCount).foreach(p => base(p + 1) = base(p) + rhs(p).length + 1)
    base
  }
  private val itemProd: Array[Int] = {
    val a = new Array[Int](itemBase(prodCount))
    (0 until prodCount).foreach(p => (itemBase(p) until itemBase(p + 1)).foreach(a(_) = p))
    a
  }
  private def item(p: Int, dot: Int): Int = itemBase(p) + dot
  private def dot(it: Int): Int = it - itemBase(itemProd(it))
  private def next(it: Int): Int = {
    val p = itemProd(it)
    val d = dot(it)
    if (d < rhs(p).length) rhs(p)(d) else -1
  }

  private val prodsOf: Array[Array[Int]] = {
    val b = Array.fill(nonTerminals)(mutable.ArrayBuilder.make[Int])
    (0 until prodCount).foreach(p => b(lhs(p)) += p)
    b.map(_.result())
  }

  val nullable: Array[Boolean] = {
    val n = new Array[Boolean](nonTerminals)
    var changed = true
    while (changed) {
      changed = false
      (0 until prodCount).foreach { p =>
        if (!n(lhs(p)) && rhs(p).forall(s => s >= terminals && n(s - terminals))) { n(lhs(p)) = true; changed = true }
      }
    }
    n
  }

  private val first: Array[mutable.BitSet] = {
    val f = Array.fill(nonTerminals)(mutable.BitSet.empty)
    var changed = true
    while (changed) {
      changed = false
      (0 until prodCount).foreach { p =>
        val target = f(lhs(p))
        val before = target.size
        var i = 0
        var go = true
        while (go && i < rhs(p).length) {
          val s = rhs(p)(i)
          if (s < terminals) { target += s; go = false }
          else {
            target |= f(s - terminals)
            go = nullable(s - terminals)
          }
          i += 1
        }
        if (target.size != before) changed = true
      }
    }
    f
  }

  /** FIRST of `rhs(p)` from position `from`, plus `tail` if that suffix is nullable. */
  private def firstOf(p: Int, from: Int, tail: mutable.BitSet): mutable.BitSet = {
    val out = mutable.BitSet.empty
    var i = from
    while (i < rhs(p).length) {
      val s = rhs(p)(i)
      if (s < terminals) { out += s; return out }
      out |= first(s - terminals)
      if (!nullable(s - terminals)) return out
      i += 1
    }
    out |= tail
  }

  /** LR(1) closure: item -> lookaheads. */
  private def closure1(seed: Iterable[(Int, mutable.BitSet)]): mutable.LinkedHashMap[Int, mutable.BitSet] = {
    val res = mutable.LinkedHashMap.empty[Int, mutable.BitSet]
    val work = mutable.Queue.empty[Int]
    seed.foreach { case (it, la) =>
      res.getOrElseUpdate(it, mutable.BitSet.empty) |= la
      work.enqueue(it)
    }
    while (work.nonEmpty) {
      val it = work.dequeue()
      val x = next(it)
      if (x >= terminals) {
        val p = itemProd(it)
        val la = firstOf(p, dot(it) + 1, res(it))
        prodsOf(x - terminals).foreach { q =>
          val target = item(q, 0)
          res.get(target) match {
            case None =>
              res(target) = la.clone()
              work.enqueue(target)
            case Some(existing) =>
              if (!la.subsetOf(existing)) {
                existing |= la
                work.enqueue(target)
              }
          }
        }
      }
    }
    res
  }

  private def closure0(kernel: Array[Int]): Array[Int] = {
    val seen = mutable.LinkedHashSet.empty[Int]
    val work = mutable.Stack.empty[Int]
    kernel.foreach(it => if (seen.add(it)) work.push(it))
    while (work.nonEmpty) {
      val x = next(work.pop())
      if (x >= terminals) prodsOf(x - terminals).foreach { q =>
        val target = item(q, 0)
        if (seen.add(target)) work.push(target)
      }
    }
    seen.toArray
  }

  // LR(0) automaton
  val kernels: mutable.ArrayBuffer[Array[Int]] = mutable.ArrayBuffer.empty
  val gotos: mutable.ArrayBuffer[mutable.Map[Int, Int]] = mutable.ArrayBuffer.empty

  locally {
    val index = mutable.HashMap.empty[Vector[Int], Int]
    def state(kernel: Array[Int]): Int = {
      val key = kernel.sorted.toVector
      index.getOrElseUpdate(
        key, {
          kernels += key.toArray
          gotos += mutable.LinkedHashMap.empty
          kernels.size - 1
        }
      )
    }
    val _ = state(Array(item(0, 0)))
    var s = 0
    while (s < kernels.size) {
      if (kernels.size > MaxStates) throw new LalrTooLarge
      val groups = mutable.LinkedHashMap.empty[Int, mutable.ArrayBuffer[Int]]
      closure0(kernels(s)).foreach { it =>
        val x = next(it)
        if (x >= 0) groups.getOrElseUpdate(x, mutable.ArrayBuffer.empty) += (it + 1)
      }
      groups.foreach { case (x, advanced) => gotos(s)(x) = state(advanced.toArray) }
      s += 1
    }
  }

  val stateCount: Int = kernels.size

  // LALR lookaheads on kernel items
  private val kernelLa: Array[Array[mutable.BitSet]] =
    kernels.map(k => Array.fill(k.length)(mutable.BitSet.empty)).toArray

  locally {
    val marker = terminals // '#': propagation marker
    val propagate = mutable.HashMap.empty[(Int, Int), mutable.ArrayBuffer[(Int, Int)]]
    def kernelIndex(state: Int, it: Int): Int = java.util.Arrays.binarySearch(kernels(state), it)
    (0 until stateCount).foreach { s =>
      kernels(s).indices.foreach { k =>
        val closure = closure1(List(kernels(s)(k) -> mutable.BitSet(marker)))
        closure.foreach { case (it, la) =>
          val x = next(it)
          if (x >= 0) {
            val j = gotos(s)(x)
            val k2 = kernelIndex(j, it + 1)
            la.foreach { a =>
              if (a == marker) propagate.getOrElseUpdate((s, k), mutable.ArrayBuffer.empty) += (j -> k2)
              else kernelLa(j)(k2) += a
            }
          }
        }
      }
    }
    kernelLa(0)(0) += 0 // end of input after the start symbol
    var changed = true
    while (changed) {
      changed = false
      propagate.foreach { case ((s, k), targets) =>
        val from = kernelLa(s)(k)
        targets.foreach { case (j, k2) =>
          val to = kernelLa(j)(k2)
          if (!from.subsetOf(to)) { to |= from; changed = true }
        }
      }
    }
  }

  // Tables
  val action: Array[Int] = new Array[Int](stateCount * terminals)
  val goto: Array[Int] = Array.fill(stateCount * nonTerminals)(-1)
  val conflicts: mutable.ArrayBuffer[Conflict] = mutable.ArrayBuffer.empty

  locally {
    (0 until stateCount).foreach { s =>
      gotos(s).foreach { case (x, j) =>
        if (x < terminals) action(s * terminals + x) = j + 1
        else goto(s * nonTerminals + (x - terminals)) = j
      }
      val closure = closure1(kernels(s).indices.map(k => kernels(s)(k) -> kernelLa(s)(k)))
      closure.foreach { case (it, la) =>
        if (next(it) < 0) {
          val p = itemProd(it)
          la.foreach { a =>
            if (a < terminals) {
              val cell = s * terminals + a
              val existing = action(cell)
              if (existing == 0) action(cell) = -(p + 1)
              else if (existing > 0) resolveShiftReduce(s, a, p, cell)
              else {
                val other = -existing - 1
                if (other != p) {
                  conflicts += ReduceReduce(s, a, math.min(p, other), math.max(p, other))
                  action(cell) = -(math.min(p, other) + 1)
                }
              }
            }
          }
        }
      }
    }
  }

  private def resolveShiftReduce(s: Int, a: Int, p: Int, cell: Int): Unit =
    (tokenPrec(a), prodPrec(p)) match {
      case (Some((tp, assoc)), Some(pp)) =>
        if (pp > tp) action(cell) = -(p + 1)
        else if (pp == tp) assoc match {
          case GrammarIR.Assoc.Left     => action(cell) = -(p + 1)
          case GrammarIR.Assoc.Right    => ()
          case GrammarIR.Assoc.NonAssoc => action(cell) = 0
        }
      case _ => conflicts += ShiftReduce(s, a, p)
    }

  /** The items of a state (kernel items first), for diagnostics. */
  def items(state: Int): List[(Int, Int)] =
    closure0(kernels(state)).toList.map(it => itemProd(it) -> dot(it))
}
private[parser] object Lalr {

  val MaxStates: Int = 20000

  final class LalrTooLarge extends Exception(s"the grammar needs more than $MaxStates LR states")

  sealed trait Conflict { def state: Int; def token: Int }
  final case class ShiftReduce(state: Int, token: Int, prod: Int) extends Conflict
  final case class ReduceReduce(state: Int, token: Int, prod1: Int, prod2: Int) extends Conflict
}
