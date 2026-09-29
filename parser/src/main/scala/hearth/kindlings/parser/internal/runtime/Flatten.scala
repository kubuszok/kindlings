package hearth.kindlings.parser
package internal.runtime

import scala.collection.mutable

/** Flattening of a grammar's alternatives into plain productions: inline groups, `opt`, `rep`, `sepBy`, ... become
  * helper non-terminals with built-in actions.
  *
  * Shared by the macro (which flattens its compile-time view of the grammar to build the LR tables) and the run-time
  * builder (which flattens the evaluated grammar block to collect the action functions). Both views are isomorphic, so
  * flattening them with this same deterministic algorithm yields the same productions in the same order; the
  * [[fingerprint]] check guards that invariant.
  *
  * `T` is the terminal payload (compile time: the terminal's description; run time: its conversion function), `A` the
  * user action payload.
  */
private[parser] object Flatten {

  sealed trait FSym[+T, +A]
  final case class FNt(id: Int) extends FSym[Nothing, Nothing]
  final case class FTerm[+T](payload: T) extends FSym[T, Nothing]
  final case class FGroup[+T, +A](alts: List[FAlt[T, A]]) extends FSym[T, A]
  final case class FOpt[+T, +A](sym: FSym[T, A]) extends FSym[T, A]
  final case class FRep[+T, +A](sym: FSym[T, A], atLeastOne: Boolean) extends FSym[T, A]
  final case class FSepBy[+T, +A](sym: FSym[T, A], sep: FSym[T, A], atLeastOne: Boolean) extends FSym[T, A]

  final case class FAlt[+T, +A](syms: List[FSym[T, A]], action: FAction[A])

  sealed trait FAction[+A]
  final case class User[+A](payload: A) extends FAction[A]
  case object Pass extends FAction[Nothing]
  final case class Const(value: String) extends FAction[Nothing]

  /** A position of a flattened production's right-hand side. `listify` converts a list builder to a `List`. */
  sealed trait Rhs[+T]
  final case class RNt(id: Int, listify: Boolean) extends Rhs[Nothing]
  final case class RTerm[+T](payload: T) extends Rhs[T]

  /** What happens on reduction of a flattened production. */
  sealed trait Act[+A]
  final case class AUser[+A](payload: A) extends Act[A]
  case object APass extends Act[Nothing]
  final case class AConst(value: String) extends Act[Nothing]
  case object AOptNone extends Act[Nothing]
  case object AOptSome extends Act[Nothing]
  case object AListEmpty extends Act[Nothing]
  case object AListOne extends Act[Nothing]
  final case class AListAppend(elementIndex: Int) extends Act[Nothing]

  final case class Prod[+T, +A](lhs: Int, rhs: Vector[Rhs[T]], action: Act[A])

  /** @param origins
    *   for each helper non-terminal (ids `userNonTerminals`, `userNonTerminals + 1`, ...): the symbol it was made for
    */
  final case class Result[+T, +A](prods: Vector[Prod[T, A]], nonTerminals: Int, origins: Vector[FSym[T, A]])

  def flatten[T, A](userNonTerminals: Int, statements: Seq[(Int, List[FAlt[T, A]])]): Result[T, A] = {
    val prods = Vector.newBuilder[Prod[T, A]]
    val origins = Vector.newBuilder[FSym[T, A]]
    var next = userNonTerminals
    val queue = mutable.Queue.empty[(Int, FSym[T, A])]

    def helper(origin: FSym[T, A]): Int = {
      val id = next
      next += 1
      origins += origin
      queue.enqueue(id -> origin)
      id
    }
    def ref(sym: FSym[T, A]): Rhs[T] = sym match {
      case FNt(id)                    => RNt(id, listify = false)
      case FTerm(payload)             => RTerm(payload)
      case g: FGroup[T, A] @unchecked => RNt(helper(g), listify = false)
      case o: FOpt[T, A] @unchecked   => RNt(helper(o), listify = false)
      case r: FRep[T, A] @unchecked   => RNt(helper(r), listify = true)
      case s: FSepBy[T, A] @unchecked => RNt(helper(s), listify = true)
    }
    def action(a: FAction[A]): Act[A] = a match {
      case User(payload) => AUser(payload)
      case Pass          => APass
      case Const(value)  => AConst(value)
    }
    def alternative(lhs: Int, alt: FAlt[T, A]): Unit =
      prods += Prod(lhs, alt.syms.map(ref).toVector, action(alt.action))

    statements.foreach { case (lhs, alts) => alts.foreach(alternative(lhs, _)) }
    while (queue.nonEmpty) {
      val (id, origin) = queue.dequeue()
      origin match {
        case FGroup(alts) => alts.foreach(alternative(id, _))
        case FOpt(sym)    =>
          val r = ref(sym)
          prods += Prod(id, Vector.empty, AOptNone)
          prods += Prod(id, Vector(r), AOptSome)
        case FRep(sym, atLeastOne) =>
          val r = ref(sym)
          if (atLeastOne) prods += Prod(id, Vector(r), AListOne)
          else prods += Prod(id, Vector.empty, AListEmpty)
          prods += Prod(id, Vector(RNt(id, listify = false), r), AListAppend(1))
        case FSepBy(sym, sep, true) =>
          val r = ref(sym)
          val s = ref(sep)
          prods += Prod(id, Vector(r), AListOne)
          prods += Prod(id, Vector(RNt(id, listify = false), s, r), AListAppend(2))
        case FSepBy(sym, sep, false) =>
          val inner = helper(FSepBy(sym, sep, atLeastOne = true))
          prods += Prod(id, Vector.empty, AListEmpty)
          prods += Prod(id, Vector(RNt(inner, listify = false)), APass)
        case _ => ()
      }
    }
    Result(prods.result(), next, origins.result())
  }

  /** A structural summary of flattened productions, compared between compile time and run time. */
  def fingerprint[T, A](root: Int, result: Result[T, A])(userKind: A => Char): String = {
    val prods = result.prods.map { p =>
      val rhs = p.rhs.map {
        case RNt(id, listify) => s"n$id${if (listify) "L" else ""}"
        case RTerm(_)         => "t"
      }
      val action = p.action match {
        case AUser(payload)     => userKind(payload).toString
        case APass              => "a"
        case AConst(value)      => s"c${value.length}"
        case AOptNone           => "o0"
        case AOptSome           => "o1"
        case AListEmpty         => "l0"
        case AListOne           => "l1"
        case AListAppend(index) => s"l+$index"
      }
      s"${p.lhs}:${rhs.mkString(",")}:$action"
    }
    s"root=$root;nts=${result.nonTerminals};${prods.mkString("|")}"
  }
}
