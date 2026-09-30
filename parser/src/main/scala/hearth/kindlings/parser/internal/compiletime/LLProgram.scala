package hearth.kindlings.parser
package internal.compiletime

import hearth.kindlings.parser.internal.runtime.Flatten
import GrammarIR.{Alternative, Term}

import scala.collection.mutable

/** The top-down (LL(1)) parser of a grammar, as a small program the macro turns into a generated state machine (see
  * `GeneratedReductions.runLL`): one operation per state, the next state explicit, return addresses on a heap stack (so
  * it is stack-safe and resumable after effects, like the LALR machine).
  *
  * It is built from the flattened productions, whose values the generated `reduce` already computes: a production is
  * parsed symbol by symbol and then reduced. The helper non-terminals of `rep`/`rep1`/`sepBy1` are left-recursive (`H
  * ::= H x`), which a top-down parser cannot follow, so they become loops: reduce the first production (a new builder,
  * or one with the first element), then while the next token can start another element, parse it and reduce the
  * appending production.
  */
private[parser] object LLProgram {

  sealed trait Op

  /** The next token must be `token`: its value is pushed and the machine goes to `next`. */
  final case class Expect(token: Int, next: Int) extends Op

  /** Parse non-terminal `nt` (its code starts at `entry`), then continue at `next`. */
  final case class Call(nt: Int, entry: Int, next: Int) extends Op

  /** Reduce production `p` (a table index) with the generated `reduce`, then go to `next`. */
  final case class Reduce(p: Int, next: Int) extends Op

  /** Continue where the current non-terminal was called from. */
  case object Return extends Op

  /** Go to the state of the case whose tokens contain the next token, else to `default` (or fail).
    *
    * @param expected
    *   the tokens acceptable here, for syntax errors (with a `default`: also the tokens that can follow)
    */
  final case class Predict(cases: List[(List[Int], Int)], default: Option[Int], expected: List[Int]) extends Op

  /** The whole input must have been read. */
  case object Accept extends Op

  /** @param expected
    *   by state: the tokens the state accepts, for syntax errors
    */
  final case class Program(ops: Vector[Op], start: Int, expected: Vector[List[Int]])

  /** The program, or `None` if the flattened grammar needs more than one token of lookahead somewhere (the LL(1)
    * analysis of the grammar as written should have ruled that out; this keeps the LALR parser in that case).
    */
  def build(
      root: Int,
      ntCount: Int,
      prods: Vector[Flatten.Prod[Term, Alternative]],
      origins: Vector[Flatten.FSym[Term, Alternative]],
      userNonTerminals: Int,
      tokenOf: Term => Int
  ): Option[Program] = {
    type Rhs = Flatten.Rhs[Term]
    val prodsOf: Vector[Vector[Int]] = { // production table indices (p = index + 1) by non-terminal
      val by = Array.fill(ntCount)(Vector.newBuilder[Int])
      prods.zipWithIndex.foreach { case (prod, i) => by(prod.lhs) += i + 1 }
      by.toVector.map(_.result())
    }
    def prod(p: Int): Flatten.Prod[Term, Alternative] = prods(p - 1)
    def origin(nt: Int): Option[Flatten.FSym[Term, Alternative]] =
      if (nt < userNonTerminals) None else Some(origins(nt - userNonTerminals))

    // nullable / first / follow of the flattened grammar
    val nullable = Array.fill(ntCount)(false)
    val first = Array.fill(ntCount)(Set.empty[Int])
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
    var changed = true
    while (changed) {
      changed = false
      prods.foreach { prod =>
        if (!nullable(prod.lhs) && seqNullable(prod.rhs)) { nullable(prod.lhs) = true; changed = true }
        val f = seqFirst(prod.rhs)
        if (!f.subsetOf(first(prod.lhs))) { first(prod.lhs) = first(prod.lhs) ++ f; changed = true }
      }
    }
    val follow = Array.fill(ntCount)(Set.empty[Int])
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

    // which non-terminals can be inlined: used at one place, not recursive (the self-reference of a loop helper,
    // `H ::= H x`, is the loop, not a call)
    def callees(nt: Int): Seq[Int] = prodsOf(nt).flatMap { p =>
      prod(p).rhs.zipWithIndex.collect { case (Flatten.RNt(id, _), i) if !(i == 0 && id == nt) => id }
    }
    val callSites = Array.fill(ntCount)(0)
    callSites(root) += 1
    (0 until ntCount).foreach(nt => callees(nt).foreach(id => callSites(id) += 1))
    val recursive = (0 until ntCount).map { nt =>
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
    def inlined(nt: Int): Boolean = callSites(nt) == 1 && !recursive(nt)

    // the program
    var ok = true
    val ops = mutable.ArrayBuffer.empty[Op]
    def add(op: Op): Int = { ops += op; ops.size - 1 }
    val returnState = add(Return)
    val entries = mutable.Map.empty[Int, Int]
    val pending = mutable.Queue.empty[Int]

    /** The entry of the shared (called) code of `nt`, which ends with `Return`. */
    def entry(nt: Int): Int = entries.getOrElseUpdate(nt, { pending.enqueue(nt); add(Return) }) // placeholder
    /** The states parsing `rhs` and then going to `next`; single-use non-terminals are inlined. */
    def seq(rhs: Seq[Rhs], next: Int): Int = rhs.foldRight(next) {
      case (Flatten.RTerm(term), n)               => add(Expect(tokenOf(term), n))
      case (Flatten.RNt(id, _), n) if inlined(id) => code(id, n)
      case (Flatten.RNt(id, _), n)                => add(Call(id, entry(id), n))
    }

    /** Continue the loop of `nt` on the tokens that start another element, end it (going to `exit`) on the tokens that
      * can follow it, fail on anything else (reporting both, like the LALR parser in that situation).
      */
    def loopDecision(continueOn: Set[Int], continueAt: Int, nt: Int, exit: Int): Op =
      Predict(
        List(continueOn.toList.sorted -> continueAt, (follow(nt) -- continueOn).toList.sorted -> exit),
        None,
        (continueOn ++ follow(nt)).toList.sorted
      )

    /** The code of `nt`, going to `k` when it is parsed (`returnState` for shared code); returns its first state. */
    def code(nt: Int, k: Int): Int = origin(nt) match {
      case Some(Flatten.FRep(_, atLeastOne, _)) =>
        // H ::= (empty | x) and H ::= H x
        val (base, append) = prodsOf(nt).partition(p => !prod(p).rhs.headOption.contains(Flatten.RNt(nt, false)))
        val element = prod(append.head).rhs.drop(1)
        val loop = add(Return) // placeholder
        ops(loop) = loopDecision(seqFirst(element), seq(element, add(Reduce(append.head, loop))), nt, k)
        val reduceBase = add(Reduce(base.head, loop))
        if (atLeastOne) seq(prod(base.head).rhs, reduceBase) else reduceBase
      case Some(Flatten.FSepBy(_, _, true, _)) =>
        // H ::= x and H ::= H sep x
        val (base, append) = prodsOf(nt).partition(p => !prod(p).rhs.headOption.contains(Flatten.RNt(nt, false)))
        val sepAndElement = prod(append.head).rhs.drop(1)
        val loop = add(Return) // placeholder
        ops(loop) =
          loopDecision(seqFirst(sepAndElement.take(1)), seq(sepAndElement, add(Reduce(append.head, loop))), nt, k)
        seq(prod(base.head).rhs, add(Reduce(base.head, loop)))
      case _ =>
        // a choice between the productions by the next token
        val predicts = prodsOf(nt).map { p =>
          val rhs = prod(p).rhs
          p -> (seqFirst(rhs) ++ (if (seqNullable(rhs)) follow(nt) else Set.empty))
        }
        val all = predicts.flatMap(_._2)
        if (
          all.size != all.distinct.size || predicts.exists { case (p, _) =>
            prod(p).rhs.headOption.contains(Flatten.RNt(nt, false))
          }
        ) ok = false
        if (predicts.size == 1) seq(prod(predicts.head._1).rhs, add(Reduce(predicts.head._1, k)))
        else
          add(
            Predict(
              predicts.toList.map { case (p, tokens) => tokens.toList.sorted -> seq(prod(p).rhs, add(Reduce(p, k))) },
              None,
              all.distinct.sorted.toList
            )
          )
    }

    val accept = add(Accept)
    val start = seq(Vector(Flatten.RNt(root, false)), accept)
    while (pending.nonEmpty && ok) {
      val nt = pending.dequeue()
      // the entry placeholder jumps to the shared code (a `Call(-1, state, -1)` marks "go to state")
      ops(entries(nt)) = Call(-1, code(nt, returnState), -1)
    }
    if (!ok) None
    else {
      // resolve the "go to" markers: a state whose op is `Call(-1, s, -1)` is replaced by the op of `s`
      def resolve(state: Int): Int = ops(state) match {
        case Call(-1, s, -1) => resolve(s)
        case _               => state
      }
      val resolved = ops.toVector.map {
        case Expect(t, n)                      => Expect(t, resolve(n))
        case Call(-1, s, -1)                   => ops(resolve(s))
        case Call(nt, e, n)                    => Call(nt, resolve(e), resolve(n))
        case Reduce(p, n)                      => Reduce(p, resolve(n))
        case Predict(cases, default, expected) =>
          Predict(cases.map { case (ts, s) => ts -> resolve(s) }, default.map(resolve), expected)
        case other => other
      }
      val expected = resolved.map {
        case Expect(t, _)            => List(t)
        case Predict(_, _, expected) => expected
        case Accept                  => List(0)
        case _                       => Nil
      }
      Some(Program(resolved, resolve(start), expected))
    }
  }
}
