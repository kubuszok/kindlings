package hearth.kindlings.parser
package internal.compiletime

import GrammarIR.Action

import scala.collection.mutable

/** What the `Grammar.grammar` macro generates, independently of the compiler API: the per-production reduction code
  * ([[CodegenPlan.Reduce]]) and the `String` lexer ([[CodegenPlan.Lexer]]). The per-compiler bridges turn these plans
  * into trees.
  */
private[parser] object CodegenPlan {

  // --- reductions ------------------------------------------------------------------------------------------------

  /** How to obtain a right-hand side value: non-terminals as is or, for repetitions, as their collection `id`
    * (converting the builder), terminals as the matched text or through converter `id`.
    */
  sealed trait RhsPlan
  final case class NtPlan(collection: Option[Int]) extends RhsPlan
  final case class TermPlan(converter: Option[Int]) extends RhsPlan

  /** The code reducing production `p` (a table index) whose right-hand side has `len` symbols: compute the value from
    * the top `len` stack values, pop them and push the value in state `goto` (when every state reached by the
    * production's left-hand side is that one state) or in the state the goto table gives for `lhs`.
    */
  final case class Reduce(p: Int, len: Int, lhs: Int, goto: Option[Int], body: ReduceBody)

  sealed trait ReduceBody
  object ReduceBody {

    /** A user action; an effectful one suspends the machine with the action's `F[R]`. */
    final case class User(action: Action, rhs: Vector[RhsPlan], effectful: Boolean) extends ReduceBody

    /** The value of the only symbol. */
    final case class Pass(rhs: RhsPlan) extends ReduceBody
    final case class Const(value: String) extends ReduceBody
    case object OptNone extends ReduceBody
    final case class OptSome(rhs: RhsPlan) extends ReduceBody

    /** A new builder of `collection`, a new builder with one element, or the builder at position 0 plus one element. */
    final case class Collect(collection: Int, step: CollectionStep) extends ReduceBody
  }

  sealed trait CollectionStep
  object CollectionStep {
    case object Empty extends CollectionStep
    final case class One(element: RhsPlan) extends CollectionStep
    final case class Append(index: Int, element: RhsPlan) extends CollectionStep
  }

  // --- lexer -----------------------------------------------------------------------------------------------------

  /** A lexer compiled to code, reading a `String` `text` of length `len` from position `i`.
    *
    * The DFA's states become code: a state reached from exactly one other state is inlined into that state's
    * transition, so only the start state and "join" states (several predecessors, or cycles) are cases of the `state`
    * loop. A token such as `{` or a closing quote finishes without reading the next char, a self-loop (string bodies,
    * digits, whitespace) is a tight `while` loop, and accepting is a constant.
    *
    * The generated code keeps the table lexer's semantics: longest match, the lowest token id among equally long
    * matches, skipped tokens (ids `>= skipFrom`) are consumed before the next token.
    *
    * @param states
    *   the code of the start state (first) and of each join state
    */
  final case class Lexer(states: Vector[(Int, LexNode)], skipFrom: Int)

  sealed trait LexNode
  object LexNode {
    final case class Block(nodes: List[LexNode]) extends LexNode

    /** `while (i < len && ranges.contains(text.charAt(i))) i += 1` */
    final case class SelfLoop(ranges: List[(Int, Int)]) extends LexNode

    /** `acc = token; accEnd = i` */
    final case class Accept(token: Int) extends LexNode

    /** `state = s`: continue in join state `s`, or stop scanning the token (`s = -1`). */
    final case class Goto(state: Int) extends LexNode

    /** `if (i < len) { val c = text.charAt(i); if (c in ranges_k) { i += 1; node_k } ... else otherwise } else
      * otherwise`
      */
    final case class Dispatch(cases: List[(List[(Int, Int)], LexNode)], otherwise: LexNode) extends LexNode
  }

  /** The UTF-16 code units not in `ranges` (sorted, disjoint). */
  def complement(ranges: List[(Int, Int)]): List[(Int, Int)] = {
    val out = List.newBuilder[(Int, Int)]
    var next = 0
    ranges.sortBy(_._1).foreach { case (lo, hi) =>
      if (lo > next) out += (next -> (lo - 1))
      next = math.max(next, hi + 1)
    }
    if (next <= 0xffff) out += (next -> 0xffff)
    out.result()
  }

  /** Beyond these sizes the lexer stays table-driven: the JVM does not JIT-compile methods over 8000 bytes of bytecode
    * (`-XX:-DontCompileHugeMethods`), which would make the generated lexer much slower than the tables.
    */
  val MaxLexerStates: Int = 256
  val MaxLexerCost: Int = 6000

  /** The generated lexer for `dfa` or `None` when it would be too large (see [[MaxLexerCost]]). */
  def lexer(dfa: LexerBuilder.Dfa, skipFrom: Int): Option[Lexer] = {
    val states = dfa.accept.length
    if (states > MaxLexerStates) return None
    def transitions(s: Int): List[(Int, Int, Int)] =
      (dfa.transStart(s) until dfa.transStart(s + 1)).toList.map(t => (dfa.lo(t), dfa.hi(t), dfa.target(t)))
    val preds = Array.fill(states)(mutable.Set.empty[Int])
    for (s <- 0 until states; (_, _, t) <- transitions(s) if t != s) preds(t) += s
    // join states: several predecessors, the start state if it is re-entered, and one state of each cycle that is not
    // otherwise entered through a join (found while inlining)
    val joins = mutable.LinkedHashSet(0)
    (0 until states).foreach(s => if (preds(s).size > 1 || (s == 0 && preds(s).nonEmpty)) joins += s)
    var cost = 0

    def code(s: Int, onPath: Set[Int]): LexNode = {
      val all = transitions(s)
      val self = all.collect { case (lo, hi, t) if t == s => (lo, hi) }
      val out = all.filter(_._3 != s)
      val nodes = List.newBuilder[LexNode]
      if (self.nonEmpty) { nodes += LexNode.SelfLoop(self); cost += 20 + 10 * self.size }
      if (dfa.accept(s) >= 0) { nodes += LexNode.Accept(dfa.accept(s)); cost += 8 }
      if (out.isEmpty) nodes += LexNode.Goto(-1)
      else {
        val byTarget = out.groupBy(_._3).toList.sortBy(_._1).map { case (t, rs) =>
          val ranges = rs.map(r => (r._1, r._2)).sortBy(_._1)
          cost += 12 + ranges.map { case (lo, hi) => if (hi < 128) (hi - lo + 1) * 4 else 16 }.sum
          val next =
            if (joins(t)) LexNode.Goto(t)
            else if (onPath(t)) { joins += t; LexNode.Goto(t) } // a cycle through inlined states
            else code(t, onPath + s)
          ranges -> next
        }
        nodes += LexNode.Dispatch(byTarget, LexNode.Goto(-1))
        cost += 20
      }
      nodes.result() match {
        case List(single) => single
        case many         => LexNode.Block(many)
      }
    }

    val result = mutable.LinkedHashMap.empty[Int, LexNode]
    var pending = joins.toList.filterNot(result.contains)
    while (pending.nonEmpty) {
      pending.foreach(j => result(j) = code(j, Set(j)))
      pending = joins.toList.filterNot(result.contains)
    }
    // join states are numbered 0, 1, 2, ... (the start state first) so that the `state` match is a `tableswitch` (a
    // jump table) rather than a `lookupswitch` (a binary search)
    val index = result.keys.zipWithIndex.toMap
    def renumber(node: LexNode): LexNode = node match {
      case LexNode.Block(nodes)               => LexNode.Block(nodes.map(renumber))
      case LexNode.Goto(t) if t >= 0          => LexNode.Goto(index(t))
      case LexNode.Dispatch(cases, otherwise) =>
        LexNode.Dispatch(cases.map { case (r, n) => r -> renumber(n) }, renumber(otherwise))
      case other => other
    }
    val compiled = result.toVector.map { case (s, node) => index(s) -> renumber(node) }
    if (cost > MaxLexerCost) None else Some(Lexer(compiled, skipFrom))
  }
}
