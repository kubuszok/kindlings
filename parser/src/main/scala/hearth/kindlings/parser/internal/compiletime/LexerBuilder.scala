package hearth.kindlings.parser
package internal.compiletime

import scala.collection.mutable

/** Builds the lexer DFA: token patterns -> Thompson NFA -> subset-constructed DFA over UTF-16 code unit ranges.
  *
  * Longest match wins; among equally long matches the token with the lowest id (literals are numbered before regexes)
  * wins.
  */
private[parser] object LexerBuilder {

  final case class Dfa(accept: Array[Int], transStart: Array[Int], lo: Array[Int], hi: Array[Int], target: Array[Int])

  val MaxStates: Int = 20000

  final private class Nfa {
    val eps: mutable.ArrayBuffer[List[Int]] = mutable.ArrayBuffer.empty
    val edges: mutable.ArrayBuffer[List[(Int, Int, Int)]] = mutable.ArrayBuffer.empty
    val accept: mutable.Map[Int, Int] = mutable.Map.empty

    def state(): Int = {
      eps += Nil
      edges += Nil
      eps.size - 1
    }
    def epsilon(from: Int, to: Int): Unit = eps(from) = to :: eps(from)
    def edge(from: Int, set: Regex.CharSet, to: Int): Unit =
      set.ranges.foreach { case (lo, hi) => edges(from) = (lo, hi, to) :: edges(from) }

    /** Adds `node` between `from` and a fresh end state, which is returned. */
    def build(node: Regex.Node, from: Int): Int = node match {
      case Regex.Chars(set) =>
        val to = state()
        edge(from, set, to)
        to
      case Regex.Concat(nodes)      => nodes.foldLeft(from)((s, n) => build(n, s))
      case Regex.Alternation(nodes) =>
        val to = state()
        nodes.foreach { n =>
          val start = state()
          epsilon(from, start)
          epsilon(build(n, start), to)
        }
        to
      case Regex.Repeat(inner, min, max) =>
        var current = from
        (0 until min).foreach(_ => current = build(inner, current))
        max match {
          case None =>
            val loopStart = state()
            epsilon(current, loopStart)
            val loopEnd = build(inner, loopStart)
            epsilon(loopEnd, loopStart)
            val to = state()
            epsilon(loopStart, to)
            to
          case Some(mx) =>
            val to = state()
            epsilon(current, to)
            (min until mx).foreach { _ =>
              current = build(inner, current)
              epsilon(current, to)
            }
            to
        }
    }

    def closure(states: Iterable[Int]): Array[Int] = {
      val seen = mutable.BitSet.empty
      val stack = mutable.Stack.empty[Int]
      states.foreach(s => if (seen.add(s)) stack.push(s))
      while (stack.nonEmpty) {
        val s = stack.pop()
        eps(s).foreach(t => if (seen.add(t)) stack.push(t))
      }
      seen.toArray
    }
  }

  /** Checks that a pattern does not match the empty string. */
  def matchesEmpty(node: Regex.Node): Boolean = node match {
    case Regex.Chars(_)           => false
    case Regex.Concat(nodes)      => nodes.forall(matchesEmpty)
    case Regex.Alternation(nodes) => nodes.exists(matchesEmpty)
    case Regex.Repeat(n, min, _)  => min == 0 || matchesEmpty(n)
  }

  /** @param tokens
    *   `(tokenId, pattern)`; the lowest id wins ties
    */
  def build(tokens: Seq[(Int, Regex.Node)]): Either[String, Dfa] = {
    val nfa = new Nfa
    val start = nfa.state()
    tokens.foreach { case (id, node) =>
      val s = nfa.state()
      nfa.epsilon(start, s)
      nfa.accept(nfa.build(node, s)) = id
    }

    val index = mutable.HashMap.empty[Vector[Int], Int]
    val sets = mutable.ArrayBuffer.empty[Array[Int]]
    val queue = mutable.Queue.empty[Int]
    def dfaState(set: Array[Int]): Int = {
      val key = set.sorted.toVector
      index.getOrElseUpdate(
        key, {
          sets += key.toArray
          queue.enqueue(sets.size - 1)
          sets.size - 1
        }
      )
    }
    val _ = dfaState(nfa.closure(List(start)))

    val accept = mutable.ArrayBuffer.empty[Int]
    val transitions = mutable.ArrayBuffer.empty[Vector[(Int, Int, Int)]]
    while (queue.nonEmpty) {
      val d = queue.dequeue()
      if (sets.size > MaxStates) return Left(s"the lexer needs more than $MaxStates DFA states")
      val set = sets(d)
      val acc = set.flatMap(nfa.accept.get)
      while (accept.size <= d) { accept += -1; transitions += Vector.empty }
      accept(d) = if (acc.isEmpty) -1 else acc.min
      val edges = set.flatMap(nfa.edges(_))
      val points = edges.flatMap { case (lo, hi, _) => List(lo, hi + 1) }.distinct.sorted
      val out = Vector.newBuilder[(Int, Int, Int)]
      var last: Option[(Int, Int, Int)] = None
      points.sliding(2).foreach {
        case Array(a, b) =>
          val targets = edges.collect { case (lo, hi, t) if lo <= a && b - 1 <= hi => t }
          if (targets.nonEmpty) {
            val t = dfaState(nfa.closure(targets.toList))
            last match {
              case Some((llo, lhi, lt)) if lt == t && lhi + 1 == a => last = Some((llo, b - 1, t))
              case Some(prev)                                      => out += prev; last = Some((a, b - 1, t))
              case None                                            => last = Some((a, b - 1, t))
            }
          }
        case _ => ()
      }
      last.foreach(out += _)
      transitions(d) = out.result()
    }
    while (accept.size < sets.size) { accept += -1; transitions += Vector.empty }

    val transStart = new Array[Int](sets.size + 1)
    val all = transitions.toVector
    all.indices.foreach(s => transStart(s + 1) = transStart(s) + all(s).size)
    val flat = all.flatten
    Right(
      Dfa(
        accept.toArray,
        transStart,
        flat.map(_._1).toArray,
        flat.map(_._2).toArray,
        flat.map(_._3).toArray
      )
    )
  }
}
