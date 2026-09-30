package hearth.kindlings.parser
package internal.compiletime

import GrammarIR.*

import scala.collection.mutable

/** Decides whether a grammar is LL(1) - whether a top-down parser can always decide what to do by looking at the next
  * token only - and, when it is not, explains why in plain words (`enable(RequireLL1)` turns the explanations into
  * compile errors).
  *
  * The analysis works on the grammar as written: `opt`, `rep`, `sepBy` and inline groups are loops and choices of the
  * top-down parser, not the left-recursive helper rules the LALR(1) tables are built from.
  *
  * @param tokenOf
  *   the token id of a terminal
  * @param tokenName
  *   the display name of a token id (0: end of input)
  */
final private[parser] class LL1(g: Grammar, tokenOf: Term => Int, tokenName: Int => String) {

  private val ntCount = g.nonTerminals.size
  private val alternatives: Vector[List[Alternative]] = {
    val byNt = Array.fill(ntCount)(List.newBuilder[Alternative])
    g.statements.foreach {
      case Production(lhs, alts, _) => byNt(lhs) ++= alts
      case _                        => ()
    }
    byNt.toVector.map(_.result())
  }
  private def ntName(nt: Int): String = g.nonTerminals(nt).name

  // --- which tokens can start a symbol, and which symbols can match nothing ------------------------------------------

  private val nullableNt = Array.fill(ntCount)(false)
  private val firstNt = Array.fill(ntCount)(Set.empty[Int])

  private def nullable(s: Sym): Boolean = s match {
    case NtRef(id)           => nullableNt(id)
    case _: Term             => false
    case Group(alts)         => alts.exists(a => nullableSeq(a.syms))
    case Opt(_)              => true
    case Rep(x, one, _)      => !one || nullable(x)
    case SepBy(x, _, one, _) => !one || nullable(x)
  }
  private def nullableSeq(syms: List[Sym]): Boolean = syms.forall(nullable)

  private def first(s: Sym): Set[Int] = s match {
    case NtRef(id)           => firstNt(id)
    case t: Term             => Set(tokenOf(t))
    case Group(alts)         => alts.flatMap(a => firstSeq(a.syms)).toSet
    case Opt(x)              => first(x)
    case Rep(x, _, _)        => first(x)
    case SepBy(x, sep, _, _) => if (nullable(x)) first(x) ++ first(sep) else first(x)
  }
  private def firstSeq(syms: List[Sym]): Set[Int] = syms match {
    case Nil          => Set.empty
    case head :: tail => if (nullable(head)) first(head) ++ firstSeq(tail) else first(head)
  }

  locally {
    var changed = true
    while (changed) {
      changed = false
      for (nt <- 0 until ntCount; alt <- alternatives(nt)) {
        if (!nullableNt(nt) && nullableSeq(alt.syms)) { nullableNt(nt) = true; changed = true }
        val f = firstSeq(alt.syms)
        if (!f.subsetOf(firstNt(nt))) { firstNt(nt) = firstNt(nt) ++ f; changed = true }
      }
    }
  }

  // --- which tokens can come right after each non-terminal ------------------------------------------------------------

  private val followNt = Array.fill(ntCount)(Set.empty[Int])
  followNt(g.root) = Set(0)

  /** Walks `syms` knowing that `after` can follow them; `onSym` sees every symbol with what can follow it. */
  private def walkSeq(syms: List[Sym], after: Set[Int])(onSym: (Sym, Set[Int]) => Unit): Unit =
    syms.tails.foreach {
      case head :: rest => walkSym(head, if (nullableSeq(rest)) firstSeq(rest) ++ after else firstSeq(rest))(onSym)
      case Nil          => ()
    }
  private def walkSym(s: Sym, after: Set[Int])(onSym: (Sym, Set[Int]) => Unit): Unit = {
    onSym(s, after)
    s match {
      case Group(alts)         => alts.foreach(a => walkSeq(a.syms, after)(onSym))
      case Opt(x)              => walkSym(x, after)(onSym)
      case Rep(x, _, _)        => walkSym(x, first(x) ++ after)(onSym)
      case SepBy(x, sep, _, _) =>
        walkSym(x, first(sep) ++ after)(onSym)
        walkSym(sep, if (nullable(x)) first(x) ++ first(sep) ++ after else first(x))(onSym)
      case _ => ()
    }
  }

  locally {
    var changed = true
    while (changed) {
      changed = false
      for (nt <- 0 until ntCount; alt <- alternatives(nt))
        walkSeq(alt.syms, followNt(nt)) {
          case (NtRef(id), after) if !after.subsetOf(followNt(id)) =>
            followNt(id) = followNt(id) ++ after
            changed = true
          case _ => ()
        }
    }
  }

  // --- explanations --------------------------------------------------------------------------------------------------

  private def showSym(s: Sym): String = s match {
    case NtRef(id)             => ntName(id)
    case t: Term               => t.name.getOrElse(t.pattern.display)
    case Group(alts)           => alts.map(a => showSeq(a.syms)).mkString("(", " || ", ")")
    case Opt(x)                => s"opt(${showSym(x)})"
    case Rep(x, one, _)        => s"${if (one) "rep1" else "rep"}(${showSym(x)})"
    case SepBy(x, sep, one, _) => s"${if (one) "sepBy1" else "sepBy"}(${showSym(x)}, ${showSym(sep)})"
  }
  private def showSeq(syms: List[Sym]): String = if (syms.isEmpty) "\"\" (nothing)" else syms.map(showSym).mkString(" ")
  private def showTokens(tokens: Set[Int]): String = tokens.toList.sorted.map(tokenName).mkString(", ")
  private def showAlt(nt: String, a: Alternative): String = s"`$nt ::= ${showSeq(a.syms)}` (${a.pos.show})"

  /** Why the grammar is not LL(1), one diagnostic per problem; empty when it is LL(1). */
  lazy val problems: List[Diagnostic] = {
    val out = List.newBuilder[Diagnostic]
    val leftRecursive = leftRecursion(out)

    /** Alternatives chosen by the next token: each token may start only one of them. */
    def checkChoice(what: String, alts: List[Alternative], after: Set[Int], show: Alternative => String): Unit = {
      val predicts = alts.map(a => a -> (firstSeq(a.syms) ++ (if (nullableSeq(a.syms)) after else Set.empty)))
      val empties = alts.filter(a => nullableSeq(a.syms))
      if (empties.size > 1)
        out += Diagnostic(
          empties(1).pos,
          s"$what has more than one alternative that can match nothing: ${empties.map(show).mkString(" and ")}. " +
            "When the next token starts none of the other alternatives, the parser cannot tell which of these to use. " +
            "Keep a single alternative that can be empty."
        )
      for (((a1, p1), i) <- predicts.zipWithIndex; (a2, p2) <- predicts.drop(i + 1)) {
        val shared = p1 intersect p2
        if (shared.nonEmpty) {
          val bothStart = firstSeq(a1.syms).intersect(firstSeq(a2.syms)).nonEmpty
          out += Diagnostic(
            a2.pos,
            if (bothStart)
              s"When the next token is ${showTokens(shared)}, the parser cannot tell which alternative of $what to use: " +
                s"${show(a1)} and ${show(a2)} can both start with it, and an LL(1) parser picks an alternative by " +
                "looking at the next token only. Move the common beginning out of the alternatives, e.g. " +
                "`all(x, \"a\") || all(x, \"b\")` becomes `all(x, \"a\" || \"b\")`, or make the alternatives start " +
                "with different tokens."
            else {
              val (empty, other) = if (nullableSeq(a1.syms)) (a1, a2) else (a2, a1)
              s"When the next token is ${showTokens(shared)}, the parser cannot tell which alternative of $what to use: " +
                s"${show(other)} starts with it, but ${show(empty)} can match nothing, and ${showTokens(shared)} can " +
                s"also come right after $what. Make what comes after $what start with a different token, or drop the " +
                s"empty alternative and write out both cases (with and without $what) where it is used."
            }
          )
        }
      }
    }

    def checkSym(where: String, pos: Pos)(s: Sym, after: Set[Int]): Unit = s match {
      case Group(alts) =>
        checkChoice(s"the inline group ${showSym(s)} in $where", alts, after, a => s"`${showSeq(a.syms)}`")
      case Opt(x) =>
        if (nullable(x))
          out += Diagnostic(
            pos,
            s"In $where, ${showSym(s)} is optional but ${showSym(x)} can already match nothing: " +
              "the parser cannot tell whether an empty match means the option is present. Drop the `opt`."
          )
        else {
          val shared = first(x) intersect after
          if (shared.nonEmpty)
            out += Diagnostic(
              pos,
              s"In $where, ${showSym(s)} may be skipped: when the next token is " +
                s"${showTokens(shared)}, it could be the start of the optional ${showSym(x)} or what comes after it, and " +
                "the parser cannot tell which. Make what follows the option start with a different token, or keep LALR " +
                "(remove `enable(RequireLL1)`), which decides later."
            )
        }
      case Rep(x, _, _) =>
        if (nullable(x))
          out += Diagnostic(
            pos,
            s"In $where, the repeated part of ${showSym(s)} can match nothing, so the repetition " +
              "could go on forever without reading input. Make the repeated part non-empty."
          )
        else {
          val shared = first(x) intersect after
          if (shared.nonEmpty)
            out += Diagnostic(
              pos,
              s"In $where, when the next token after an element of ${showSym(s)} is " +
                s"${showTokens(shared)}, it could start another ${showSym(x)} or be what comes after the repetition, so " +
                "the parser cannot tell whether the repetition continues. Add a separator or terminator (`sepBy`, or a " +
                "closing token), or keep LALR (remove `enable(RequireLL1)`)."
            )
        }
      case SepBy(x, sep, one, _) =>
        val sharedSep = first(sep) intersect after
        if (sharedSep.nonEmpty)
          out += Diagnostic(
            pos,
            s"In $where, when the next token after an element of ${showSym(s)} is " +
              s"${showTokens(sharedSep)}, it could be the separator (another element follows) or what comes after the " +
              "list, and the parser cannot tell which. Use a different separator, or close the list with its own token."
          )
        if (!one) {
          val sharedStart = first(x) intersect after
          if (sharedStart.nonEmpty)
            out += Diagnostic(
              pos,
              s"In $where, ${showSym(s)} may be empty: when the next token is " +
                s"${showTokens(sharedStart)}, it could start the first element or be what comes after the list, and the " +
                "parser cannot tell whether the list is empty. Close or open the list with its own token."
            )
        }
      case _ => ()
    }

    for (nt <- 0 until ntCount if !leftRecursive(nt)) {
      val name = s"`${ntName(nt)}`"
      val alts = alternatives(nt)
      if (alts.nonEmpty) checkChoice(name, alts, followNt(nt), a => showAlt(ntName(nt), a))
      alts.foreach(a => walkSeq(a.syms, followNt(nt))(checkSym(s"${showAlt(ntName(nt), a)}", a.pos)))
    }
    out.result()
  }

  def isLL1: Boolean = problems.isEmpty

  /** Reports each left-recursive cycle once; returns the non-terminals on such cycles. */
  private def leftRecursion(out: mutable.Growable[Diagnostic]): Set[Int] = {
    // edges nt -> (nt that can be parsed first, the alternative it comes from)
    def leftCorners(syms: List[Sym]): List[Int] = syms match {
      case Nil          => Nil
      case head :: tail =>
        val here = head match {
          case NtRef(id)           => List(id)
          case _: Term             => Nil
          case Group(alts)         => alts.flatMap(a => leftCorners(a.syms))
          case Opt(x)              => leftCorners(List(x))
          case Rep(x, _, _)        => leftCorners(List(x))
          case SepBy(x, sep, _, _) => leftCorners(List(x)) ++ (if (nullable(x)) leftCorners(List(sep)) else Nil)
        }
        if (nullable(head)) here ++ leftCorners(tail) else here
    }
    val edges: Vector[List[(Int, Alternative)]] =
      alternatives.map(alts => alts.flatMap(a => leftCorners(a.syms).distinct.map(_ -> a)))
    val onCycle = mutable.Set.empty[Int]
    val reported = mutable.Set.empty[Set[Int]]
    // a path from `start` back to itself, found by breadth-first search
    for (start <- 0 until ntCount) {
      val previous = mutable.Map.empty[Int, (Int, Alternative)]
      val queue = mutable.Queue(start)
      var found: Option[(Int, Alternative)] = None
      val seen = mutable.Set(start)
      while (queue.nonEmpty && found.isEmpty) {
        val nt = queue.dequeue()
        edges(nt).foreach { case (next, alt) =>
          if (next == start && found.isEmpty) found = Some(nt -> alt)
          else if (seen.add(next)) { previous(next) = nt -> alt; queue.enqueue(next) }
        }
      }
      found.foreach { case (last, lastAlt) =>
        var path = List(last -> lastAlt)
        var nt = last
        while (nt != start) { val (p, a) = previous(nt); path = (p -> a) :: path; nt = p }
        val cycle = path.map(_._1).toSet
        onCycle ++= cycle
        if (reported.add(cycle)) {
          val steps = path.map { case (from, alt) => s"`${ntName(from)} ::= ${showSeq(alt.syms)}` (${alt.pos.show})" }
          val loop = (path.map(p => ntName(p._1)) :+ ntName(start)).mkString(" -> ")
          val explanation =
            if (path.size == 1)
              s"`${ntName(start)}` can start with itself: ${steps.head}."
            else
              s"`${ntName(start)}` can start with itself through $loop: ${steps.mkString(", then ")}."
          out += Diagnostic(
            lastAlt.pos,
            s"$explanation To read `${ntName(start)}`, a top-down parser would first have to read `${ntName(start)}` " +
              "again, forever, before looking at any input (this is called left recursion). Describe the repetition " +
              "with `rep`/`sepBy` instead - e.g. `list ::= all(item, rep(all(\",\", item)))` rather than " +
              "`list ::= all(list, \",\", item)` - or keep LALR (remove `enable(RequireLL1)`), which handles left " +
              "recursion."
          )
        }
      }
    }
    onCycle.toSet
  }
}
