package hearth.kindlings.parser
package internal.compiletime

import hearth.kindlings.parser.internal.runtime.Flatten
import GrammarIR.{Alternative, Term}
import CodegenPlan.{CollectionStep, ReduceBody}

import scala.collection.mutable

/** The recursive-descent parser generated for LL(1) grammars: the fast path for `String` inputs.
  *
  * Every non-terminal becomes a method that returns its value. Symbols are read in order into local values and the
  * production's reduction (the same code as the LR machine's `reduce`) is applied to them, so there is no value stack,
  * no state stack and no reduction dispatch. Repetitions are loops that fill their collection's builder in place, and
  * non-terminals used at one place are inlined.
  *
  * Choices are made on characters where possible: when only one token of the grammar (skipped ones included) can
  * start with the next char, that char decides, and one-char and keyword literals are compared with the input instead
  * of being lexed. Only other tokens go through the generated lexer.
  *
  * The parser does not report errors itself. On anything unexpected (a syntax error, a rejected value, nesting deeper
  * than `Machine.MaxDescentDepth`) it gives up, and the input is parsed again by the LR (or LL) machine, which reports
  * the error with its usual messages and handles any depth on the heap. The same actions run in the same order in both
  * parsers, so for pure actions the result is the same.
  */
private[parser] object DescentPlan {

  /** How a terminal is read. */
  sealed trait Read
  object Read {

    /** A one-char literal no other token can start with: the next char is compared. */
    final case class OneChar(ch: Char) extends Read

    /** A literal whose first char no other token can start with: the input is compared with the text. */
    final case class Word(text: String) extends Read

    /** The generated lexer reads the token. */
    case object Lexed extends Read

    /** A token no other token starts like: its generated scanner reads it (`Program.scanners`), the lexer only when the
      * scanner does not start there (e.g. at a comment).
      */
    case object Scan extends Read

    /** The one-char literal a decision found at the cursor: consumed without looking at it. */
    case object Decided extends Read

    /** A literal whose first char a decision found at the cursor: the rest of the input is compared with the text. */
    final case class DecidedWord(text: String) extends Read
  }

  sealed trait Item

  /** Terminal `token`; its value is `literal`, the `mapSlice` conversion (`sliced`) or the matched text. */
  final case class Tok(token: Int, read: Read, literal: Option[String], sliced: Boolean) extends Item

  /** The value of non-terminal `nt`, parsed by its method. */
  final case class Call(nt: Int) extends Item

  /** The value of non-terminal `nt`, parsed by `code` inlined here. */
  final case class Inline(nt: Int, code: Code) extends Item

  /** The builder of the enclosing loop (the self-reference of a repetition helper, `H ::= H x`). */
  case object Acc extends Item

  /** Parses production `p` (a table index): reads `items` in order, then computes its reduction's value. Items whose
    * value the reduction does not use (`used(i) == false`) are only consumed.
    */
  final case class Prod(p: Int, items: Vector[Item], used: Vector[Boolean])

  sealed trait Code

  /** A non-terminal with one production. */
  final case class Single(prod: Prod) extends Code

  /** A choice between productions: `prods(k)` for the decision's case `k`, a failure otherwise. */
  final case class Choose(decision: Decision, prods: Vector[Prod]) extends Code

  /** A repetition helper: the builder is `base`, then while the decision's case is `0` (the next token can start
    * another element) `append` adds to it.
    */
  final case class Loop(base: Prod, decision: Decision, append: Prod) extends Code

  /** A choice made by the next char or, where chars do not tell, by the next token.
    *
    * @param chars
    *   ASCII chars (as `Int`s) that decide by themselves, with their case (`-1`: the default)
    * @param eof
    *   the case at the end of the input (`-1`: the default)
    * @param tokens
    *   the tokens of each case, for the other chars
    * @param failByDefault
    *   whether the default is a failure (a choice) rather than case `-1` (leaving a loop)
    */
  final case class Decision(
      chars: List[(List[Int], Int)],
      eof: Int,
      tokens: List[(List[Int], Int)],
      failByDefault: Boolean
  )

  /** @param methods
    *   the non-terminals with a method (called from several places, or recursive) and their code
    * @param recursive
    *   the non-terminals whose methods are recursive (they count the nesting depth)
    */
  /** @param scanners
    *   the scanner of each token read with [[Read.Scan]]
    */
  final case class Program(
      methods: Vector[(Int, Code)],
      root: Item,
      recursive: Set[Int],
      scanners: Vector[(Int, CodegenPlan.Lexer)]
  )

  /** Which values of the right-hand side the reduction reads. */
  def uses(body: ReduceBody, len: Int): Vector[Boolean] = body match {
    case ReduceBody.User(action, _, _)                    => action.used.toVector.padTo(len, false)
    case ReduceBody.Pass(_) | ReduceBody.OptSome(_)       => Vector.tabulate(len)(_ == 0)
    case ReduceBody.Collect(_, CollectionStep.One(_))     => Vector.tabulate(len)(_ == 0)
    case ReduceBody.Collect(_, CollectionStep.Append(i, _)) => Vector.tabulate(len)(j => j == 0 || j == i)
    case _                                                => Vector.fill(len)(false)
  }

  /** The program, or `None` when the flattened grammar needs more than one token of lookahead somewhere.
    *
    * @param uniqueStart
    *   the token each ASCII char starts when no other token (skipped ones included) can start with it
    */
  def build(
      analysis: FlatAnalysis,
      root: Int,
      origins: Vector[Flatten.FSym[Term, Alternative]],
      userNonTerminals: Int,
      tokenOf: Term => Int,
      bodies: Map[Int, ReduceBody],
      sliced: Int => Boolean,
      uniqueStart: Map[Int, Int],
      scanners: Map[Int, CodegenPlan.Lexer]
  ): Option[Program] = {
    import analysis.{inlined, prod, prodsOf, recursive, seqFirst}
    var ok = true

    def origin(nt: Int): Option[Flatten.FSym[Term, Alternative]] =
      if (nt < userNonTerminals) None else Some(origins(nt - userNonTerminals))

    def decision(cases: Vector[Set[Int]], failByDefault: Boolean): Decision = {
      val caseOf = cases.zipWithIndex.flatMap { case (ts, k) => ts.toList.map(_ -> k) }.toMap
      if (caseOf.size != cases.map(_.size).sum) ok = false // a token selects two cases: not LL(1)
      val chars = uniqueStart.toList
        .map { case (c, t) => c -> caseOf.getOrElse(t, -1) }
        .groupBy(_._2)
        .toList
        .sortBy(_._1)
        .map { case (k, cs) => cs.map(_._1).sorted -> k }
      Decision(
        chars,
        caseOf.getOrElse(0, -1),
        cases.toList.zipWithIndex.collect { case (ts, k) if ts.nonEmpty => ts.toList.sorted -> k },
        failByDefault
      )
    }

    def item(r: Flatten.Rhs[Term]): Item = r match {
      case Flatten.RTerm(term) =>
        val t = tokenOf(term)
        val literal = term.pattern match {
          case GrammarIR.LiteralPattern(text) => Some(text)
          case _                              => None
        }
        val read = literal match {
          case Some(text) if text.length == 1 && uniqueStart.get(text.charAt(0).toInt).contains(t) =>
            Read.OneChar(text.charAt(0))
          case Some(text) if text.nonEmpty && uniqueStart.get(text.charAt(0).toInt).contains(t) => Read.Word(text)
          case _ if scanners.contains(t)                                                         => Read.Scan
          case _                                                                                 => Read.Lexed
        }
        Tok(t, read, literal, sliced(t))
      case Flatten.RNt(id, _) if inlined(id) => Inline(id, code(id))
      case Flatten.RNt(id, _)                => Call(id)
    }

    def production(p: Int, items: Vector[Item]): Prod =
      Prod(p, items, uses(bodies(p), items.size))

    def plain(p: Int): Prod = production(p, prod(p).rhs.map(item))

    /** `prod` chosen by a decision: its first token starts at the cursor, a literal there is not looked at again. */
    def decided(prod: Prod): Prod = {
      val first = prod.items.indexWhere(_ != Acc)
      if (first < 0) prod
      else
        prod.items(first) match {
          case tok @ Tok(_, Read.OneChar(_), _, false) =>
            prod.copy(items = prod.items.updated(first, tok.copy(read = Read.Decided)))
          case tok @ Tok(_, Read.Word(text), _, false) =>
            prod.copy(items = prod.items.updated(first, tok.copy(read = Read.DecidedWord(text))))
          case _ => prod
        }
    }

    def code(nt: Int): Code = origin(nt) match {
      case Some(Flatten.FRep(_, _, _)) | Some(Flatten.FSepBy(_, _, true, _)) =>
        // H ::= base and H ::= H element...: a loop
        val (base, append) = prodsOf(nt).partition(p => !prod(p).rhs.headOption.contains(Flatten.RNt(nt, false)))
        if (base.size != 1 || append.size != 1) { ok = false; Single(plain(prodsOf(nt).head)) }
        else {
          val rest = prod(append.head).rhs.drop(1)
          // continue on the tokens that start another element (or separator), leave the loop on anything else (the
          // LL(1) analysis of the grammar as written ruled out tokens that could do both)
          Loop(
            plain(base.head),
            decision(Vector(seqFirst(rest.take(1))), failByDefault = false),
            decided(production(append.head, Acc +: rest.map(item)))
          )
        }
      case _ =>
        val ps = prodsOf(nt)
        if (ps.exists(p => prod(p).rhs.headOption.contains(Flatten.RNt(nt, false)))) ok = false // left recursion
        if (ps.size == 1) Single(plain(ps.head))
        else Choose(decision(ps.map(p => analysis.predict(nt, p)), failByDefault = true), ps.map(p => decided(plain(p))))
    }

    val methods = mutable.LinkedHashMap.empty[Int, Code]
    val rootItem = item(Flatten.RNt(root, false))
    var pending: List[Int] = Nil
    def calls(i: Item): List[Int] = i match {
      case Call(nt)            => List(nt)
      case Inline(_, c)        => codeCalls(c)
      case _                   => Nil
    }
    def codeCalls(c: Code): List[Int] = c match {
      case Single(p)          => p.items.toList.flatMap(calls)
      case Choose(_, ps)      => ps.toList.flatMap(_.items.flatMap(calls))
      case Loop(base, _, app) => (base.items ++ app.items).toList.flatMap(calls)
    }
    pending = calls(rootItem)
    while (pending.nonEmpty && ok) {
      val nt = pending.head
      pending = pending.tail
      if (!methods.contains(nt)) {
        val c = code(nt)
        methods(nt) = c
        pending = codeCalls(c) ++ pending
      }
    }
    def items(c: Code): Vector[Item] = c match {
      case Single(p)          => p.items
      case Choose(_, ps)      => ps.flatMap(_.items)
      case Loop(base, _, app) => base.items ++ app.items
    }
    def scanned(i: Item): Vector[Int] = i match {
      case Tok(t, Read.Scan, _, _) => Vector(t)
      case Inline(_, c)            => items(c).flatMap(scanned)
      case _                       => Vector.empty
    }
    if (!ok) None
    else {
      val used = (scanned(rootItem) ++ methods.values.toVector.flatMap(c => items(c).flatMap(scanned))).distinct.sorted
      Some(Program(methods.toVector, rootItem, methods.keys.filter(recursive).toSet, used.map(t => t -> scanners(t))))
    }
  }
}
