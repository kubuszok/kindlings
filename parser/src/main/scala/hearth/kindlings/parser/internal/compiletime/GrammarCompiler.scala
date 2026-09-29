package hearth.kindlings.parser
package internal.compiletime

import hearth.kindlings.parser.internal.runtime.{Flatten, Tables}
import GrammarIR.*

import scala.collection.mutable

/** Compiles a [[GrammarIR.Grammar]] into encoded [[Tables]] plus the structural fingerprint checked at run time, or
  * into diagnostics (errors abort compilation, warnings are reported).
  */
private[parser] object GrammarCompiler {

  /** How to generate the code of user production `p` (a table index): its action and how to obtain each value. */
  final case class ProdPlan(p: Int, action: Action, rhs: Vector[RhsPlan])

  sealed trait RhsPlan
  final case class NtPlan(listify: Boolean) extends RhsPlan
  final case class TermPlan(converter: Option[Int]) extends RhsPlan

  /** @param converters
    *   by converter id: the `.map` functions to apply to the matched text, in order
    */
  final case class Output(
      tables: List[String],
      fingerprint: String,
      warnings: List[Diagnostic],
      summary: String,
      prods: Vector[ProdPlan],
      converters: Vector[List[Any]]
  )

  def compile(g: Grammar): Either[List[Diagnostic], Output] = {
    val errors = mutable.ListBuffer.empty[Diagnostic]
    val warnings = mutable.ListBuffer.empty[Diagnostic]
    def err(pos: Pos, msg: String): Unit = errors += Diagnostic(pos, msg)

    // --- tokens --------------------------------------------------------------------------------------------------
    final case class Token(pattern: Pattern, var name: Option[String], pos: Pos)
    val literals = mutable.LinkedHashMap.empty[String, Token]
    val regexes = mutable.LinkedHashMap.empty[String, Token]
    def register(t: Term): Unit = t.pattern match {
      case LiteralPattern(text) =>
        val tok = literals.getOrElseUpdate(text, Token(t.pattern, None, t.pos))
        if (tok.name.isEmpty) tok.name = t.name
      case RegexPattern(re) =>
        val tok = regexes.getOrElseUpdate(re, Token(t.pattern, None, t.pos))
        if (tok.name.isEmpty) tok.name = t.name
    }
    def walkSym(s: Sym): Unit = s match {
      case t: Term            => register(t)
      case Group(alts)        => alts.foreach(walkAlt)
      case Opt(sym)           => walkSym(sym)
      case Rep(sym, _)        => walkSym(sym)
      case SepBy(sym, sep, _) => walkSym(sym); walkSym(sep)
      case NtRef(_)           => ()
    }
    def walkAlt(a: Alternative): Unit = { a.syms.foreach(walkSym); a.prec.foreach(walkSym) }
    g.statements.foreach {
      case Production(_, alts, _) => alts.foreach(walkAlt)
      case Precedence(_, ops, _)  => ops.foreach(walkSym)
      case Skip(_, _)             => ()
    }
    val skips = g.statements.collect { case Skip(re, pos) => re -> pos }.distinctBy(_._1)
    skips.foreach { case (re, pos) =>
      if (regexes.contains(re)) err(pos, s"skipped pattern /$re/ is also used as a terminal")
    }
    literals.values.foreach { t =>
      if (t.pattern == LiteralPattern("")) err(t.pos, "the empty literal \"\" is only allowed as a whole alternative")
    }
    val tokens: Vector[Token] = literals.values.toVector ++ regexes.values.toVector
    val tokenId: Map[Pattern, Int] = tokens.zipWithIndex.map { case (t, i) => t.pattern -> (i + 1) }.toMap
    val skipIds = skips.indices.map(i => tokens.size + 1 + i)
    val tokenCount = tokens.size + skips.size + 1
    val tokenNames: Array[String] =
      (("end of input" +: tokens.map(t => t.name.getOrElse(t.pattern.display))) ++ skips.map { case (re, _) =>
        s"/$re/"
      }).toArray
    val skipFlags = Array.tabulate(tokenCount)(i => i > tokens.size)

    val lexerPatterns = mutable.ArrayBuffer.empty[(Int, Regex.Node)]
    tokens.foreach { t =>
      val node = t.pattern match {
        case LiteralPattern(text) => Right(Regex.literal(text))
        case RegexPattern(re)     => Regex.parse(re)
      }
      node match {
        case Left(msg) => err(t.pos, s"invalid terminal pattern: $msg")
        case Right(n)  =>
          if (LexerBuilder.matchesEmpty(n) && t.pattern != LiteralPattern(""))
            err(t.pos, s"terminal ${t.pattern.display} matches the empty string")
          else if (t.pattern != LiteralPattern("")) lexerPatterns += (tokenId(t.pattern) -> n)
      }
    }
    skips.zip(skipIds).foreach { case ((re, pos), id) =>
      Regex.parse(re) match {
        case Left(msg) => err(pos, s"invalid skip pattern: $msg")
        case Right(n)  =>
          if (LexerBuilder.matchesEmpty(n)) err(pos, s"skipped pattern /$re/ matches the empty string")
          else lexerPatterns += (id -> n)
      }
    }

    // --- precedence ----------------------------------------------------------------------------------------------
    val precOf = mutable.HashMap.empty[Int, (Int, Assoc)]
    g.statements.collect { case p: Precedence => p }.zipWithIndex.foreach { case (Precedence(assoc, ops, pos), level) =>
      ops.foreach {
        case t: Term => precOf(tokenId(t.pattern)) = (level + 1) -> assoc
        case _       => err(pos, "precedence declarations accept only terminals")
      }
    }

    // --- productions ---------------------------------------------------------------------------------------------
    def toF(s: Sym): Flatten.FSym[Term, Alternative] = s match {
      case NtRef(id)            => Flatten.FNt(id)
      case t: Term              => Flatten.FTerm(t)
      case Group(alts)          => Flatten.FGroup(alts.map(toFAlt))
      case Opt(sym)             => Flatten.FOpt(toF(sym))
      case Rep(sym, one)        => Flatten.FRep(toF(sym), one)
      case SepBy(sym, sep, one) => Flatten.FSepBy(toF(sym), toF(sep), one)
    }
    def toFAlt(a: Alternative): Flatten.FAlt[Term, Alternative] = Flatten.FAlt(
      a.syms.map(toF),
      a.kind match {
        case Kind.Pure | Kind.Effectful => Flatten.User(a)
        case Kind.Pass                  => Flatten.Pass
        case Kind.Empty                 => Flatten.Const("")
      }
    )
    val statements = g.statements.collect { case Production(lhs, alts, _) => lhs -> alts.map(toFAlt) }
    val flat = Flatten.flatten(g.nonTerminals.size, statements)
    val fingerprint = Flatten.fingerprint(g.root, flat) { a =>
      if (a.kind == Kind.Effectful) 'e' else 'p'
    }

    def symName(s: Flatten.FSym[Term, Alternative]): String = s match {
      case Flatten.FNt(id)               => ntName(id)
      case Flatten.FTerm(t)              => t.name.getOrElse(t.pattern.display)
      case Flatten.FGroup(alts)          => alts.map(a => a.syms.map(symName).mkString(" ")).mkString("(", " | ", ")")
      case Flatten.FOpt(sym)             => s"opt(${symName(sym)})"
      case Flatten.FRep(sym, one)        => s"${if (one) "rep1" else "rep"}(${symName(sym)})"
      case Flatten.FSepBy(sym, sep, one) => s"${if (one) "sepBy1" else "sepBy"}(${symName(sym)}, ${symName(sep)})"
    }
    lazy val ntNames: Vector[String] = g.nonTerminals.map(_.name) ++ flat.origins.map(symName)
    def ntName(id: Int): String = if (id < g.nonTerminals.size) g.nonTerminals(id).name else ntNames(id)

    // --- hygiene -------------------------------------------------------------------------------------------------
    val defined = flat.prods.map(_.lhs).toSet
    g.nonTerminals.zipWithIndex.foreach { case (nt, id) =>
      if (!defined(id))
        err(nt.pos, s"non-terminal `${nt.name}` has no productions (add some with `${nt.name} ::= ...`)")
    }
    val reachable = mutable.BitSet(g.root)
    var grew = true
    while (grew) {
      grew = false
      flat.prods.foreach { p =>
        if (reachable(p.lhs)) p.rhs.foreach {
          case Flatten.RNt(id, _) => if (reachable.add(id)) grew = true
          case _                  => ()
        }
      }
    }
    g.nonTerminals.zipWithIndex.foreach { case (nt, id) =>
      if (!reachable(id) && defined(id))
        warnings += Diagnostic(nt.pos, s"non-terminal `${nt.name}` is not reachable from the start symbol")
    }
    val productive = mutable.BitSet.empty
    grew = true
    while (grew) {
      grew = false
      flat.prods.foreach { p =>
        if (
          !productive(p.lhs) && p.rhs.forall {
            case Flatten.RNt(id, _) => productive(id)
            case _                  => true
          }
        ) { productive += p.lhs; grew = true }
      }
    }
    g.nonTerminals.zipWithIndex.foreach { case (nt, id) =>
      if (defined(id) && !productive(id))
        err(nt.pos, s"non-terminal `${nt.name}` cannot derive any finite input (every production recurses into itself)")
    }
    if (errors.nonEmpty) return Left(errors.toList)

    // --- LR tables -----------------------------------------------------------------------------------------------
    val nts = flat.nonTerminals + 1 // + augmented start
    val start = flat.nonTerminals
    val prods = flat.prods
    val lhs = (start +: prods.map(_.lhs)).toArray
    val rhs = (Array(tokenCount + g.root) +: prods.map { p =>
      p.rhs.map {
        case Flatten.RNt(id, _)  => tokenCount + id
        case Flatten.RTerm(term) => tokenId(term.pattern)
      }.toArray
    }).toArray
    def userAlt(p: Int): Option[Alternative] = if (p == 0) None
    else
      prods(p - 1).action match {
        case Flatten.AUser(a) => Some(a)
        case _                => None
      }
    val prodPrecedence: Int => Option[Int] = { p =>
      val explicit =
        userAlt(p).flatMap(_.prec).collect { case t: Term => precOf.get(tokenId(t.pattern)).map(_._1) }.flatten
      explicit.orElse(
        rhs(p).reverseIterator.filter(_ < tokenCount).map(precOf.get).collectFirst { case Some((l, _)) => l }
      )
    }
    val lalr =
      try new Lalr(tokenCount, nts, lhs, rhs, precOf.get, prodPrecedence)
      catch { case e: Lalr.LalrTooLarge => return Left(List(Diagnostic(g.rootPos, e.getMessage))) }

    def prodPos(p: Int): Pos = userAlt(p).map(_.pos).getOrElse {
      // helper productions: point at the first user alternative using the helper, or at the grammar
      g.rootPos
    }
    def showProd(p: Int, dotAt: Int = -1): String = {
      val syms = rhs(p).toList.map(s => if (s < tokenCount) tokenNames(s) else ntName(s - tokenCount))
      val withDot = if (dotAt < 0) syms else syms.patch(dotAt, List("."), 0)
      val body = if (withDot.isEmpty) "ε" else withDot.mkString(" ")
      val lhsName = if (lhs(p) == start) "<start>" else ntName(lhs(p))
      s"$lhsName ::= $body"
    }
    def showState(s: Int): String =
      lalr.items(s).map { case (p, d) => s"      ${showProd(p, d)}" }.mkString("\n")
    lalr.conflicts.foreach {
      case Lalr.ShiftReduce(s, a, p) =>
        err(
          prodPos(p),
          s"shift/reduce conflict on ${tokenNames(a)}: after `${showProd(p)}` the parser can either reduce by it or " +
            s"shift ${tokenNames(a)}.\n  Resolve it by declaring the precedence/associativity of ${tokenNames(a)} " +
            s"(left/right/nonassoc) and of this production, or by restructuring the grammar.\n  LR state $s:\n${showState(s)}"
        )
      case Lalr.ReduceReduce(s, a, p1, p2) =>
        err(
          prodPos(p2),
          s"reduce/reduce conflict on ${tokenNames(a)}: both `${showProd(p1)}` (${prodPos(p1).show}) and " +
            s"`${showProd(p2)}` can be reduced.\n  LR state $s:\n${showState(s)}"
        )
    }
    if (errors.nonEmpty) return Left(errors.toList)

    val dfa = LexerBuilder.build(lexerPatterns.toList) match {
      case Left(msg) => return Left(List(Diagnostic(g.rootPos, msg)))
      case Right(d)  => d
    }
    import hearth.kindlings.parser.internal.runtime.CompiledGrammar.*
    val converterIds = mutable.LinkedHashMap.empty[Term, Int]
    def converterOf(t: Term): Option[Int] =
      if (t.converters.isEmpty) None else Some(converterIds.getOrElseUpdate(t, converterIds.size))
    val constants = mutable.LinkedHashMap.empty[String, Int]
    val prodKind = new Array[Int](lhs.length)
    val prodArg = new Array[Int](lhs.length)
    prodKind(0) = -1
    val rhsStart = new Array[Int](lhs.length + 1)
    val rhsConv = mutable.ArrayBuilder.make[Int]
    rhsStart(1) = 0
    val plans = Vector.newBuilder[ProdPlan]
    prods.zipWithIndex.foreach { case (prod, index) =>
      val p = index + 1
      val rhsPlans = prod.rhs.map {
        case Flatten.RNt(_, listify) =>
          rhsConv += (if (listify) Tables.Listify else Tables.Raw)
          NtPlan(listify)
        case Flatten.RTerm(term) =>
          val conv = converterOf(term)
          rhsConv += conv.getOrElse(Tables.Raw)
          TermPlan(conv)
      }
      rhsStart(p + 1) = rhsStart(p) + prod.rhs.size
      prod.action match {
        case Flatten.AUser(alt) =>
          prodKind(p) = if (alt.kind == Kind.Effectful) ActEffect else ActPure
          alt.action.foreach(a => plans += ProdPlan(p, a, rhsPlans))
        case Flatten.APass         => prodKind(p) = ActPass
        case Flatten.AConst(value) =>
          prodKind(p) = ActConst; prodArg(p) = constants.getOrElseUpdate(value, constants.size)
        case Flatten.AOptNone           => prodKind(p) = ActOptNone
        case Flatten.AOptSome           => prodKind(p) = ActOptSome
        case Flatten.AListEmpty         => prodKind(p) = ActListEmpty
        case Flatten.AListOne           => prodKind(p) = ActListOne
        case Flatten.AListAppend(index) => prodKind(p) = ActListAppend; prodArg(p) = index
      }
    }
    val tables = new Tables(
      tokenCount = tokenCount,
      tokenNames = tokenNames,
      skip = skipFlags,
      lexAccept = dfa.accept,
      transStart = dfa.transStart,
      transLo = dfa.lo,
      transHi = dfa.hi,
      transTarget = dfa.target,
      stateCount = lalr.stateCount,
      nonTerminalCount = nts,
      action = lalr.action,
      goto = lalr.goto,
      prodLhs = lhs,
      prodLen = rhs.map(_.length),
      prodKind = prodKind,
      prodArg = prodArg,
      rhsStart = rhsStart,
      rhsConv = rhsConv.result(),
      constants = constants.keys.toArray
    )
    val summary =
      s"${tokenCount - 1} terminals, ${nts - 1} non-terminals (${flat.nonTerminals - g.nonTerminals.size} helpers), " +
        s"${prods.size} productions, ${lalr.stateCount} LALR(1) states, ${dfa.accept.length} lexer states"
    Right(
      Output(
        tables.encode.chunks,
        fingerprint,
        warnings.toList,
        summary,
        plans.result(),
        converterIds.keys.toVector.map(_.converters)
      )
    )
  }
}
