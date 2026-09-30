package hearth.kindlings.parser
package internal.compiletime

import hearth.kindlings.parser.internal.runtime.{Flatten, Tables}
import GrammarIR.*
import CodegenPlan.{CollectionStep, NtPlan, ReduceBody, RhsPlan, TermPlan}

import scala.collection.mutable

/** Compiles a [[GrammarIR.Grammar]] into encoded [[Tables]] plus the structural fingerprint checked at run time, or
  * into diagnostics (errors abort compilation, warnings are reported).
  */
private[parser] object GrammarCompiler {

  /** @param converters
    *   by converter id: the `.map` functions to apply to the matched text, in order
    * @param collections
    *   by collection id: the collection of a repetition
    * @param reduces
    *   the code of every production's reduction (only when compiling for generated code)
    * @param lexer
    *   the generated `String` lexer (only when compiling for generated code, and when the DFA is small enough)
    * @param flags
    *   the enabled `GrammarFlag`s, by name
    * @param slicers
    *   the `.mapSlice` functions, by token id (only when compiling for generated code)
    */
  final case class Output(
      tables: List[String],
      fingerprint: String,
      warnings: List[Diagnostic],
      summary: String,
      converters: Vector[List[Any]],
      collections: Vector[Collection],
      reduces: Vector[CodegenPlan.Reduce],
      lexer: Option[CodegenPlan.Lexer],
      slicers: Vector[(Int, Any)],
      flags: Set[String]
  )

  /** @param generated
    *   whether the grammar is run by generated code (`Grammar.grammar`, see [[CodegenPlan]]) rather than interpreted
    */
  def compile(g: Grammar, generated: Boolean): Either[List[Diagnostic], Output] = {
    val errors = mutable.ListBuffer.empty[Diagnostic]
    val warnings = mutable.ListBuffer.empty[Diagnostic]
    def err(pos: Pos, msg: String): Unit = errors += Diagnostic(pos, msg)

    // --- tokens --------------------------------------------------------------------------------------------------
    final case class Token(pattern: Pattern, var name: Option[String], pos: Pos)
    val literals = mutable.LinkedHashMap.empty[String, Token]
    val regexes = mutable.LinkedHashMap.empty[String, Token]
    // `.mapSlice` converts at shift time, per token: every terminal with the pattern must share it (be the same val)
    val slicerOf = mutable.LinkedHashMap.empty[Pattern, Option[Any]]
    def registerSlicer(t: Term): Unit = slicerOf.get(t.pattern) match {
      case None           => slicerOf(t.pattern) = t.slicer
      case Some(existing) =>
        val same = (existing, t.slicer) match {
          case (Some(a), Some(b)) => a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]
          case (None, None)       => true
          case _                  => false
        }
        if (!same)
          err(
            t.pos,
            s"the pattern ${t.pattern.display} is used by a terminal with `mapSlice` and by another terminal: " +
              "`mapSlice` converts the token when it is read, so its pattern must belong to one terminal declaration"
          )
    }
    def register(t: Term): Unit = { registerSlicer(t); registerPattern(t) }
    def registerPattern(t: Term): Unit = t.pattern match {
      case LiteralPattern(text) =>
        val tok = literals.getOrElseUpdate(text, Token(t.pattern, None, t.pos))
        if (tok.name.isEmpty) tok.name = t.name
      case RegexPattern(re) =>
        val tok = regexes.getOrElseUpdate(re, Token(t.pattern, None, t.pos))
        if (tok.name.isEmpty) tok.name = t.name
    }
    def walkSym(s: Sym): Unit = s match {
      case t: Term               => register(t)
      case Group(alts)           => alts.foreach(walkAlt)
      case Opt(sym)              => walkSym(sym)
      case Rep(sym, _, _)        => walkSym(sym)
      case SepBy(sym, sep, _, _) => walkSym(sym); walkSym(sep)
      case NtRef(_)              => ()
    }
    def walkAlt(a: Alternative): Unit = { a.syms.foreach(walkSym); a.prec.foreach(walkSym) }
    g.statements.foreach {
      case Production(_, alts, _) => alts.foreach(walkAlt)
      case Precedence(_, ops, _)  => ops.foreach(walkSym)
      case Skip(_, _)             => ()
      case FlagSetting(_, _, _)   => ()
    }
    // --- flags -------------------------------------------------------------------------------------------------------
    val flags: Set[String] = {
      val enabled = mutable.Set.empty[String]
      g.statements.collect { case f: FlagSetting => f }.groupBy(_.flag).foreach { case (flag, settings) =>
        if (settings.exists(_.enabled) && settings.exists(!_.enabled))
          err(settings.last.pos, s"`$flag` is both enabled and disabled in this grammar")
        else if (settings.head.enabled) enabled += flag
      }
      enabled.toSet
    }
    if (flags("RequireLL1") && flags("RequireLALR"))
      err(
        g.statements.collect { case f @ FlagSetting("RequireLALR", _, _) => f.pos }.head,
        "`RequireLL1` and `RequireLALR` ask for different parsers: enable only one of them"
      )
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
    val collections = Vector.newBuilder[Collection]
    var collectionCount = 0
    def collection(c: Collection): Int = {
      collections += c
      collectionCount += 1
      collectionCount - 1
    }
    def toF(s: Sym): Flatten.FSym[Term, Alternative] = s match {
      case NtRef(id)           => Flatten.FNt(id)
      case t: Term             => Flatten.FTerm(t)
      case Group(alts)         => Flatten.FGroup(alts.map(toFAlt))
      case Opt(sym)            => Flatten.FOpt(toF(sym))
      case Rep(sym, one, coll) =>
        val c = collection(coll)
        Flatten.FRep(toF(sym), one, c)
      case SepBy(sym, sep, one, coll) =>
        val c = collection(coll)
        Flatten.FSepBy(toF(sym), toF(sep), one, c)
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
      case Flatten.FNt(id)           => ntName(id)
      case Flatten.FTerm(t)          => t.name.getOrElse(t.pattern.display)
      case Flatten.FGroup(alts)      => alts.map(a => a.syms.map(symName).mkString(" ")).mkString("(", " | ", ")")
      case Flatten.FOpt(sym)         => s"opt(${symName(sym)})"
      case Flatten.FRep(sym, one, _) => s"${if (one) "rep1" else "rep"}(${symName(sym)})"
      case Flatten.FSepBy(sym, sep, one, _) => s"${if (one) "sepBy1" else "sepBy"}(${symName(sym)}, ${symName(sep)})"
    }

    /** The collection of each repetition helper non-terminal. */
    def ntCollection(id: Int): Int = flat.origins(id - g.nonTerminals.size) match {
      case Flatten.FRep(_, _, c)      => c
      case Flatten.FSepBy(_, _, _, c) => c
      case other                      => throw new IllegalStateException(s"not a repetition: $other")
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
    val reduces = Vector.newBuilder[CodegenPlan.Reduce]

    /** The goto target of `nt` when it is the same from every state (then generated code needs no goto lookup). */
    def constantGoto(nt: Int): Option[Int] = {
      val targets = (0 until lalr.stateCount).map(s => lalr.goto(s * nts + nt)).filter(_ >= 0).distinct
      if (targets.size == 1) Some(targets.head) else None
    }

    /** Generated grammars keep the values of user non-terminals with a primitive type unboxed; helpers stay boxed. */
    def primOf(nt: Int): Int =
      if (generated && nt < g.nonTerminals.size) g.nonTerminals(nt).prim
      else if (generated && nt == start) primOf(g.root)
      else hearth.kindlings.parser.internal.runtime.Prims.Boxed
    prods.zipWithIndex.foreach { case (prod, index) =>
      val p = index + 1
      val rhsPlans: Vector[RhsPlan] = prod.rhs.map {
        case Flatten.RNt(id, listify) => NtPlan(if (listify) Some(ntCollection(id)) else None, primOf(id))
        case Flatten.RTerm(term)      => TermPlan(converterOf(term))
      }
      val body: ReduceBody = prod.action match {
        case Flatten.AUser(alt) =>
          prodKind(p) = if (alt.kind == Kind.Effectful) ActEffect else ActPure
          val action = alt.action.getOrElse(throw new IllegalStateException(s"no action for production $p"))
          ReduceBody.User(action, rhsPlans, alt.kind == Kind.Effectful)
        case Flatten.APass =>
          prodKind(p) = ActPass
          ReduceBody.Pass(rhsPlans(0))
        case Flatten.AConst(value) =>
          prodKind(p) = ActConst; prodArg(p) = constants.getOrElseUpdate(value, constants.size)
          ReduceBody.Const(value)
        case Flatten.AOptNone =>
          prodKind(p) = ActOptNone
          ReduceBody.OptNone
        case Flatten.AOptSome =>
          prodKind(p) = ActOptSome
          ReduceBody.OptSome(rhsPlans(0))
        case Flatten.AListEmpty =>
          prodKind(p) = ActListEmpty
          ReduceBody.Collect(ntCollection(prod.lhs), CollectionStep.Empty)
        case Flatten.AListOne =>
          prodKind(p) = ActListOne
          ReduceBody.Collect(ntCollection(prod.lhs), CollectionStep.One(rhsPlans(0)))
        case Flatten.AListAppend(index) =>
          prodKind(p) = ActListAppend; prodArg(p) = index
          ReduceBody.Collect(ntCollection(prod.lhs), CollectionStep.Append(index, rhsPlans(index)))
      }
      if (generated)
        reduces += CodegenPlan.Reduce(p, prod.rhs.size, prod.lhs, constantGoto(prod.lhs), body, primOf(prod.lhs))
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
      ntPrim = Array.tabulate(nts)(primOf),
      constants = constants.keys.toArray,
      sliced = Array.tabulate(tokenCount) { t =>
        generated && t >= 1 && t <= tokens.size && slicerOf.get(tokens(t - 1).pattern).exists(_.isDefined)
      },
      literals = Array.tabulate(tokenCount) { t =>
        if (t >= 1 && t <= tokens.size) tokens(t - 1).pattern match {
          case LiteralPattern(text) => text
          case _                    => null
        }
        else null
      }
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
        converterIds.keys.toVector.map(_.converters),
        collections.result(),
        reduces.result(),
        if (generated) CodegenPlan.lexer(dfa, skipFrom = tokens.size + 1) else None,
        if (generated) slicerOf.toVector.collect { case (pattern, Some(fn)) => tokenId(pattern) -> fn }.sortBy(_._1)
        else Vector.empty,
        flags
      )
    )
  }
}
