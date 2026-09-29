package hearth.kindlings.parser
package internal.runtime

/** Run-time half of the `Grammar` macros. */
object Builder {

  /** `Grammar.grammar`: the tables and the macro-generated actions. */
  def generated[F[_], R](tables: List[String], engine: ParserEngine[F], reductions: GeneratedReductions): Parser[F, R] =
    new Parser[F, R](new GeneratedGrammar(Tables.decode(tables.mkString), reductions), engine)

  /** `Grammar.interpreted`: evaluates the grammar block once to collect the action functions, checks that the evaluated
    * grammar has the structure the macro compiled, and decodes the compile-time tables.
    */
  def build[F[_], R](
      tables: List[String],
      fingerprint: String,
      body: Dsl[F] => NonTerminal[R],
      engine: ParserEngine[F]
  ): Parser[F, R] = {
    val dsl = new Dsl[F]
    val root = body(dsl)
    val recorder = dsl.recorder
    val statements = recorder.recorded.map { case (lhs, alt) => lhs -> alt.alternatives.map(single) }
    val flat = Flatten.flatten(recorder.nonTerminalCount, statements)
    val actual = Flatten.fingerprint(root.id, flat)(action => if (action.effectful) 'e' else 'p')
    if (actual != fingerprint)
      throw new IllegalStateException(
        "The grammar evaluated at run time differs from the one compiled by the macro " +
          "(grammar blocks must be static: no conditionals, loops or values computed at run time around " +
          s"declarations and productions).\n  compiled:  $fingerprint\n  evaluated: $actual"
      )
    new Parser[F, R](new InterpretedGrammar(Tables.decode(tables.mkString), flat), engine)
  }

  private type FSym = Flatten.FSym[String => Any, Alt.Action]

  private def single(alt: Alt.Single): Flatten.FAlt[String => Any, Alt.Action] =
    Flatten.FAlt(alt.syms.map(sym), alt.action)

  private def sym(s: Sym[Any]): FSym = s match {
    case nt: NonTerminal[?] => Flatten.FNt(nt.id)
    case t: Terminal[?]     => Flatten.FTerm(t.convert)
    case g: GroupSym[?]     => Flatten.FGroup(g.alt.alternatives.map(single))
    case o: OptSym[?]       => Flatten.FOpt(sym(o.sym))
    case r: RepSym[?]       => Flatten.FRep(sym(r.sym), r.atLeastOne)
    case s: SepBySym[?]     => Flatten.FSepBy(sym(s.sym), sym(s.sep), s.atLeastOne)
  }
}
