package hearth.kindlings.parser

/** A compile-time option of a grammar, set inside the grammar block with `enable(flag)` / `disable(flag)` (the flags
  * are also members of the DSL, so `import g._` brings them into scope). Flags are read by the macro only; at run time
  * `enable`/`disable` do nothing.
  */
sealed abstract class GrammarFlag(val name: String) {
  override def toString: String = name
}
object GrammarFlag {

  /** Allows values that can be rejected after they were parsed (repetitions collected with `.as[C]` into a collection
    * with a smart constructor, e.g. cats `NonEmptyList`) in effects without an error channel, such as `Id`: the
    * rejection is then thrown as a [[ParseError]], like syntax errors are. Without it, such grammars need an `F` with
    * an [[ErrorChannel]].
    */
  case object ThrowingInRuntime extends GrammarFlag("ThrowingInRuntime")

  /** Parses `String` inputs with a generated top-down parser, and fails the compilation unless the grammar is LL(1) -
    * parseable by looking at one token ahead, top down - explaining why it is not (like `@tailrec` does for tail
    * calls). The top-down parser reports only the tokens valid in the current context; on JSON it is currently ~14%
    * slower than the LALR(1) parser, which is the default.
    */
  case object RequireLL1 extends GrammarFlag("RequireLL1")

  /** Uses the LALR(1) parser (the default; this states it explicitly). */
  case object RequireLALR extends GrammarFlag("RequireLALR")

  val all: List[GrammarFlag] = List(ThrowingInRuntime, RequireLL1, RequireLALR)
}
