package hearth.kindlings.parser

/** Inline string literals in grammars: a literal is a terminal whose value is the literal itself (a singleton value).
  * `""` as a whole alternative is the empty alternative.
  */
trait DslLiterals {

  implicit def litSym[S <: String with Singleton](literal: S): Sym[S] = Terminal.literal(literal)

  implicit def litAlt[S <: String with Singleton](literal: S): Alt[S] = Alt.literal(literal)
}
