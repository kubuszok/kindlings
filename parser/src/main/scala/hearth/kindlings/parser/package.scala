package hearth.kindlings

/** Yacc-style BNF grammars compiled at compile time into LALR(1) parsers.
  *
  * {{{
  * import hearth.kindlings.parser.*
  *
  * val calc: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
  *   import g.*
  *   val expr = nonTerminal[Int]
  *   val num  = terminal("[0-9]+").map(_.toInt)
  *   skip("[ \\t]+")
  *   left("+", "-"); left("*", "/")
  *   expr ::= (
  *        all(expr, "+", expr).pure { (a, _, b) => a + b }
  *     || all(expr, "*", expr).pure { (a, _, b) => a * b }
  *     || all(num).pure(n => n)
  *   )
  *   expr
  * }
  * }}}
  */
package object parser {

  /** The identity effect: `Parser[Id, R].parse` returns `R` directly and throws [[ParseError]] on failure. */
  type Id[A] = A
}
