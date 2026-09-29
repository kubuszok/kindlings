package hearth.kindlings.parser

import scala.language.experimental.macros

/** Entry point of the parser DSL: see [[Dsl]] for the grammar syntax. */
object Grammar {

  /** Compiles the grammar defined in `body` into an LALR(1) [[Parser]] producing `F[R]`.
    *
    * The macro reads the grammar at compile time, reports conflicts and other grammar errors at their source positions,
    * embeds the lexer/parser tables in the generated code and generates the code of the actions (inlined into one
    * `switch`); `engine` executes the parser in `F`.
    */
  def grammar[R, F[_]](body: Dsl[F] => NonTerminal[R])(implicit engine: ParserEngine[F]): Parser[F, R] =
    macro internal.compiletime.GrammarMacros.grammarImpl[R, F]

  /** Like [[grammar]], but the actions are not inlined: the grammar block is evaluated once at run time and the actions
    * are called as function values. Slower; useful as a fallback and as a baseline for benchmarks.
    */
  def interpreted[R, F[_]](body: Dsl[F] => NonTerminal[R])(implicit engine: ParserEngine[F]): Parser[F, R] =
    macro internal.compiletime.GrammarMacros.interpretedImpl[R, F]
}
