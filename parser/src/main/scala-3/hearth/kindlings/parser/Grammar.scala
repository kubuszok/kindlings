package hearth.kindlings.parser

/** Entry point of the parser DSL: see [[Dsl]] for the grammar syntax. */
object Grammar {

  /** Compiles the grammar defined in `body` into an LALR(1) [[Parser]] producing `F[R]`.
    *
    * The macro reads the grammar at compile time, reports conflicts and other grammar errors at their source positions,
    * and embeds the lexer/parser tables in the generated code; `engine` executes the parser in `F`.
    */
  inline def grammar[R, F[_]](inline body: Dsl[F] => NonTerminal[R])(using engine: ParserEngine[F]): Parser[F, R] =
    ${ internal.compiletime.GrammarMacros.grammarImpl[R, F]('body, 'engine) }
}
