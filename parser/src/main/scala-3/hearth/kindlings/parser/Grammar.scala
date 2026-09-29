package hearth.kindlings.parser

/** Entry point of the parser DSL: see [[Dsl]] for the grammar syntax. */
object Grammar {

  /** Compiles the grammar defined in `body` into an LALR(1) [[Parser]] producing `F[R]`.
    *
    * The macro reads the grammar at compile time, reports conflicts and other grammar errors at their source positions,
    * embeds the lexer/parser tables in the generated code and generates the code of the actions (inlined into one
    * `switch`); `engine` executes the parser in `F`.
    */
  inline def grammar[R, F[_]](inline body: Dsl[F] => NonTerminal[R])(using engine: ParserEngine[F]): Parser[F, R] =
    ${ internal.compiletime.GrammarMacros.grammarImpl[R, F]('body, 'engine) }

  /** Like [[grammar]], but the actions are not inlined: the grammar block is evaluated once at run time and the actions
    * are called as function values. Slower; useful as a fallback and as a baseline for benchmarks.
    */
  inline def interpreted[R, F[_]](inline body: Dsl[F] => NonTerminal[R])(using
      engine: ParserEngine[F]
  ): Parser[F, R] =
    ${ internal.compiletime.GrammarMacros.interpretedImpl[R, F]('body, 'engine) }
}
