package hearth.kindlings.parser

import hearth.kindlings.parser.internal.runtime.{CompiledGrammar, Machine}

/** A parser compiled from a `Grammar.grammar[R, F] { ... }` definition.
  *
  * Immutable and safe to share: every `parse` call allocates its own machine.
  */
final class Parser[F[_], R] private[parser] (grammar: CompiledGrammar, engine: ParserEngine[F]) {

  /** Parses the whole `input` (it must match the start symbol exactly, up to skipped tokens). */
  def parse(input: String): F[R] = engine.run[R](new Machine(grammar, input))

  /** Display names of the terminals of this grammar, by token id (id 0 is end of input). */
  def terminalNames: List[String] = grammar.tables.tokenNames.toList
}
