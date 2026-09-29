package hearth.kindlings.parser

import hearth.kindlings.parser.internal.runtime.{CompiledGrammar, Machine, ReaderInput, StringInput}

/** A parser compiled from a `Grammar.grammar[R, F] { ... }` definition.
  *
  * Immutable and safe to share. Every `parse` allocates its own machine when the returned `F[R]` is run (for lazy
  * effects: every time it is run).
  */
final class Parser[F[_], R] private[parser] (grammar: CompiledGrammar, engine: ParserEngine[F]) {

  /** Parses the whole `input` (it must match the start symbol exactly, up to skipped tokens). The text is read in
    * place; token values are copied only when they are shifted.
    */
  def parse(input: String): F[R] = engine.run[R](() => new Machine(grammar, new StringInput(input)))

  /** Parses everything `reader` produces, reading it in chunks of `bufferSize` chars. Already parsed text is discarded
    * as parsing progresses, so memory stays bounded by the buffer and the longest token, whatever the input size. The
    * reader is not closed.
    */
  def parse(reader: java.io.Reader, bufferSize: Int = Parser.DefaultBufferSize): F[R] =
    engine.run[R](() => new Machine(grammar, new ReaderInput(reader, bufferSize)))

  /** Parses a UTF-8 encoded `stream` (see the `Reader` overload). The stream is not closed. */
  def parse(stream: java.io.InputStream): F[R] =
    parse(new java.io.InputStreamReader(stream, java.nio.charset.StandardCharsets.UTF_8), Parser.DefaultBufferSize)

  /** Display names of the terminals of this grammar, by token id (id 0 is end of input). */
  def terminalNames: List[String] = grammar.tables.tokenNames.toList
}
object Parser {

  /** Default chunk size (in chars) for `Reader`/`InputStream` inputs. */
  val DefaultBufferSize: Int = 64 * 1024
}
