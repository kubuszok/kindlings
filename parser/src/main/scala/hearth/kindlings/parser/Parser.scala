package hearth.kindlings.parser

import hearth.kindlings.parser.internal.runtime.{CompiledGrammar, Machine, PushInput, ReaderInput, StringInput}

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

  /** [[parse]] without the recursive-descent fast path of LL(1) grammars (for tests comparing both parsers). */
  private[parser] def parseByMachine(input: String): F[R] =
    engine.run[R] { () =>
      val m = new Machine(grammar, new StringInput(input))
      m.skipDescent()
      m
    }

  /** The compiled grammar (for tests). */
  private[parser] def compiled: CompiledGrammar = grammar

  /** Parses a UTF-8 encoded `stream` (see the `Reader` overload). The stream is not closed. */
  def parse(stream: java.io.InputStream): F[R] =
    parse(new java.io.InputStreamReader(stream, java.nio.charset.StandardCharsets.UTF_8), Parser.DefaultBufferSize)

  /** Low-level API for integrations (stream pipes, REPLs, custom engines): a fresh machine whose input is pushed with
    * `machine.feed(chunk)` / `machine.endOfInput()`.
    *
    * Drive it with `machine.run(budget)`:
    *   - `Machine.NeedInput`: feed another chunk or end the input,
    *   - `Machine.Effect`: run `machine.pendingEffect` (an `F[Any]` produced by an effectful action) and continue with
    *     `machine.resume(result)`,
    *   - `Machine.Yield`: the step budget was spent; run again,
    *   - `Machine.Done` / `Machine.Error`: `machine.result` / `machine.error`.
    *
    * A machine is mutable and belongs to one parse.
    */
  def pushMachine(initialBufferSize: Int = Parser.DefaultBufferSize): Machine =
    new Machine(grammar, new PushInput(initialBufferSize))

  /** Display names of the terminals of this grammar, by token id (id 0 is end of input). */
  def terminalNames: List[String] = grammar.tables.tokenNames.toList
}
object Parser {

  /** Default chunk size (in chars) for `Reader`/`InputStream` inputs. */
  val DefaultBufferSize: Int = 64 * 1024
}
