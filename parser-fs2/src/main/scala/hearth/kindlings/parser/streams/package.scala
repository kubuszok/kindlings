package hearth.kindlings.parser

import _root_.fs2.{text, Pipe, Pull, RaiseThrowable}

/** fs2 integration: `import hearth.kindlings.parser.streams._`.
  *
  * {{{
  * bytes.through(parser.bytePipe).compile.lastOrError   // Stream[IO, Byte] -> IO[R]
  * }}}
  *
  * The whole stream is parsed as one input producing one `R`; chunks are fed to the parser as it needs them and
  * already-parsed text is discarded, so arbitrarily large streams are parsed with bounded memory (plus whatever the
  * actions build).
  */
package object streams {

  /** Steps (shifts + reductions) between re-entries into `Pull`. */
  val DefaultBudget: Int = 4096

  implicit final class ParserStreamOps[F[_], R](private val parser: Parser[F, R]) extends AnyVal {

    /** Parses a stream of text chunks; effectful actions (`all(...) { ... }`) are evaluated in `F`. */
    def pipe(implicit rt: RaiseThrowable[F]): Pipe[F, String, R] =
      StreamDriver.pipe[F, F, R](parser, DefaultBudget, effect => Pull.eval(effect.asInstanceOf[F[Any]]))

    /** Parses a stream of UTF-8 bytes (multi-byte chars may be split across chunks). */
    def bytePipe(implicit rt: RaiseThrowable[F]): Pipe[F, Byte, R] = _.through(text.utf8.decode).through(pipe)
  }

  implicit final class PureParserStreamOps[R](private val parser: Parser[Id, R]) extends AnyVal {

    /** Parses a stream of text chunks in any stream effect `G` (the grammar's actions are pure or `Id`). */
    def pipeIn[G[_]](implicit rt: RaiseThrowable[G]): Pipe[G, String, R] =
      StreamDriver.pipe[Id, G, R](parser, DefaultBudget, value => Pull.pure(value))

    /** Parses a stream of UTF-8 bytes in any stream effect `G`. */
    def bytePipeIn[G[_]](implicit rt: RaiseThrowable[G]): Pipe[G, Byte, R] =
      _.through(text.utf8.decode).through(pipeIn[G])
  }
}
