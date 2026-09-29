package hearth.kindlings.parser
package streams

import _root_.fs2.{Pipe, Pull, RaiseThrowable, Stream}
import hearth.kindlings.parser.internal.runtime.Machine

/** Drives a push machine from a stream of text chunks, inside `Pull`: chunks are fed as the machine asks for input
  * (already-parsed text is discarded, so memory stays bounded), effectful actions are evaluated with `effect`, and the
  * machine is allocated when the stream runs.
  */
private[streams] object StreamDriver {

  def pipe[F[_], G[_], R](
      parser: Parser[F, R],
      budget: Int,
      effect: Any => Pull[G, Nothing, Any]
  )(implicit rt: RaiseThrowable[G]): Pipe[G, String, R] =
    input => Stream.suspend(loop[G, R](parser.pushMachine(), input, budget, effect).stream)

  private def loop[G[_], R](
      m: Machine,
      input: Stream[G, String],
      budget: Int,
      effect: Any => Pull[G, Nothing, Any]
  )(implicit rt: RaiseThrowable[G]): Pull[G, R, Unit] =
    (m.run(budget): @scala.annotation.switch) match {
      case Machine.Done   => Pull.output1(m.result.asInstanceOf[R])
      case Machine.Error  => Pull.raiseError[G](m.error)
      case Machine.Effect =>
        effect(m.pendingEffect).flatMap { value =>
          m.resume(value)
          loop[G, R](m, input, budget, effect)
        }
      case Machine.NeedInput =>
        input.pull.uncons.flatMap {
          case Some((chunk, rest)) =>
            chunk.foreach(m.feed)
            loop[G, R](m, rest, budget, effect)
          case None =>
            m.endOfInput()
            loop[G, R](m, Stream.empty, budget, effect)
        }
      case _ => Pull.suspend(loop[G, R](m, input, budget, effect))
    }
}
