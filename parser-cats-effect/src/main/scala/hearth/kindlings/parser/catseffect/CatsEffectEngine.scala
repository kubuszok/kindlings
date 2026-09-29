package hearth.kindlings.parser
package catseffect

import cats.effect.kernel.{Async, Sync}
import hearth.kindlings.parser.internal.runtime.Machine

/** Runs `kindlings-parser` grammars in a Cats Effect `F`.
  *
  *   - Pure actions, lexing and all shifts/reductions run inside one `Sync.delay` segment of at most `budget` steps.
  *   - An effectful action costs exactly one `flatMap`: the next pure segment runs directly in its continuation.
  *   - After `budget` steps the engine `cede`s (with `Async`), so long parses do not starve other fibers.
  *   - `Reader`/`InputStream` inputs are read with `Sync.blocking`.
  *   - The machine is allocated when the returned `F[R]` runs, so running it twice parses twice.
  */
final class CatsEffectEngine[F[_]] private (budget: Int, cede: Option[F[Unit]])(implicit F: Sync[F])
    extends ParserEngine[F] {

  def run[R](machine: () => Machine): F[R] = F.defer(step[R](machine()))

  private def step[R](m: Machine): F[R] = (m.run(budget): @scala.annotation.switch) match {
    case Machine.Done   => F.pure(m.result.asInstanceOf[R])
    case Machine.Error  => F.raiseError(m.error)
    case Machine.Effect =>
      F.flatMap(m.pendingEffect.asInstanceOf[F[Any]]) { value =>
        m.resume(value)
        step[R](m)
      }
    case Machine.NeedInput => F.flatMap(F.blocking(m.refill()))(_ => step[R](m))
    case _                 =>
      cede match {
        case Some(c) => F.flatMap(c)(_ => step[R](m))
        case None    => F.defer(step[R](m))
      }
  }
}
object CatsEffectEngine {

  /** Steps (shifts + reductions) between cedes: roughly tens of microseconds of parsing, following Cats Effect's
    * guidance that work longer than ~10µs without a yield is "expensive".
    */
  val DefaultBudget: Int = 4096

  /** An engine for `Async[F]`: cedes every `budget` steps. */
  def async[F[_]](budget: Int = DefaultBudget)(implicit F: Async[F]): ParserEngine[F] =
    new CatsEffectEngine[F](budget, Some(F.cede))

  /** An engine for `Sync[F]` (no `cede` available: segments are chained with `defer`). */
  def sync[F[_]](budget: Int = DefaultBudget)(implicit F: Sync[F]): ParserEngine[F] =
    new CatsEffectEngine[F](budget, None)
}
