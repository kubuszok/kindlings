package hearth.kindlings.parser

import scala.concurrent.Future
import scala.util.Try

/** Evidence that parsers in `F` report failures as values of `F` (`None`, `Failure`, `Left`, a failed `Future`, a
  * raised error, ...) instead of throwing them.
  *
  * Grammars whose values can be rejected after they were parsed - repetitions collected with `.as[C]` into a collection
  * with a smart constructor, such as cats `NonEmptyList` - compile only in such an `F`, so that the rejection goes
  * through the error channel instead of being thrown from `Id` code.
  *
  * Throwing the rejection instead (as a [[ParseError]], like the `Id` engine throws syntax errors) is an explicit
  * opt-in: `import hearth.kindlings.parser.ErrorChannel.throwing._`.
  */
@scala.annotation.implicitNotFound(
  "${F} has no error channel for rejected values: use an effect such as Option, Try, Either[E, *] (with a ParseErrorLift[E]), Future or a cats-effect F, or opt into throwing them with `import hearth.kindlings.parser.ErrorChannel.throwing._`"
)
trait ErrorChannel[F[_]]
object ErrorChannel {

  private object Instance extends ErrorChannel[Option]
  private def instance[F[_]]: ErrorChannel[F] = Instance.asInstanceOf[ErrorChannel[F]]

  implicit val option: ErrorChannel[Option] = instance[Option]
  implicit val tryChannel: ErrorChannel[Try] = instance[Try]
  implicit val future: ErrorChannel[Future] = instance[Future]
  @scala.annotation.nowarn("msg=unused")
  implicit def either[E](implicit lift: ParseErrorLift[E]): ErrorChannel[({ type L[A] = Either[E, A] })#L] =
    instance[({ type L[A] = Either[E, A] })#L]

  /** For integrations whose engines report failures in `F` (e.g. cats-effect). */
  def assumed[F[_]]: ErrorChannel[F] = instance[F]

  /** Explicit opt-in (`import hearth.kindlings.parser.ErrorChannel.throwing._`): accepts rejectable values in any `F`,
    * including `Id`, whose engine then throws the rejection as a [[ParseError]].
    */
  object throwing {
    implicit def throwingErrorChannel[F[_]]: ErrorChannel[F] = instance[F]
  }
}
