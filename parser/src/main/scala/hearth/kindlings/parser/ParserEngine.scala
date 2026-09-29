package hearth.kindlings.parser

import hearth.kindlings.parser.internal.runtime.Machine

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** Executes a compiled grammar in the effect `F`: drives the resumable LR [[Machine]], sequences the `F[R]` returned by
  * effectful actions (`all(...) { ... }`) and represents syntax errors in `F`.
  *
  * Pure actions (`all(...).pure { ... }`) never reach the engine: the machine runs them inline. An engine is only
  * consulted when the machine stops on an effectful action (`Machine.Effect`), finishes (`Machine.Done`) or fails
  * (`Machine.Error`).
  *
  * Instances for Scala's built-in effects live in the companion; other runtimes (cats-effect, ZIO, ...) provide
  * instances from their own modules.
  */
@scala.annotation.implicitNotFound(
  "No ParserEngine for ${F}: built-in engines exist for Id, Option, Try, Either[E, *] (with a ParseErrorLift[E]) and Future (with an implicit ExecutionContext); other effects need an engine from their integration module"
)
trait ParserEngine[F[_]] {

  /** Runs `machine` to completion in `F`. The machine is freshly allocated for this parse and used by this call only.
    */
  def run[R](machine: Machine): F[R]
}
object ParserEngine {

  /** Effectful actions return the value directly; syntax errors are thrown as [[ParseError]]. */
  implicit val id: ParserEngine[Id] = new ParserEngine[Id] {
    def run[R](m: Machine): R = {
      while (true)
        (m.run(): @scala.annotation.switch) match {
          case Machine.Done   => return m.result.asInstanceOf[R]
          case Machine.Error  => throw m.error
          case Machine.Effect => m.resume(m.pendingEffect)
        }
      throw new IllegalStateException("unreachable")
    }
  }

  /** An effectful action returning `None` stops the parse with `None`; syntax errors become `None` too. */
  implicit val option: ParserEngine[Option] = new ParserEngine[Option] {
    def run[R](m: Machine): Option[R] = {
      while (true)
        (m.run(): @scala.annotation.switch) match {
          case Machine.Done   => return Some(m.result.asInstanceOf[R])
          case Machine.Error  => return None
          case Machine.Effect =>
            m.pendingEffect.asInstanceOf[Option[Any]] match {
              case Some(value) => m.resume(value)
              case None        => return None
            }
        }
      None
    }
  }

  /** An effectful action returning `Failure` stops the parse with it; syntax errors become `Failure(ParseError)`;
    * non-fatal exceptions thrown by actions are captured as `Failure`.
    */
  implicit val tryEngine: ParserEngine[Try] = new ParserEngine[Try] {
    def run[R](m: Machine): Try[R] =
      try {
        while (true)
          (m.run(): @scala.annotation.switch) match {
            case Machine.Done   => return Success(m.result.asInstanceOf[R])
            case Machine.Error  => return Failure(m.error)
            case Machine.Effect =>
              m.pendingEffect.asInstanceOf[Try[Any]] match {
                case Success(value) => m.resume(value)
                case f: Failure[?]  => return f.asInstanceOf[Try[R]]
              }
          }
        Failure(new IllegalStateException("unreachable"))
      } catch { case NonFatal(e) => Failure(e) }
  }

  /** An effectful action returning `Left` stops the parse with it; syntax errors are lifted with [[ParseErrorLift]]. */
  implicit def either[E](implicit lift: ParseErrorLift[E]): ParserEngine[({ type L[A] = Either[E, A] })#L] =
    new ParserEngine[({ type L[A] = Either[E, A] })#L] {
      def run[R](m: Machine): Either[E, R] = {
        while (true)
          (m.run(): @scala.annotation.switch) match {
            case Machine.Done   => return Right(m.result.asInstanceOf[R])
            case Machine.Error  => return Left(lift.lift(m.error))
            case Machine.Effect =>
              m.pendingEffect.asInstanceOf[Either[E, Any]] match {
                case Right(value)  => m.resume(value)
                case l: Left[?, ?] => return l.asInstanceOf[Either[E, R]]
              }
          }
        Left(lift.lift(m.error))
      }
    }

  /** Pure parsing runs synchronously until an effectful action returns a `Future`; the machine resumes in that future's
    * continuation (one `flatMap` per effectful action, nothing per token). Syntax errors fail the future with
    * [[ParseError]].
    */
  implicit def future(implicit ec: ExecutionContext): ParserEngine[Future] = new ParserEngine[Future] {
    def run[R](m: Machine): Future[R] = {
      def loop(): Future[R] =
        (m.run(): @scala.annotation.switch) match {
          case Machine.Done  => Future.successful(m.result.asInstanceOf[R])
          case Machine.Error => Future.failed(m.error)
          case _             =>
            m.pendingEffect.asInstanceOf[Future[Any]].flatMap { value =>
              m.resume(value)
              loop()
            }
        }
      try loop()
      catch { case NonFatal(e) => Future.failed(e) }
    }
  }
}
