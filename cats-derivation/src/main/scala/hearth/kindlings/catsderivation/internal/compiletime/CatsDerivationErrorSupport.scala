package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*

/** Shared helpers routing macro-time failures into the MIO error channel.
  *
  * Replaces the historical `throw new RuntimeException(...)` calls (which crashed the compiler) with proper
  * [[CatsDerivationError]] failures reported through Hearth's MIO error channel.
  */
private[compiletime] trait CatsDerivationErrorSupport
    extends hearth.kindlings.derivation.compiletime.MethodFolds
    with hearth.kindlings.derivation.compiletime.JavaEnumSupport
    with hearth.kindlings.derivation.compiletime.CaseFieldValuesSupport {
  this: MacroCommons =>

  import CatsDerivationError.*

  /** Logs the error and fails the MIO computation with it. */
  final protected def failDerivation[A](err: CatsDerivationError): MIO[A] =
    logDerivationError(err.message) >> MIO.fail(err)

  private var derivationErrorsAreTentative: Boolean = false

  /** Logs a derivation error - as information while [[tentatively]] runs an attempt that another derivation replaces
    * when it fails (error logs of a successful expansion are reported as a warning).
    */
  final protected def logDerivationError(message: => String): MIO[Unit] =
    if (derivationErrorsAreTentative) Log.info(message) else Log.error(message)

  /** Runs `attempt` with its error logs demoted to information. */
  final protected def tentatively[A](attempt: => MIO[A]): MIO[A] =
    MIO.defer {
      val before = derivationErrorsAreTentative
      derivationErrorsAreTentative = true
      MIO.defer(attempt).attemptTap(_ => derivationErrorsAreTentative = before)
    }

  /** Parses `A` as a case class or fails the derivation with [[CatsDerivationError.CannotParseCaseClass]]. */
  final protected def parseCaseClassMIO[A: Type](label: => String): MIO[CaseClass[A]] =
    CaseClass.parse[A].toEither match {
      case Right(cc) => MIO.pure(cc)
      case Left(e)   => failDerivation(CannotParseCaseClass(label, e.toString))
    }

  /** Parses `A` as an enum or fails the derivation with [[CatsDerivationError.CannotParseEnum]]. */
  final protected def parseEnumMIO[A: Type](label: => String): MIO[Enum[A]] =
    parseEnum[A].toEither match {
      case Right(e)  => MIO.pure(e)
      case Left(err) => failDerivation(CannotParseEnum(label, err.toString))
    }

  /** Calls an instance-free method (usually the primary constructor) with the given arguments, failing the derivation
    * with [[CatsDerivationError.CannotConstructResult]] on any error (including the impossible instance branch).
    */
  final protected def constructInstanceFree(method: Method, what: String, tpeName: => String)(
      values: Map[String, Expr_??]
  ): MIO[Expr_??] =
    foldInstanceFree(method, what)(onTypes = _ => Map.empty, onValues = _ => values) match {
      case Right(expr) => MIO.pure(expr)
      case Left(error) => failDerivation(CannotConstructResult(tpeName, error))
    }
}
