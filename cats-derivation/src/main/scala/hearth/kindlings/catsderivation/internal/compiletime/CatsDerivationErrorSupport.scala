package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*

/** Shared helpers routing macro-time failures into the MIO error channel.
  *
  * Replaces the historical `throw new RuntimeException(...)` calls (which crashed the compiler) with proper
  * [[CatsDerivationError]] failures reported through Hearth's MIO error channel.
  */
private[compiletime] trait CatsDerivationErrorSupport extends hearth.kindlings.derivation.compiletime.MethodFolds {
  this: MacroCommons =>

  import CatsDerivationError.*

  /** Logs the error and fails the MIO computation with it. */
  final protected def failDerivation[A](err: CatsDerivationError): MIO[A] =
    Log.error(err.message) >> MIO.fail(err)

  /** Parses `A` as a case class or fails the derivation with [[CatsDerivationError.CannotParseCaseClass]]. */
  final protected def parseCaseClassMIO[A: Type](label: => String): MIO[CaseClass[A]] =
    CaseClass.parse[A].toEither match {
      case Right(cc) => MIO.pure(cc)
      case Left(e)   => failDerivation(CannotParseCaseClass(label, e.toString))
    }

  /** Parses `A` as an enum or fails the derivation with [[CatsDerivationError.CannotParseEnum]]. */
  final protected def parseEnumMIO[A: Type](label: => String): MIO[Enum[A]] =
    Enum.parse[A].toEither match {
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
