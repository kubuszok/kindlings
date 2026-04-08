package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.ConfigDecodingError

trait ReaderHandleAsSingletonRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsSingletonRule extends ReaderDerivationRule("handle as singleton when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(sv) =>
            implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
            MIO.pure(Rule.matched(Expr.quote {
              Right(Expr.splice(sv.singletonExpr)): Either[ConfigDecodingError, A]
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
