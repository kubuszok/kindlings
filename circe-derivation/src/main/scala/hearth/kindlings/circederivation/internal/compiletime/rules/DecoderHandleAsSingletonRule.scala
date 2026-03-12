package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import io.circe.DecodingFailure

trait DecoderHandleAsSingletonRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsSingletonRule extends DecoderDerivationRule("handle as singleton when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(sv) =>
            implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
            MIO.pure(Rule.matched(Expr.quote {
              Right(Expr.splice(sv.singletonExpr)): Either[DecodingFailure, A]
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
