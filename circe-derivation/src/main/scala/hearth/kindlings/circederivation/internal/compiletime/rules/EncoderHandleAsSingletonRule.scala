package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.Json

trait EncoderHandleAsSingletonRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsSingletonRule extends EncoderDerivationRule("handle as singleton when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(_) =>
            MIO.pure(Rule.matched(Expr.quote {
              CirceDerivationUtils.jsonFromFields(Nil)
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
