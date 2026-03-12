package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.virtuslab.yaml.ConstructError

trait DecoderHandleAsSingletonRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsSingletonRule extends DecoderDerivationRule("handle as singleton when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(sv) =>
            implicit val EitherT: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
            MIO.pure(Rule.matched(Expr.quote {
              Right(Expr.splice(sv.singletonExpr)): Either[ConstructError, A]
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
