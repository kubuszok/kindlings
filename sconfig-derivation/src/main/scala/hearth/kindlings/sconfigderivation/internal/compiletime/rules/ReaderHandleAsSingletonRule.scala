package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.ConfigDecodingError
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.ConfigValue

trait ReaderHandleAsSingletonRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsSingletonRule extends ReaderDerivationRule("handle as singleton when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(sv) =>
            implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
            @scala.annotation.nowarn("msg=is never used")
            implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
            @scala.annotation.nowarn("msg=is never used")
            implicit val BooleanT: Type[Boolean] = RTypes.Boolean
            // Like upstream PureConfig's derived reader of a case object: the value must be an object, and a strict
            // `ProductHint` (`allowUnknownKeys = false`) rejects every key of it. (Subtypes of a sealed family are
            // read by the enum rule, which knows how the family is encoded.)
            val (staticAllowUnknownKeys, allowUnknownKeysExpr) = ReaderHandleAsCaseClassRule.allowUnknownKeysOf[A]
            MIO.pure(
              Rule.matched(
                if (staticAllowUnknownKeys.contains(true))
                  Expr.quote {
                    SConfigDerivationUtils.readObjectAs[A](Expr.splice(rctx.value), Expr.splice(sv.singletonExpr))
                  }
                else
                  Expr.quote {
                    SConfigDerivationUtils.readEmptyProduct[A](
                      Expr.splice(rctx.value),
                      Expr.splice(sv.singletonExpr),
                      Expr.splice(allowUnknownKeysExpr)
                    )
                  }
              )
            )
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
