package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.{ConfigCodec, ConfigDecodingError, ConfigReader}

trait ReaderUseImplicitWhenAvailableRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderUseImplicitWhenAvailableRule extends ReaderDerivationRule("use implicit when available") {

    /** Filter out our own auto-derivation entry points (`derived` methods) so that the
      * macro never finds and reuses them, which would cause infinite expansion. We
      * deliberately do NOT filter the built-in primitive / collection instances on the
      * companions — those are exactly what we want implicit search to find first.
      *
      * Filter strictly by `isImplicit && name == "derived"` so we never accidentally drop
      * a non-implicit `derive` helper or one of the built-in instances.
      */
    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[ConfigReader.type].methods.collect {
        case method if method.value.isImplicit && method.value.name == "derived" => method.value.asUntyped
      } ++ Type.of[ConfigCodec.type].methods.collect {
        case method if method.value.isImplicit && method.value.name == "derived" => method.value.asUntyped
      }

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to use implicit ConfigReader for ${Type[A].prettyPrint}") >> {
        if (rctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          RTypes.ConfigReader[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: ReaderCtx](
        instanceExpr: Expr[ConfigReader[A]]
    ): MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Found implicit reader ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).from(Expr.splice(rctx.value))
        }))

    private def yieldUnsupported[A: ReaderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit ConfigReader instance: $reason"
        )
      )
  }
}
