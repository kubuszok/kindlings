package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.{ConfigCodec, ConfigWriter}
import org.ekrich.config.ConfigValue

trait WriterUseImplicitWhenAvailableRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterUseImplicitWhenAvailableRule extends WriterDerivationRule("use implicit when available") {

    /** See [[ReaderUseImplicitWhenAvailableRule.ignoredImplicits]] for the rationale. */
    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[ConfigWriter.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      } ++ Type.of[ConfigCodec.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to use implicit ConfigWriter for ${Type[A].prettyPrint}") >> {
        if (wctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          WTypes.ConfigWriter[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: WriterCtx](
        instanceExpr: Expr[ConfigWriter[A]]
    ): MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Found implicit writer ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).to(Expr.splice(wctx.value))
        }))

    private def yieldUnsupported[A: WriterCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[ConfigValue]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit ConfigWriter instance: $reason"
        )
      )
  }
}
