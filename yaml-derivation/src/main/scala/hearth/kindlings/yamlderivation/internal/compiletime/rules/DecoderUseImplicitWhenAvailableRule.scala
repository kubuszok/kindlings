package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.yamlderivation.{KindlingsYamlCodec, KindlingsYamlDecoder}
import org.virtuslab.yaml.ConstructError

trait DecoderUseImplicitWhenAvailableRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[KindlingsYamlDecoder.type].methods.collect {
        case method if method.value.isImplicit => method.value.asUntyped
      } ++ Type.of[KindlingsYamlCodec.type].methods.collect {
        case method if method.value.isImplicit => method.value.asUntyped
      }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to use implicit YamlDecoder for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          DTypes.YamlDecoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: DecoderCtx](
        instanceExpr: Expr[org.virtuslab.yaml.YamlDecoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Found implicit decoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).construct(Expr.splice(dctx.node))(org.virtuslab.yaml.LoadSettings.empty)
        }))

    private def yieldUnsupported[A: DecoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit YamlDecoder instance: $reason"
        )
      )
  }
}
