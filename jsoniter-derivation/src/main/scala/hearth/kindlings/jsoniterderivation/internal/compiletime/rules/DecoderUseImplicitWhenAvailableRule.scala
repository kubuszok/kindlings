package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

trait DecoderUseImplicitWhenAvailableRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsJsonValueCodec.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use implicit JsonValueCodec for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          CTypes.JsonValueCodec[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: DecoderCtx](
        instanceExpr: Expr[JsonValueCodec[A]]
    ): MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Found implicit codec ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).decodeValue(Expr.splice(dctx.reader), Expr.splice(instanceExpr).nullValue)
        }))

    private def yieldUnsupported[A: DecoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[A]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit JsonValueCodec instance: $reason"
        )
      )
  }

}
