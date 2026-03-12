package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

trait EncoderUseImplicitWhenAvailableRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsJsonValueCodec.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to use implicit JsonValueCodec for ${Type[A].prettyPrint}") >> {
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          CTypes.JsonValueCodec[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: EncoderCtx](
        instanceExpr: Expr[JsonValueCodec[A]]
    ): MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Found implicit codec ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).encodeValue(Expr.splice(ectx.value), Expr.splice(ectx.writer))
        }))

    private def yieldUnsupported[A: EncoderCtx](reason: String): MIO[Rule.Applicability[Expr[Unit]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit JsonValueCodec instance: $reason"
        )
      )
  }

}
