package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

trait EncoderUseImplicitWhenAvailableRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to use implicit JsonValueCodec for ${Type[A].prettyPrint}") >> {
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          summonJsonValueCodecCached[A] match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit codec ${instanceExpr.prettyPrint}, using directly") >>
                MIO.pure(Rule.matched(Expr.quote {
                  Expr.splice(instanceExpr).encodeValue(Expr.splice(ectx.value), Expr.splice(ectx.writer))
                }))
            case Left(reason) =>
              implicit val CodecA: Type[JsonValueCodec[A]] = CTypes.JsonValueCodec[A]
              summonJsonValueCodecSearch[A].crashReason match {
                case Some(crash) =>
                  failOnCrashedUserInstance[JsonValueCodec[A], Rule.Applicability[Expr[Unit]]](crash)
                case None =>
                  // a user's generic codec that needs codecs Kindlings derives (e.g. `JsonValueCodec[Page[Item]]`
                  // built from a derived codec of `Item`)
                  completeUserCodecForEncoder[A].map {
                    case Some(instanceExpr) =>
                      Rule.matched(Expr.quote {
                        Expr.splice(instanceExpr).encodeValue(Expr.splice(ectx.value), Expr.splice(ectx.writer))
                      })
                    case None =>
                      Rule.yielded(
                        s"The type ${Type[A].prettyPrint} does not have an implicit JsonValueCodec instance: $reason"
                      )
                  }
              }
          }
      }
  }

}
