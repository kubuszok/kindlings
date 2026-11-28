package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

trait DecoderUseImplicitWhenAvailableRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use implicit JsonValueCodec for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          summonJsonValueCodecCached[A] match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit codec ${instanceExpr.prettyPrint}, using directly") >>
                MIO.pure(Rule.matched(Expr.quote {
                  Expr.splice(instanceExpr).decodeValue(Expr.splice(dctx.reader), Expr.splice(instanceExpr).nullValue)
                }))
            case Left(reason) =>
              implicit val CodecA: Type[JsonValueCodec[A]] = CTypes.JsonValueCodec[A]
              summonJsonValueCodecSearch[A].crashReason match {
                case Some(crash) =>
                  failOnCrashedUserInstance[JsonValueCodec[A], Rule.Applicability[Expr[A]]](crash)
                case None =>
                  // a user's generic codec that needs codecs Kindlings derives (e.g. `JsonValueCodec[Page[Item]]`
                  // built from a derived codec of `Item`)
                  completeUserCodecForDecoder[A].map {
                    case Some(instanceExpr) =>
                      Rule.matched(Expr.quote {
                        Expr
                          .splice(instanceExpr)
                          .decodeValue(Expr.splice(dctx.reader), Expr.splice(instanceExpr).nullValue)
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
