package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait DecoderHandleAsValueTypeRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner

            // Build wrap lambda outside quotes to avoid staging issues with wrap.Result type
            // For EitherStringOrValue wraps, handle the Either and throw on Left
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                isValueType.value.wrap match {
                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    val wrapResult = isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, A]]]
                    MIO.pure(Expr.quote {
                      Expr.splice(wrapResult) match {
                        case scala.Right(v)  => v
                        case scala.Left(msg) => Expr.splice(dctx.reader).decodeError(msg)
                      }
                    })
                  case _ =>
                    MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
                }
              }
              .flatMap { builder =>
                val wrapLambda = builder.build[A]
                // Try implicit first, fall back to recursive derivation (includes built-in types)
                summonJsonValueCodecCached[Inner] match {
                  case Right(innerCodec) =>
                    MIO.pure(Rule.matched(Expr.quote {
                      Expr
                        .splice(wrapLambda)
                        .apply(
                          Expr
                            .splice(innerCodec)
                            .decodeValue(Expr.splice(dctx.reader), Expr.splice(innerCodec).nullValue)
                        )
                    }))
                  case Left(_) =>
                    // No implicit — derive via recursive rules (includes built-in types)
                    deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.reader)).map { innerDecoded =>
                      Rule.matched(Expr.quote {
                        Expr.splice(wrapLambda).apply(Expr.splice(innerDecoded))
                      })
                    }
                }
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

}
