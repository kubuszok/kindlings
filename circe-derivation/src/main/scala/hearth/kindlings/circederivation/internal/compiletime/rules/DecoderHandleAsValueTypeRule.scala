package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import io.circe.DecodingFailure

trait DecoderHandleAsValueTypeRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner

            // Summon a Decoder[Inner]
            DTypes
              .Decoder[Inner]
              .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
              .toEither match {
              case Right(innerDecoder) =>
                isValueType.value.wrap match {
                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    // Wrap returns Either[String, A] — convert Left(String) to Left(DecodingFailure)
                    @scala.annotation.nowarn("msg=is never used")
                    implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
                    LambdaBuilder
                      .of1[Inner]("inner")
                      .traverse { innerExpr =>
                        val wrapResult = isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, A]]]
                        MIO.pure(Expr.quote {
                          Expr.splice(wrapResult).left.map { (msg: String) =>
                            io.circe.DecodingFailure(msg, Expr.splice(dctx.cursor).history)
                          }
                        })
                      }
                      .map { builder =>
                        val wrapLambda = builder.build[Either[DecodingFailure, A]]
                        Rule.matched(Expr.quote {
                          Expr.splice(innerDecoder).apply(Expr.splice(dctx.cursor)).flatMap(Expr.splice(wrapLambda))
                        })
                      }
                  case _ =>
                    // PlainValue — original behavior
                    LambdaBuilder
                      .of1[Inner]("inner")
                      .traverse { innerExpr =>
                        MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
                      }
                      .map { builder =>
                        val wrapLambda = builder.build[A]
                        Rule.matched(Expr.quote {
                          Expr.splice(innerDecoder).apply(Expr.splice(dctx.cursor)).map(Expr.splice(wrapLambda))
                        })
                      }
                }
              case Left(reason) =>
                MIO.pure(Rule.yielded(s"Value type inner ${Type[Inner].prettyPrint} has no Decoder: $reason"))
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }
}
