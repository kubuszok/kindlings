package hearth.kindlings.ubjsonderivation.internal.compiletime
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
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.reader)).map { innerDecoded =>
                  Rule.matched(Expr.quote {
                    Expr.splice(wrapLambda).apply(Expr.splice(innerDecoded))
                  })
                }
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

}
