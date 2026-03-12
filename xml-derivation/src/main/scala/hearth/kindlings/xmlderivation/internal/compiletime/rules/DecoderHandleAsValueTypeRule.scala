package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlDecodingError

trait DecoderHandleAsValueTypeRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            implicit val EitherInnerT: Type[Either[XmlDecodingError, Inner]] = DTypes.DecoderResult[Inner]
            isValueType.value.wrap match {
              case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                // Wrap returns Either[String, A] — convert Left(String) to Left(XmlDecodingError)
                implicit val StringT: Type[String] = DTypes.String
                implicit val EitherStringA: Type[Either[String, A]] = DTypes.eitherStringType[A]
                LambdaBuilder
                  .of1[Inner]("inner")
                  .traverse { innerExpr =>
                    val wrapResult = isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, A]]]
                    MIO.pure(Expr.quote {
                      Expr.splice(wrapResult).left.map { (msg: String) =>
                        XmlDecodingError.General(msg): XmlDecodingError
                      }
                    })
                  }
                  .flatMap { builder =>
                    val wrapLambda = builder.build[Either[XmlDecodingError, A]]
                    for {
                      innerResult <- deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.elem))
                    } yield Rule.matched(Expr.quote {
                      Expr.splice(innerResult).flatMap(Expr.splice(wrapLambda))
                    })
                  }

              case _ =>
                // Wrap returns A directly (identity or simple constructor)
                LambdaBuilder
                  .of1[Inner]("inner")
                  .traverse { innerExpr =>
                    MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
                  }
                  .flatMap { builder =>
                    val wrapLambda = builder.build[A]
                    for {
                      innerResult <- deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.elem))
                    } yield Rule.matched(Expr.quote {
                      Expr.splice(innerResult).map(Expr.splice(wrapLambda))
                    })
                  }
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

}
