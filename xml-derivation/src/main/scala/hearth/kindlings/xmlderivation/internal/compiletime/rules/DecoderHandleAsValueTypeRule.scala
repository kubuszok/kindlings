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
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                isValueType.value.wrap match {
                  case _: CtorLikeOf.PlainValue[?, ?] =>
                    val wrapped = isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]]
                    MIO.pure(Expr.quote(Right(Expr.splice(wrapped)): Either[XmlDecodingError, A]))
                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    val wrapResult =
                      isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, A]]]
                    MIO.pure(Expr.quote {
                      Expr
                        .splice(wrapResult)
                        .left
                        .map((msg: String) => XmlDecodingError.General(msg): XmlDecodingError)
                    })
                  case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                    val wrapResult =
                      isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Iterable[String], A]]]
                    MIO.pure(Expr.quote {
                      Expr
                        .splice(wrapResult)
                        .left
                        .map((errs: Iterable[String]) =>
                          XmlDecodingError.General(errs.mkString("\n")): XmlDecodingError
                        )
                    })
                  case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                    val wrapResult =
                      isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Throwable, A]]]
                    MIO.pure(Expr.quote {
                      Expr
                        .splice(wrapResult)
                        .left
                        .map((err: Throwable) => XmlDecodingError.General(err.getMessage): XmlDecodingError)
                    })
                  case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                    val wrapResult =
                      isValueType.value.wrap
                        .apply(innerExpr)
                        .asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                    MIO.pure(Expr.quote {
                      Expr
                        .splice(wrapResult)
                        .left
                        .map((errs: Iterable[Throwable]) =>
                          XmlDecodingError.General(errs.map(_.getMessage).mkString("\n")): XmlDecodingError
                        )
                    })
                }
              }
              .flatMap { builder =>
                val wrapLambda = builder.build[Either[XmlDecodingError, A]]
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.elem)).map { innerResult =>
                  Rule.matched(Expr.quote {
                    Expr.splice(innerResult).flatMap(Expr.splice(wrapLambda))
                  })
                }
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }

}
