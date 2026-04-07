package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.ConfigDecodingError
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils

trait ReaderHandleAsValueTypeRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsValueTypeRule extends ReaderDerivationRule("handle as value type when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        implicit val ErrT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
        implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            implicit val EitherInnerT: Type[Either[ConfigDecodingError, Inner]] = RTypes.ReaderResult[Inner]
            isValueType.value.wrap match {
              case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                implicit val StringT: Type[String] = RTypes.String
                implicit val EitherStringA: Type[Either[String, A]] = RTypes.eitherStringType[A]
                val wrapLambda: Expr[Inner => Either[ConfigDecodingError, A]] =
                  buildEitherWrap[A, Inner](isValueType.value)
                deriveReaderRecursively[Inner](using rctx.nest[Inner](rctx.value)).map { innerResult =>
                  Rule.matched(Expr.quote {
                    Expr.splice(innerResult).flatMap(Expr.splice(wrapLambda))
                  })
                }

              case _ =>
                val wrapLambda: Expr[Inner => A] = buildPlainWrap[A, Inner](isValueType.value)
                deriveReaderRecursively[Inner](using rctx.nest[Inner](rctx.value)).map { innerResult =>
                  Rule.matched(Expr.quote {
                    Expr.splice(innerResult).map(Expr.splice(wrapLambda))
                  })
                }
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }

    private def buildEitherWrap[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    ): Expr[Inner => Either[ConfigDecodingError, A]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val ErrT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      @scala.annotation.nowarn("msg=is never used")
      implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
      Expr.quote { (inner: Inner) =>
        Expr
          .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[String, A]]])
          .left
          .map((msg: String) => SConfigDerivationUtils.liftStringError(msg))
      }
    }

    private def buildPlainWrap[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    ): Expr[Inner => A] =
      Expr.quote { (inner: Inner) =>
        Expr.splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[A]])
      }
  }
}
