package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
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
            // Per project rule 5, [[LambdaBuilder]] is reserved for lambdas passed to
            // collection / Optional iteration helpers. The wrap function we build here is a
            // pre-derived `Inner => ...` value spliced once into a single `Expr.quote`, so we
            // use a direct cross-quotes function literal instead.
            isValueType.value.wrap match {
              case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                // Wrap returns Either[String, A] — convert Left(String) to Left(XmlDecodingError)
                implicit val StringT: Type[String] = DTypes.String
                implicit val EitherStringA: Type[Either[String, A]] = DTypes.eitherStringType[A]
                val wrapLambda: Expr[Inner => Either[XmlDecodingError, A]] =
                  buildEitherWrap[A, Inner](isValueType.value)
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.elem)).map { innerResult =>
                  Rule.matched(Expr.quote {
                    Expr.splice(innerResult).flatMap(Expr.splice(wrapLambda))
                  })
                }

              case _ =>
                // Wrap returns A directly (identity or simple constructor)
                val wrapLambda: Expr[Inner => A] = buildPlainWrap[A, Inner](isValueType.value)
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.elem)).map { innerResult =>
                  Rule.matched(Expr.quote {
                    Expr.splice(innerResult).map(Expr.splice(wrapLambda))
                  })
                }
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }

    /** Build an `Inner => Either[XmlDecodingError, A]` lambda for value types whose `wrap` returns `Either[String, A]`.
      * Extracted as a helper because the `wrap` closure captures path-dependent state from the [[IsValueTypeOf]]
      * instance.
      */
    private def buildEitherWrap[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    ): Expr[Inner => Either[XmlDecodingError, A]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      @scala.annotation.nowarn("msg=is never used")
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      Expr.quote { (inner: Inner) =>
        Expr
          .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[String, A]]])
          .left
          .map((msg: String) => XmlDecodingError.General(msg): XmlDecodingError)
      }
    }

    /** Build an `Inner => A` lambda for value types whose `wrap` is total. */
    private def buildPlainWrap[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    ): Expr[Inner => A] =
      Expr.quote { (inner: Inner) =>
        Expr.splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[A]])
      }
  }

}
