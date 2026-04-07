package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import pureconfig.ConfigCursor
import pureconfig.error.{CannotConvert, ConfigReaderFailures, ConvertFailure}

trait ReaderHandleAsValueTypeRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsValueTypeRule extends ReaderDerivationRule("handle as value type when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
        implicit val EitherT: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]
        implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
        val cursorExpr = rctx.cursor
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            implicit val EitherInnerT: Type[Either[ConfigReaderFailures, Inner]] = RTypes.ReaderResult[Inner]
            isValueType.value.wrap match {
              case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                implicit val StringT: Type[String] = RTypes.String
                implicit val EitherStringA: Type[Either[String, A]] = RTypes.eitherStringType[A]
                val wrapLambda: Expr[Inner => Either[ConfigReaderFailures, A]] =
                  buildEitherWrap[A, Inner](isValueType.value, cursorExpr)
                deriveReaderRecursively[Inner](using rctx.nest[Inner](rctx.cursor)).map { innerResult =>
                  Rule.matched(Expr.quote {
                    Expr.splice(innerResult).flatMap(Expr.splice(wrapLambda))
                  })
                }

              case _ =>
                val wrapLambda: Expr[Inner => A] = buildPlainWrap[A, Inner](isValueType.value)
                deriveReaderRecursively[Inner](using rctx.nest[Inner](rctx.cursor)).map { innerResult =>
                  Rule.matched(Expr.quote {
                    Expr.splice(innerResult).map(Expr.splice(wrapLambda))
                  })
                }
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }

    /** Build an `Inner => Either[ConfigReaderFailures, A]` lambda for value types whose `wrap`
      * returns `Either[String, A]` (e.g. refined types). The path-dependent inner type is
      * isolated behind a regular type parameter so it never appears inside an `Expr.quote` body.
      *
      * The `cursorExpr` parameter is captured from the enclosing context's `rctx.cursor` and
      * used inside the quote to materialise origin/path metadata for the failure record.
      */
    private def buildEitherWrap[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner],
        cursorExpr: Expr[ConfigCursor]
    ): Expr[Inner => Either[ConfigReaderFailures, A]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
      @scala.annotation.nowarn("msg=is never used")
      implicit val EitherT: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]
      @scala.annotation.nowarn("msg=is never used")
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      Expr.quote { (inner: Inner) =>
        Expr
          .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[String, A]]])
          .left
          .map((msg: String) =>
            ConfigReaderFailures(
              ConvertFailure(
                reason = CannotConvert(
                  value = Expr.splice(cursorExpr).valueOpt.map(_.render).getOrElse(""),
                  toType = Expr.splice(Expr(Type[A].prettyPrint)),
                  because = msg
                ),
                origin = Expr.splice(cursorExpr).origin,
                path = Expr.splice(cursorExpr).path
              )
            )
          )
      }
    }

    /** Build an `Inner => A` lambda for value types whose `wrap` is total (no error path). */
    private def buildPlainWrap[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    ): Expr[Inner => A] =
      Expr.quote { (inner: Inner) =>
        Expr.splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[A]])
      }
  }
}
