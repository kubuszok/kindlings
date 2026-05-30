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
        if (Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is a named tuple, not a value type"))
        else
          Type[A] match {
            case IsValueType(isValueType) =>
              import isValueType.Underlying as Inner
              implicit val EitherInnerT: Type[Either[ConfigReaderFailures, Inner]] = RTypes.ReaderResult[Inner]
              val wrapLambda: Expr[Inner => Either[ConfigReaderFailures, A]] =
                buildWrap[A, Inner](isValueType.value, cursorExpr)
              deriveReaderRecursively[Inner](using rctx.nest[Inner](rctx.cursor)).map { innerResult =>
                Rule.matched(Expr.quote {
                  Expr.splice(innerResult).flatMap(Expr.splice(wrapLambda))
                })
              }

            case _ =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
          }
      }

    private def buildWrap[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner],
        cursorExpr: Expr[ConfigCursor]
    ): Expr[Inner => Either[ConfigReaderFailures, A]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
      @scala.annotation.nowarn("msg=is never used")
      implicit val EitherT: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]
      @scala.annotation.nowarn("msg=is never used")
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor

      def makeFailure(msg: Expr[String]): Expr[ConfigReaderFailures] = Expr.quote {
        ConfigReaderFailures(
          ConvertFailure(
            reason = CannotConvert(
              value = Expr.splice(cursorExpr).valueOpt.map(_.render).getOrElse(""),
              toType = Expr.splice(Expr(Type[A].prettyPrint)),
              because = Expr.splice(msg)
            ),
            origin = Expr.splice(cursorExpr).origin,
            path = Expr.splice(cursorExpr).path
          )
        )
      }

      isValueType.wrap match {
        case _: CtorLikeOf.PlainValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Right(Expr.splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[A]]))
          }
        case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr
              .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[String, A]]])
              .left
              .map((msg: String) => Expr.splice(makeFailure(Expr.quote(msg))))
          }
        case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr
              .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Iterable[String], A]]])
              .left
              .map((errs: Iterable[String]) => Expr.splice(makeFailure(Expr.quote(errs.mkString("\n")))))
          }
        case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr
              .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Throwable, A]]])
              .left
              .map((err: Throwable) => Expr.splice(makeFailure(Expr.quote(err.getMessage))))
          }
        case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr
              .splice(
                isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
              )
              .left
              .map((errs: Iterable[Throwable]) =>
                Expr.splice(makeFailure(Expr.quote(errs.map(_.getMessage).mkString("\n"))))
              )
          }
      }
    }
  }
}
