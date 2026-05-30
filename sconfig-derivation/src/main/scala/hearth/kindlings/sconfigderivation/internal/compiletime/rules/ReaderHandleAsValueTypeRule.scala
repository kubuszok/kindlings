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
        if (Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is a named tuple, not a value type"))
        else
          Type[A] match {
            case IsValueType(isValueType) =>
              import isValueType.Underlying as Inner
              implicit val EitherInnerT: Type[Either[ConfigDecodingError, Inner]] = RTypes.ReaderResult[Inner]
              val wrapLambda: Expr[Inner => Either[ConfigDecodingError, A]] =
                buildWrap[A, Inner](isValueType.value)
              deriveReaderRecursively[Inner](using rctx.nest[Inner](rctx.value)).map { innerResult =>
                Rule.matched(Expr.quote {
                  Expr.splice(innerResult).flatMap(Expr.splice(wrapLambda))
                })
              }

            case _ =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
          }
      }

    private def buildWrap[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    ): Expr[Inner => Either[ConfigDecodingError, A]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val ErrT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      @scala.annotation.nowarn("msg=is never used")
      implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
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
              .map((msg: String) => SConfigDerivationUtils.liftStringError(msg))
          }
        case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr
              .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Iterable[String], A]]])
              .left
              .map((errs: Iterable[String]) => SConfigDerivationUtils.liftStringError(errs.mkString("\n")))
          }
        case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr
              .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Throwable, A]]])
              .left
              .map((err: Throwable) => SConfigDerivationUtils.liftStringError(err.getMessage))
          }
        case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
          Expr.quote { (inner: Inner) =>
            Expr
              .splice(
                isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
              )
              .left
              .map((errs: Iterable[Throwable]) =>
                SConfigDerivationUtils.liftStringError(errs.map(_.getMessage).mkString("\n"))
              )
          }
      }
    }
  }
}
