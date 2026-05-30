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
        if (Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is a named tuple, not a value type"))
        else
          Type[A] match {
            case IsValueType(isValueType) =>
              import isValueType.Underlying as Inner

              DTypes
                .Decoder[Inner]
                .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
                .toEither match {
                case Right(innerDecoder) =>
                  @scala.annotation.nowarn("msg=is never used")
                  implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
                  buildWrapLambda[A, Inner](isValueType.value).map { builder =>
                    val wrapLambda = builder.build[Either[DecodingFailure, A]]
                    Rule.matched(Expr.quote {
                      Expr.splice(innerDecoder).apply(Expr.splice(dctx.cursor)).flatMap(Expr.splice(wrapLambda))
                    })
                  }
                case Left(reason) =>
                  MIO.pure(Rule.yielded(s"Value type inner ${Type[Inner].prettyPrint} has no Decoder: $reason"))
              }

            case _ =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
          }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def buildWrapLambda[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    ) = {
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
      LambdaBuilder
        .of1[Inner]("inner")
        .traverse { innerExpr =>
          isValueType.wrap match {
            case _: CtorLikeOf.PlainValue[?, ?] =>
              val wrapped = isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[A]]
              MIO.pure(Expr.quote(Right(Expr.splice(wrapped)): Either[DecodingFailure, A]))
            case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
              val wrapResult = isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (msg: String) =>
                  io.circe.DecodingFailure(msg, Nil)
                }
              })
            case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
              val wrapResult =
                isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Iterable[String], A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (errs: Iterable[String]) =>
                  io.circe.DecodingFailure(errs.mkString("\n"), Nil)
                }
              })
            case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
              val wrapResult = isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Throwable, A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (err: Throwable) =>
                  io.circe.DecodingFailure(err.getMessage, Nil)
                }
              })
            case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
              val wrapResult =
                isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (errs: Iterable[Throwable]) =>
                  io.circe.DecodingFailure(errs.map(_.getMessage).mkString("\n"), Nil)
                }
              })
          }
        }
    }
  }
}
