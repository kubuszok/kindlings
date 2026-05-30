package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.virtuslab.yaml.ConstructError

trait DecoderHandleAsValueTypeRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsValueTypeRule extends DecoderDerivationRule("handle as value type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        if (Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is a named tuple, not a value type"))
        else
          Type[A] match {
            case IsValueType(isValueType) =>
              import isValueType.Underlying as Inner

              DTypes
                .YamlDecoder[Inner]
                .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
                .toEither match {
                case Right(innerDecoder) =>
                  @scala.annotation.nowarn("msg=is never used")
                  implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
                  buildWrapLambda[A, Inner](isValueType.value).map { builder =>
                    val wrapLambda = builder.build[Either[ConstructError, A]]
                    Rule.matched(Expr.quote {
                      Expr
                        .splice(innerDecoder)
                        .construct(Expr.splice(dctx.node))(org.virtuslab.yaml.LoadSettings.empty)
                        .flatMap(Expr.splice(wrapLambda))
                    })
                  }
                case Left(reason) =>
                  MIO.pure(Rule.yielded(s"Value type inner ${Type[Inner].prettyPrint} has no YamlDecoder: $reason"))
              }

            case _ =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
          }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def buildWrapLambda[A: Type, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    )(implicit dctx: DecoderCtx[?]) = {
      implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
      LambdaBuilder
        .of1[Inner]("inner")
        .traverse { innerExpr =>
          isValueType.wrap match {
            case _: CtorLikeOf.PlainValue[?, ?] =>
              val wrapped = isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[A]]
              MIO.pure(Expr.quote(Right(Expr.splice(wrapped)): Either[ConstructError, A]))
            case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
              val wrapResult = isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (msg: String) =>
                  ConstructError.from(msg, Expr.splice(dctx.node))
                }
              })
            case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
              val wrapResult =
                isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Iterable[String], A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (errs: Iterable[String]) =>
                  ConstructError.from(errs.mkString("\n"), Expr.splice(dctx.node))
                }
              })
            case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
              val wrapResult = isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Throwable, A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (err: Throwable) =>
                  ConstructError.from(err.getMessage, Expr.splice(dctx.node))
                }
              })
            case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
              val wrapResult =
                isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (errs: Iterable[Throwable]) =>
                  ConstructError.from(errs.map(_.getMessage).mkString("\n"), Expr.splice(dctx.node))
                }
              })
          }
        }
    }
  }
}
