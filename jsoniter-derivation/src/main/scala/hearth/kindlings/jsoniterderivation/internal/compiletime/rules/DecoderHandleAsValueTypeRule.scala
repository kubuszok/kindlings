package hearth.kindlings.jsoniterderivation.internal.compiletime
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
        if (Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is a named tuple, not a value type"))
        else
          Type[A] match {
            case IsValueType(isValueType) =>
              import isValueType.Underlying as Inner

              LambdaBuilder
                .of1[Inner]("inner")
                .traverse { innerExpr =>
                  isValueType.value.wrap match {
                    case _: CtorLikeOf.PlainValue[?, ?] =>
                      MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[A]])
                    case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                      val wrapResult = isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, A]]]
                      MIO.pure(Expr.quote {
                        Expr.splice(wrapResult) match {
                          case scala.Right(v)  => v
                          case scala.Left(msg) => Expr.splice(dctx.reader).decodeError(msg)
                        }
                      })
                    case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                      val wrapResult =
                        isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Iterable[String], A]]]
                      MIO.pure(Expr.quote {
                        Expr.splice(wrapResult) match {
                          case scala.Right(v)   => v
                          case scala.Left(errs) => Expr.splice(dctx.reader).decodeError(errs.mkString("\n"))
                        }
                      })
                    case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                      val wrapResult =
                        isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Throwable, A]]]
                      MIO.pure(Expr.quote {
                        Expr.splice(wrapResult) match {
                          case scala.Right(v)  => v
                          case scala.Left(err) => Expr.splice(dctx.reader).decodeError(err.getMessage)
                        }
                      })
                    case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                      val wrapResult =
                        isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                      MIO.pure(Expr.quote {
                        Expr.splice(wrapResult) match {
                          case scala.Right(v)   => v
                          case scala.Left(errs) =>
                            Expr.splice(dctx.reader).decodeError(errs.map(_.getMessage).mkString("\n"))
                        }
                      })
                  }
                }
                .flatMap { builder =>
                  val wrapLambda = builder.build[A]
                  summonJsonValueCodecCached[Inner] match {
                    case Right(innerCodec) =>
                      MIO.pure(Rule.matched(Expr.quote {
                        Expr
                          .splice(wrapLambda)
                          .apply(
                            Expr
                              .splice(innerCodec)
                              .decodeValue(Expr.splice(dctx.reader), Expr.splice(innerCodec).nullValue)
                          )
                      }))
                    case Left(_) =>
                      deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.reader)).map { innerDecoded =>
                        Rule.matched(Expr.quote {
                          Expr.splice(wrapLambda).apply(Expr.splice(innerDecoded))
                        })
                      }
                  }
                }

            case _ =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
          }
      }
  }

}
