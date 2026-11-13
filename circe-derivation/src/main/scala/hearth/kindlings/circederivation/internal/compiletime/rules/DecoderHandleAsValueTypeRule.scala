package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
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
              anyValCaseClass[A] match {
                case None => decodeUnwrapped[A, Inner](isValueType.value, dctx).map(Rule.matched)
                // An `AnyVal` case class: unwrapped by default, decoded as a product (like upstream circe) with
                // `unwrapValueClasses = false`. Cached as a helper of both forms (a cached helper of only the product
                // form, set by the case-class rule, would be reused for the unwrapped form too).
                case Some(caseClass) =>
                  dctx.evaluatedConfig.map(_.unwrapValueClasses) match {
                    case Some(true) => decodeUnwrapped[A, Inner](isValueType.value, dctx).map(Rule.matched)
                    case _          =>
                      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
                      implicit val AnyT: Type[Any] = DTypes.Any
                      val staticallyProduct = dctx.evaluatedConfig.exists(!_.unwrapValueClasses)
                      for {
                        _ <- dctx.setHelper[A] { (cursor, config, failFast) =>
                          val ctx = dctx.nestInCache(cursor, config, failFast)
                          val productMIO = DecoderHandleAsCaseClassRule.decodeCaseClassFields[A](caseClass)(using ctx)
                          if (staticallyProduct) productMIO
                          else
                            for {
                              unwrapped <- decodeUnwrapped[A, Inner](isValueType.value, ctx)
                              product <- productMIO
                            } yield Expr.quote {
                              if (Expr.splice(config).unwrapValueClasses) (Expr.splice(unwrapped): Any)
                              else Expr.splice(product)
                            }
                        }
                        helper <- dctx.getHelper[A]
                      } yield Rule.matched(Expr.quote {
                        CirceDerivationUtils.unsafeResult[Either[DecodingFailure, A]](
                          Expr.splice(helper.get(dctx.cursor, dctx.config, dctx.failFast))
                        )
                      })
                  }
              }

            case _ =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
          }
      }

    /** The unwrapped form: the inner value's decoder (user-provided or derived), then the value type's wrapper. */
    private def decodeUnwrapped[A, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner],
        ctx: DecoderCtx[A]
    ): MIO[Expr[Either[DecodingFailure, A]]] = {
      implicit val decoderCtx: DecoderCtx[A] = ctx
      implicit val AT: Type[A] = ctx.tpe
      @scala.annotation.nowarn("msg=is never used")
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
      val innerResultMIO: MIO[Expr[Either[DecodingFailure, Inner]]] = DTypes
        .Decoder[Inner]
        .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(innerDecoder) =>
          MIO.pure(runDecoderInstance[Inner](innerDecoder)(using dctx.nest[Inner](dctx.cursor)))
        case Left(_) =>
          // No implicit for the inner type: derive it recursively, symmetric with the encoder's value-type
          // rule (which encodes the unwrapped inner value with a derived encoder).
          deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.cursor))
      }
      for {
        innerResult <- innerResultMIO
        builder <- buildWrapLambda[A, Inner](isValueType)
      } yield {
        val wrapLambda = builder.build[Either[DecodingFailure, A]]
        Expr.quote {
          CirceDerivationUtils.unsafeResult[Either[DecodingFailure, A]](
            CirceDerivationUtils
              .flatMapResult(Expr.splice(dctx.failFast), Expr.splice(innerResult), Expr.splice(wrapLambda))
          )
        }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def buildWrapLambda[A: DecoderCtx, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    ) = {
      // Smart-constructor errors keep the cursor history (circe's `emap` uses `c.history`).
      val cursor = dctx.cursor
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
                  io.circe.DecodingFailure(msg, Expr.splice(cursor).history)
                }
              })
            case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
              val wrapResult =
                isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Iterable[String], A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (errs: Iterable[String]) =>
                  io.circe.DecodingFailure(errs.mkString("\n"), Expr.splice(cursor).history)
                }
              })
            case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
              val wrapResult = isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Throwable, A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (err: Throwable) =>
                  io.circe.DecodingFailure(err.getMessage, Expr.splice(cursor).history)
                }
              })
            case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
              val wrapResult =
                isValueType.wrap.apply(innerExpr).asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
              MIO.pure(Expr.quote {
                Expr.splice(wrapResult).left.map { (errs: Iterable[Throwable]) =>
                  io.circe.DecodingFailure(errs.map(_.getMessage).mkString("\n"), Expr.splice(cursor).history)
                }
              })
          }
        }
    }
  }
}
