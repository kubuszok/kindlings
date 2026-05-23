package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{DecodingFailure, HCursor}

trait DecoderHandleAsCollectionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val HCursorT: Type[HCursor] = DTypes.HCursor
            implicit val EitherDFItem: Type[Either[DecodingFailure, Item]] = DTypes.DecoderResult[Item]
            implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]

            LambdaBuilder
              .of1[HCursor]("itemCursor")
              .traverse { itemCursorExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemCursorExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[DecodingFailure, Item]]
                val factoryExpr = isCollection.value.factory
                val buildStep = isCollection.value.build

                val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
                  val cursor = Expr.splice(dctx.cursor)
                  val decoder = CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn))
                  val collBuilder = Expr.splice(factoryExpr).newBuilder
                  cursor.values match {
                    case None =>
                      throw new CirceDerivationUtils.CollectionBuildException(
                        DecodingFailure("Expected JSON array", cursor.history)
                      )
                    case Some(jsonValues) =>
                      val iter = jsonValues.iterator
                      while (iter.hasNext)
                        decoder.decodeJson(iter.next()) match {
                          case Right(item) => collBuilder += item
                          case Left(err)   => throw new CirceDerivationUtils.CollectionBuildException(err)
                        }
                  }
                  collBuilder
                }
                val buildResultExpr = buildStep.ctor(readLoop)

                buildStep match {
                  case _: CtorLikeOf.PlainValue[?, ?] =>
                    Rule.matched(Expr.quote {
                      try Right(Expr.splice(buildResultExpr.asInstanceOf[Expr[A]]))
                      catch {
                        case e: CirceDerivationUtils.CollectionBuildException =>
                          Left(e.failure)
                      }
                    })

                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[String, A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(err)    =>
                            Left(DecodingFailure(err, Expr.splice(dctx.cursor).history))
                        }
                      catch {
                        case e: CirceDerivationUtils.CollectionBuildException =>
                          Left(e.failure)
                      }
                    })

                  case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(errs)   =>
                            Left(DecodingFailure(errs.mkString("\n"), Expr.splice(dctx.cursor).history))
                        }
                      catch {
                        case e: CirceDerivationUtils.CollectionBuildException =>
                          Left(e.failure)
                      }
                    })

                  case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(err)    =>
                            Left(DecodingFailure(err.getMessage, Expr.splice(dctx.cursor).history))
                        }
                      catch {
                        case e: CirceDerivationUtils.CollectionBuildException =>
                          Left(e.failure)
                      }
                    })

                  case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(errs)   =>
                            Left(
                              DecodingFailure(
                                errs.map(_.getMessage).mkString("\n"),
                                Expr.splice(dctx.cursor).history
                              )
                            )
                        }
                      catch {
                        case e: CirceDerivationUtils.CollectionBuildException =>
                          Left(e.failure)
                      }
                    })
                }
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
