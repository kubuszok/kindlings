package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils
import pureconfig.ConfigCursor
import pureconfig.error.ConfigReaderFailures

trait ReaderHandleAsCollectionRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsCollectionRule extends ReaderDerivationRule("handle as collection when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
            implicit val EitherItem: Type[Either[ConfigReaderFailures, Item]] = RTypes.ReaderResult[Item]
            implicit val EitherA: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]

            LambdaBuilder
              .of1[ConfigCursor]("itemCursor")
              .traverse { itemCursorExpr =>
                deriveReaderRecursively[Item](using rctx.nest[Item](itemCursorExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[ConfigReaderFailures, Item]]
                val factoryExpr = isCollection.value.factory
                val buildStep = isCollection.value.build

                val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
                  val collBuilder = Expr.splice(factoryExpr).newBuilder
                  val reader = PureConfigDerivationUtils.readerFromFn(Expr.splice(decodeFn))
                  val cursor = Expr.splice(rctx.cursor)
                  cursor.asList match {
                    case Right(items) =>
                      items.foreach { itemCursor =>
                        reader.from(itemCursor) match {
                          case Right(item) => collBuilder += item
                          case Left(e)     =>
                            throw new PureConfigDerivationUtils.CollectionBuildException(e)
                        }
                      }
                    case Left(e) =>
                      throw new PureConfigDerivationUtils.CollectionBuildException(e)
                  }
                  collBuilder
                }
                val buildResultExpr = buildStep.ctor(readLoop)

                buildStep match {
                  case _: CtorLikeOf.PlainValue[?, ?] =>
                    Rule.matched(Expr.quote {
                      try Right(Expr.splice(buildResultExpr.asInstanceOf[Expr[A]]))
                      catch {
                        case e: PureConfigDerivationUtils.CollectionBuildException => Left(e.error)
                      }
                    })

                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[String, A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(err)    =>
                            Left(PureConfigDerivationUtils.liftStringError(Expr.splice(rctx.cursor), err))
                        }
                      catch {
                        case e: PureConfigDerivationUtils.CollectionBuildException => Left(e.error)
                      }
                    })

                  case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(errs)   =>
                            Left(
                              PureConfigDerivationUtils
                                .liftStringError(Expr.splice(rctx.cursor), errs.mkString("\n"))
                            )
                        }
                      catch {
                        case e: PureConfigDerivationUtils.CollectionBuildException => Left(e.error)
                      }
                    })

                  case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(err)    =>
                            Left(
                              PureConfigDerivationUtils
                                .liftStringError(Expr.splice(rctx.cursor), err.getMessage)
                            )
                        }
                      catch {
                        case e: PureConfigDerivationUtils.CollectionBuildException => Left(e.error)
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
                              PureConfigDerivationUtils
                                .liftStringError(Expr.splice(rctx.cursor), errs.map(_.getMessage).mkString("\n"))
                            )
                        }
                      catch {
                        case e: PureConfigDerivationUtils.CollectionBuildException => Left(e.error)
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
