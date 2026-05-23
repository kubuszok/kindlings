package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.ConfigDecodingError
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.ConfigValue

trait ReaderHandleAsCollectionRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsCollectionRule extends ReaderDerivationRule("handle as collection when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
            implicit val EitherItem: Type[Either[ConfigDecodingError, Item]] = RTypes.ReaderResult[Item]
            implicit val EitherA: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]

            LambdaBuilder
              .of1[ConfigValue]("itemValue")
              .traverse { itemValueExpr =>
                deriveReaderRecursively[Item](using rctx.nest[Item](itemValueExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[ConfigDecodingError, Item]]
                val factoryExpr = isCollection.value.factory
                val buildStep = isCollection.value.build

                val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
                  val collBuilder = Expr.splice(factoryExpr).newBuilder
                  val reader = SConfigDerivationUtils.readerFromFn(Expr.splice(decodeFn))
                  val list = SConfigDerivationUtils.asList(Expr.splice(rctx.value)) match {
                    case Right(l) => l
                    case Left(e)  => throw new SConfigDerivationUtils.CollectionBuildException(e)
                  }
                  val iter = list.iterator()
                  while (iter.hasNext)
                    reader.from(iter.next()) match {
                      case Right(item) => collBuilder += item
                      case Left(e)     => throw new SConfigDerivationUtils.CollectionBuildException(e)
                    }
                  collBuilder
                }
                val buildResultExpr = buildStep.ctor(readLoop)

                buildStep match {
                  case _: CtorLikeOf.PlainValue[?, ?] =>
                    Rule.matched(Expr.quote {
                      try Right(Expr.splice(buildResultExpr.asInstanceOf[Expr[A]]))
                      catch {
                        case e: SConfigDerivationUtils.CollectionBuildException =>
                          Left(e.error)
                      }
                    })

                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[String, A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(err)    => Left(SConfigDerivationUtils.liftStringError(err))
                        }
                      catch {
                        case e: SConfigDerivationUtils.CollectionBuildException =>
                          Left(e.error)
                      }
                    })

                  case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(errs)   =>
                            Left(SConfigDerivationUtils.liftStringError(errs.mkString("\n")))
                        }
                      catch {
                        case e: SConfigDerivationUtils.CollectionBuildException =>
                          Left(e.error)
                      }
                    })

                  case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(err)    =>
                            Left(SConfigDerivationUtils.liftStringError(err.getMessage))
                        }
                      catch {
                        case e: SConfigDerivationUtils.CollectionBuildException =>
                          Left(e.error)
                      }
                    })

                  case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(errs)   =>
                            Left(SConfigDerivationUtils.liftStringError(errs.map(_.getMessage).mkString("\n")))
                        }
                      catch {
                        case e: SConfigDerivationUtils.CollectionBuildException =>
                          Left(e.error)
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
