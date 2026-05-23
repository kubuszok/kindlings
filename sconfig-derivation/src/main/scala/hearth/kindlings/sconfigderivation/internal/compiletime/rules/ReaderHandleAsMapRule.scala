package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.ConfigDecodingError
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.ConfigValue

trait ReaderHandleAsMapRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object ReaderHandleAsMapRule extends ReaderDerivationRule("handle as map when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            decodeMapEntries[A, Pair](isMap.value)
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeMapEntries[A: ReaderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = RTypes.String
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val EitherValueT: Type[Either[ConfigDecodingError, Value]] = RTypes.ReaderResult[Value]
      implicit val EitherA: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]

      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else
        LambdaBuilder
          .of1[ConfigValue]("valueValue")
          .traverse { valueValueExpr =>
            deriveReaderRecursively[Value](using rctx.nest[Value](valueValueExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[ConfigDecodingError, Value]]
            val factoryExpr = isMap.factory
            val buildStep = isMap.build

            val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
              val mapBuilder = Expr.splice(factoryExpr).newBuilder
              val reader = SConfigDerivationUtils.readerFromFn(Expr.splice(decodeFn))
              val obj = SConfigDerivationUtils.asObject(Expr.splice(rctx.value)) match {
                case Right(o) => o
                case Left(e)  => throw new SConfigDerivationUtils.CollectionBuildException(e)
              }
              val iter = {
                import scala.jdk.CollectionConverters.*
                obj.entrySet().iterator().asScala
              }
              while (iter.hasNext) {
                val entry = iter.next()
                reader.from(entry.getValue) match {
                  case Right(v) =>
                    mapBuilder += Expr.splice(
                      isMap.pair(Expr.quote(entry.getKey.asInstanceOf[Key]), Expr.quote(v))
                    )
                  case Left(e) => throw new SConfigDerivationUtils.CollectionBuildException(e)
                }
              }
              mapBuilder
            }
            val buildResultExpr = buildStep.ctor(readLoop)

            buildStep match {
              case _: CtorLikeOf.PlainValue[?, ?] =>
                Rule.matched(Expr.quote {
                  try Right(Expr.splice(buildResultExpr.asInstanceOf[Expr[A]]))
                  catch {
                    case e: SConfigDerivationUtils.CollectionBuildException => Left(e.error)
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
                    case e: SConfigDerivationUtils.CollectionBuildException => Left(e.error)
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
                    case e: SConfigDerivationUtils.CollectionBuildException => Left(e.error)
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
                    case e: SConfigDerivationUtils.CollectionBuildException => Left(e.error)
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
                    case e: SConfigDerivationUtils.CollectionBuildException => Left(e.error)
                  }
                })
            }
          }
    }
  }
}
