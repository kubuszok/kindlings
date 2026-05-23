package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils
import pureconfig.ConfigCursor
import pureconfig.error.ConfigReaderFailures

trait ReaderHandleAsMapRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object ReaderHandleAsMapRule extends ReaderDerivationRule("handle as map when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
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
    ): MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = RTypes.String
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val EitherValueT: Type[Either[ConfigReaderFailures, Value]] = RTypes.ReaderResult[Value]
      implicit val EitherA: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]

      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else
        LambdaBuilder
          .of1[ConfigCursor]("valueCursor")
          .traverse { valueCursorExpr =>
            deriveReaderRecursively[Value](using rctx.nest[Value](valueCursorExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[ConfigReaderFailures, Value]]
            val factoryExpr = isMap.factory
            val buildStep = isMap.build

            val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
              val mapBuilder = Expr.splice(factoryExpr).newBuilder
              val reader = PureConfigDerivationUtils.readerFromFn(Expr.splice(decodeFn))
              val cursor = Expr.splice(rctx.cursor)
              cursor.asMap match {
                case Right(map) =>
                  map.foreach { case (key, valueCursor) =>
                    reader.from(valueCursor) match {
                      case Right(v) =>
                        mapBuilder += Expr.splice(
                          isMap.pair(Expr.quote(key.asInstanceOf[Key]), Expr.quote(v))
                        )
                      case Left(e) =>
                        throw new PureConfigDerivationUtils.CollectionBuildException(e)
                    }
                  }
                case Left(e) =>
                  throw new PureConfigDerivationUtils.CollectionBuildException(e)
              }
              mapBuilder
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
                          PureConfigDerivationUtils.liftStringError(Expr.splice(rctx.cursor), errs.mkString("\n"))
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
                        Left(PureConfigDerivationUtils.liftStringError(Expr.splice(rctx.cursor), err.getMessage))
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
    }
  }
}
