package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils
import pureconfig.{ConfigCursor, ConfigReader}
import pureconfig.error.ConfigReaderFailures

trait ReaderHandleAsNamedTupleRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsNamedTupleRule extends ReaderDerivationRule("handle as named tuple when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- rctx.setHelper[A] { (cur, config) =>
                decodeNamedTupleFields[A](namedTuple.primaryConstructor)(using rctx.nestInCache(cur, config))
              }
              result <- rctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(rctx.cursor, rctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeNamedTupleFields[A: ReaderCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[Either[ConfigReaderFailures, A]]] = {
      implicit val StringT: Type[String] = RTypes.String
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
      implicit val AnyT: Type[Any] = RTypes.Any
      implicit val EitherFailuresAnyT: Type[Either[ConfigReaderFailures, Any]] = RTypes.EitherFailuresAny
      implicit val ArrayAnyT: Type[Array[Any]] = RTypes.ArrayAny
      implicit val ListEitherT: Type[List[Either[ConfigReaderFailures, Any]]] = RTypes.ListEitherFailuresAny

      val fieldsList = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          constructor(Map.empty) match {
            case Right(constructExpr) =>
              MIO.pure(Expr.quote {
                Expr.splice(rctx.cursor).asObjectCursor.map(_ => Expr.splice(constructExpr))
              })
            case Left(error) =>
              val err =
                ReaderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
              Log.error(err.message) >> MIO.fail(err)
          }

        case Some(fields) =>
          val indexedFields = fields.toList.zipWithIndex
          NonEmptyList
            .fromList(indexedFields)
            .get
            .parTraverse { case ((fName, param), reindex) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving reader for named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldReaderForNT[Field].map { readerExpr =>
                  val decodeExpr: Expr[Either[ConfigReaderFailures, Any]] = Expr.quote {
                    Expr
                      .splice(rctx.cursor)
                      .asObjectCursor
                      .flatMap { obj =>
                        PureConfigDerivationUtils.readRequiredField[Any](
                          obj,
                          Expr.splice(rctx.config).transformMemberNames(Expr.splice(Expr(fName))),
                          Expr.splice(readerExpr).asInstanceOf[ConfigReader[Any]]
                        )
                      }
                      .asInstanceOf[Either[ConfigReaderFailures, Any]]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      PureConfigDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(reindex))),
                        Expr.splice(readerExpr)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (decodeExpr, makeAccessor)
                }
              }
            }
            .flatMap { fieldData =>
              val decodeExprs = fieldData.toList.map(_._1)
              val makeAccessors = fieldData.toList.map(_._2)

              val listExpr: Expr[List[Either[ConfigReaderFailures, Any]]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Either[ConfigReaderFailures, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val fieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  constructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      val err = ReaderDerivationError.CannotConstructType(
                        Type[A].prettyPrint,
                        isSingleton = false,
                        Some(error)
                      )
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]
                  Expr.quote {
                    PureConfigDerivationUtils
                      .sequenceResults(Expr.splice(listExpr))
                      .map(Expr.splice(constructLambda))
                  }
                }
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveFieldReaderForNT[Field: Type](implicit ctx: ReaderCtx[?]): MIO[Expr[ConfigReader[Field]]] = {
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val ReaderResultField: Type[Either[ConfigReaderFailures, Field]] = RTypes.ReaderResult[Field]
      implicit val ReaderField: Type[ConfigReader[Field]] = RTypes.ConfigReader[Field]

      RTypes
        .ConfigReader[Field]
        .summonExprIgnoring(ReaderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(readerExpr) => MIO.pure(readerExpr)
        case Left(_)           =>
          LambdaBuilder
            .of1[ConfigCursor]("fieldCursor")
            .traverse { fieldCursorExpr =>
              deriveReaderRecursively[Field](using ctx.nest[Field](fieldCursorExpr))
            }
            .map { builder =>
              val readFn = builder.build[Either[ConfigReaderFailures, Field]]
              Expr.quote(PureConfigDerivationUtils.readerFromFn(Expr.splice(readFn)))
            }
      }
    }
  }
}
