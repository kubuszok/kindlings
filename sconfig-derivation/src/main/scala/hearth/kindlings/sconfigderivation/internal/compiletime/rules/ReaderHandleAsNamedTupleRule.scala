package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.{ConfigDecodingError, ConfigReader}
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.ConfigValue

trait ReaderHandleAsNamedTupleRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsNamedTupleRule extends ReaderDerivationRule("handle as named tuple when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- rctx.setHelper[A] { (value, config) =>
                decodeNamedTupleFields[A](namedTuple.primaryConstructor)(using rctx.nestInCache(value, config))
              }
              result <- rctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(rctx.value, rctx.config)))
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
    ): MIO[Expr[Either[ConfigDecodingError, A]]] = {
      implicit val StringT: Type[String] = RTypes.String
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val AnyT: Type[Any] = RTypes.Any
      implicit val EitherErrorAnyT: Type[Either[ConfigDecodingError, Any]] = RTypes.EitherErrorAny
      implicit val ArrayAnyT: Type[Array[Any]] = RTypes.ArrayAny
      implicit val ListEitherT: Type[List[Either[ConfigDecodingError, Any]]] = RTypes.ListEitherErrorAny

      val fieldsList = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          constructor(Map.empty) match {
            case Right(constructExpr) =>
              MIO.pure(Expr.quote {
                SConfigDerivationUtils.asObject(Expr.splice(rctx.value)).map(_ => Expr.splice(constructExpr))
              })
            case Left(error) =>
              val err = ReaderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
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
                  val decodeExpr: Expr[Either[ConfigDecodingError, Any]] = Expr.quote {
                    SConfigDerivationUtils
                      .asObject(Expr.splice(rctx.value))
                      .flatMap { obj =>
                        SConfigDerivationUtils
                          .readRequiredField(
                            obj,
                            Expr.splice(rctx.config).transformMemberNames(Expr.splice(Expr(fName)))
                          )
                          .flatMap(c => Expr.splice(readerExpr).from(c))
                      }
                      .asInstanceOf[Either[ConfigDecodingError, Any]]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      SConfigDerivationUtils.unsafeCast(
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

              val listExpr: Expr[List[Either[ConfigDecodingError, Any]]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Either[ConfigDecodingError, Any]])) { (elem, acc) =>
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
                    SConfigDerivationUtils
                      .sequenceResults(Expr.splice(listExpr))
                      .map(Expr.splice(constructLambda))
                  }
                }
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveFieldReaderForNT[Field: Type](implicit ctx: ReaderCtx[?]): MIO[Expr[ConfigReader[Field]]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ReaderResultField: Type[Either[ConfigDecodingError, Field]] = RTypes.ReaderResult[Field]
      implicit val ReaderField: Type[ConfigReader[Field]] = RTypes.ConfigReader[Field]

      RTypes
        .ConfigReader[Field]
        .summonExprIgnoring(ReaderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(readerExpr) => MIO.pure(readerExpr)
        case Left(_) =>
          LambdaBuilder
            .of1[ConfigValue]("fieldValue")
            .traverse { fieldValueExpr =>
              deriveReaderRecursively[Field](using ctx.nest[Field](fieldValueExpr))
            }
            .map { builder =>
              val readFn = builder.build[Either[ConfigDecodingError, Field]]
              Expr.quote(SConfigDerivationUtils.readerFromFn(Expr.splice(readFn)))
            }
      }
    }
  }
}
