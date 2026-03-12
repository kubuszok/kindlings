package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import cats.data.Validated
import io.circe.{DecodingFailure, HCursor}

trait DecoderHandleAsNamedTupleRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsNamedTupleRule extends DecoderDerivationRule("handle as named tuple when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- dctx.setHelper[A] { (cursor, config, failFast) =>
                @scala.annotation.nowarn("msg=is never used")
                implicit val AnyT: Type[Any] = DTypes.Any
                @scala.annotation.nowarn("msg=is never used")
                implicit val BooleanT: Type[Boolean] = DTypes.Boolean
                decodeNamedTupleFields[A](namedTuple.primaryConstructor)(using
                  dctx.nestInCache(cursor, config, failFast)
                )
                  .map { eitherExpr =>
                    // Named tuples always use fail-fast decode; convert to ValidatedNel when failFast=false
                    Expr.quote {
                      (if (Expr.splice(failFast)) Expr.splice(eitherExpr)
                       else
                         Validated.fromEither(Expr.splice(eitherExpr)).leftMap(cats.data.NonEmptyList.one(_))): Any
                    }
                  }
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
                  MIO.pure(Rule.matched(Expr.quote {
                    Expr
                      .splice(helperCall(dctx.cursor, dctx.config, dctx.failFast))
                      .asInstanceOf[Either[DecodingFailure, A]]
                  }))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeNamedTupleFields[A: DecoderCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[Either[DecodingFailure, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherDFAnyT: Type[Either[DecodingFailure, Any]] = DTypes.EitherDFAny
      implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
      implicit val ListEitherT: Type[List[Either[DecodingFailure, Any]]] = DTypes.ListEitherDFAny

      val fieldsList = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // Empty named tuple: return Right(empty tuple)
          constructor(Map.empty) match {
            case Right(constructExpr) =>
              MIO.pure(Expr.quote {
                Right(Expr.splice(constructExpr)): Either[DecodingFailure, A]
              })
            case Left(error) =>
              val err =
                DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
              Log.error(err.message) >> MIO.fail(err)
          }

        case Some(fields) =>
          // Step 1: For each field, derive a decoder and build decode + accessor expressions
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving decoder for named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  val decodeExpr: Expr[Either[DecodingFailure, Any]] = Expr.quote {
                    Expr
                      .splice(dctx.cursor)
                      .downField(
                        Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName)))
                      )
                      .as(Expr.splice(decoderExpr))
                      .asInstanceOf[Either[DecodingFailure, Any]]
                  }

                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      CirceDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(param.index))),
                        Expr.splice(decoderExpr)
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

              // Step 2: Build List literal from the decode expressions
              val listExpr: Expr[List[Either[DecodingFailure, Any]]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Either[DecodingFailure, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Step 3: Build the constructor lambda using LambdaBuilder + primaryConstructor
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val fieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  constructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      val err = DecoderDerivationError.CannotConstructType(
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
                    CirceDerivationUtils.sequenceDecodeResults(Expr.splice(listExpr)).map { arr =>
                      Expr.splice(constructLambda).apply(arr)
                    }
                  }
                }
            }
      }
    }
  }
}
