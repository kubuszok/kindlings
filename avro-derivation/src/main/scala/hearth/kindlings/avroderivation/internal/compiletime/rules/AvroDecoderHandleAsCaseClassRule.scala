package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.annotations.{avroFixed, fieldName, transientField}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.generic.GenericRecord

trait AvroDecoderHandleAsCaseClassRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            decodeCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = DecTypes.String
      implicit val AnyT: Type[Any] = DecTypes.Any
      implicit val fieldNameT: Type[fieldName] = DecTypes.FieldName
      implicit val transientFieldT: Type[transientField] = DecTypes.TransientField
      implicit val avroFixedT: Type[avroFixed] = DecTypes.AvroFixed

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = DecoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        case None => // OK
      }

      // Separate transient and non-transient fields
      val transientFields = fieldsList.filter { case (_, param) => hasAnnotationType[transientField](param) }
      val nonTransientFields = fieldsList.filter { case (_, param) => !hasAnnotationType[transientField](param) }

      // Build transient defaults map
      val transientDefaults: Map[String, Expr_??] = transientFields.flatMap { case (fName, param) =>
        param.defaultValue.flatMap { existentialOuter =>
          val methodOf = existentialOuter.value
          methodOf.value match {
            case noInstance: Method.NoInstance[?] =>
              import noInstance.Returned
              noInstance(Map.empty).toOption.map { defaultExpr =>
                (fName, defaultExpr.as_??)
              }
            case _ => None
          }
        }
      }.toMap

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // All fields are transient or there are no fields — validate input and construct with defaults
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                transientDefaults.get(field.name) match {
                  case Some(defaultExpr) =>
                    MIO.pure(defaultExpr.value.asInstanceOf[Expr[field.tpe.Underlying]])
                  case None =>
                    val err = DecoderDerivationError.CannotConstructType(
                      Type[A].prettyPrint,
                      isSingleton = false,
                      Some(s"Unexpected parameter in zero-argument case class")
                    )
                    Log.error(err.message) >> MIO.fail(err)
                }
            })
            .flatMap {
              case Some(expr) =>
                MIO.pure(Expr.quote {
                  val _ = AvroDerivationUtils.checkIsRecord(Expr.splice(dctx.avroValue))
                  Expr.splice(expr)
                })
              case None =>
                val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val ArrayAnyT: Type[Array[Any]] = DecTypes.ArrayAny

          val indexedFields = fields.toList.zipWithIndex

          // Step 1: For each non-transient field, derive a decode expression and build accessor
          NonEmptyList
            .fromList(indexedFields)
            .get
            .parTraverse { case ((fName, param), reindex) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldName](param)
              val avroFixedSize = getAnnotationIntArg[avroFixed](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                avroFixedSize match {
                  case Some(_) =>
                    // Decode GenericFixed -> Array[Byte] directly, no AvroDecoder needed
                    val arrayByteType: Type[Array[Byte]] = DecTypes.ArrayByte
                    MIO.pure {
                      implicit val ArrayByteT: Type[Array[Byte]] = arrayByteType
                      val decodeExpr: Expr[Any] = nameOverride match {
                        case Some(customName) =>
                          Expr.quote {
                            val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                            val fieldValue = AvroDerivationUtils.decodeRecord(
                              record,
                              Expr.splice(Expr(customName))
                            )
                            AvroDerivationUtils.decodeFixed(fieldValue): Any
                          }
                        case None =>
                          Expr.quote {
                            val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                            val fieldValue = AvroDerivationUtils.decodeRecord(
                              record,
                              Expr.splice(dctx.config).transformFieldNames(Expr.splice(Expr(fName)))
                            )
                            AvroDerivationUtils.decodeFixed(fieldValue): Any
                          }
                      }
                      val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                        val typedExpr = Expr.quote {
                          Expr.splice(arrExpr)(Expr.splice(Expr(reindex))).asInstanceOf[Array[Byte]]
                        }
                        (fName, typedExpr.as_??)
                      }
                      (decodeExpr, makeAccessor)
                    }
                  case None =>
                    deriveFieldDecoder[Field].map { decoderExpr =>
                      val decodeExpr: Expr[Any] = nameOverride match {
                        case Some(customName) =>
                          Expr.quote {
                            val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                            val fieldValue = AvroDerivationUtils.decodeRecord(
                              record,
                              Expr.splice(Expr(customName))
                            )
                            Expr.splice(decoderExpr).decode(fieldValue): Any
                          }
                        case None =>
                          Expr.quote {
                            val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                            val fieldValue = AvroDerivationUtils.decodeRecord(
                              record,
                              Expr.splice(dctx.config).transformFieldNames(Expr.splice(Expr(fName)))
                            )
                            Expr.splice(decoderExpr).decode(fieldValue): Any
                          }
                      }
                      val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                        val typedExpr = Expr.quote {
                          AvroDerivationUtils.unsafeCast(
                            Expr.splice(arrExpr)(Expr.splice(Expr(reindex))),
                            Expr.splice(decoderExpr)
                          )
                        }
                        (fName, typedExpr.as_??)
                      }
                      (decodeExpr, makeAccessor)
                    }
                }
              }
            }
            .flatMap { fieldData =>
              val decodeExprs = fieldData.toList.map(_._1)
              val makeAccessors = fieldData.toList.map(_._2)

              // Step 2: Build List literal from the decode expressions
              val listExpr: Expr[List[Any]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Any])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Step 3: Build the constructor lambda
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  val fieldMap: Map[String, Expr_??] = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor(fieldMap) match {
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
                    Expr
                      .splice(constructLambda)
                      .apply(
                        AvroDerivationUtils.sequenceDecodeResults(Expr.splice(listExpr))
                      )
                  }
                }
            }
      }
    }

  }
}
