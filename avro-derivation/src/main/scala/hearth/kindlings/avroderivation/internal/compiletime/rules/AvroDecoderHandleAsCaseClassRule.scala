package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.annotations.{avroAlias, avroFixed, avroScalePrecision, fieldName, transientField}
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
      implicit val avroScalePrecisionT: Type[avroScalePrecision] = SfTypes.AvroScalePrecision
      implicit val avroAliasT: Type[avroAlias] = DecTypes.AvroAlias

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
          implicit val ListStringT: Type[List[String]] = DecTypes.ListString

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldName](param)
              val avroFieldNameExpr: Expr[String] = nameOverride match {
                case Some(custom) => Expr(custom)
                case None         =>
                  Expr.quote(Expr.splice(dctx.config).transformFieldNames(Expr.splice(Expr(fName))))
              }
              val aliases = getAllAnnotationStringArgs[avroAlias](param)
              val avroFixedSize = getAnnotationIntArg[avroFixed](param)
              val decimalOverride = getAnnotationTwoIntArgs[avroScalePrecision](param)

              val aliasesExpr: Expr[List[String]] =
                aliases.foldRight(Expr.quote(List.empty[String])) { (a, acc) =>
                  Expr.quote(Expr.splice(Expr(a)) :: Expr.splice(acc))
                }

              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                (avroFixedSize, decimalOverride) match {
                  case (Some(_), _) =>
                    val arrayByteType: Type[Array[Byte]] = DecTypes.ArrayByte
                    MIO.pure {
                      implicit val ArrayByteT: Type[Array[Byte]] = arrayByteType
                      val decodedExpr: Expr_?? = Expr.quote {
                        val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                        AvroDerivationUtils.decodeFixed(
                          AvroDerivationUtils.getFieldByNameOrAlias(
                            record,
                            Expr.splice(avroFieldNameExpr),
                            Expr.splice(aliasesExpr)
                          )
                        ): Array[Byte]
                      }.as_??
                      (fName, decodedExpr)
                    }
                  case (_, Some((_, scale))) =>
                    val bigDecimalType: Type[BigDecimal] = Type.of[BigDecimal]
                    MIO.pure {
                      implicit val BigDecimalT: Type[BigDecimal] = bigDecimalType
                      val decodedExpr: Expr_?? = Expr.quote {
                        val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                        AvroDerivationUtils.decodeBigDecimal(
                          AvroDerivationUtils.getFieldByNameOrAlias(
                            record,
                            Expr.splice(avroFieldNameExpr),
                            Expr.splice(aliasesExpr)
                          ),
                          Expr.splice(Expr(scale))
                        ): BigDecimal
                      }.as_??
                      (fName, decodedExpr)
                    }
                  case _ =>
                    val fieldAvroValue: Expr[Any] = Expr.quote {
                      AvroDerivationUtils.getFieldByNameOrAlias(
                        Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord],
                        Expr.splice(avroFieldNameExpr),
                        Expr.splice(aliasesExpr)
                      )
                    }
                    deriveDecoderRecursively[Field](using dctx.nest[Field](fieldAvroValue)).map { decoded =>
                      (fName, decoded.as_??)
                    }
                }
              }
            }
            .flatMap { fieldData =>
              val fieldMap: Map[String, Expr_??] =
                fieldData.toList.map { case (fName, decodedExpr) => (fName, decodedExpr) }.toMap ++ transientDefaults
              caseClass.primaryConstructor(fieldMap) match {
                case Right(constructExpr) =>
                  MIO.pure(Expr.quote {
                    val _ = AvroDerivationUtils.checkIsRecord(Expr.splice(dctx.avroValue))
                    Expr.splice(constructExpr)
                  })
                case Left(error) =>
                  val err = DecoderDerivationError.CannotConstructType(
                    Type[A].prettyPrint,
                    isSingleton = false,
                    Some(error)
                  )
                  Log.error(err.message) >> MIO.fail(err)
              }
            }
      }
    }

  }
}
