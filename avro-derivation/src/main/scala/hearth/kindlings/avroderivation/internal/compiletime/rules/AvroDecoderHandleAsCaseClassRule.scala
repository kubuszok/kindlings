package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.instances.ApplicativeTraverseForList
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.annotations.{avroAlias, avroFixed, avroScalePrecision, fieldName, transientField}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.generic.GenericRecord

trait AvroDecoderHandleAsCaseClassRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  // in a method of its own: a path-dependent type (`param.tpe.Underlying`) cannot be referred to inside a quote
  private def noneAs[Field: Type]: Expr[Field] = Expr.quote(None.asInstanceOf[Field])

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
      val fieldsList = constructor.totalParameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && transientFieldHasNoFallback(param) => name
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
        param.defaultValue
          .flatMap { method =>
            foldInstanceFree(method, "Default value")(
              onTypes = _ => Map.empty,
              onValues = _ => Map.empty
            ).toOption
              .map { defaultExpr =>
                (fName, defaultExpr)
              }
          }
          .orElse {
            // no default: an Option field is None (as avro4s decodes transient fields)
            import param.tpe.Underlying as Field
            if (transientFieldIsOptional(param)) Some((fName, noneAs[Field].as_??)) else None
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
          implicit val GenericRecordT: Type[GenericRecord] = DecTypes.GenericRecord

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldName](param)
              val avroFieldNameExpr: Expr[String] = nameOverride match {
                case Some(custom) => Expr(custom)
                case None         =>
                  dctx.evaluatedConfig match {
                    // Config statically known: map the field name at compile time to a constant string,
                    // dropping the per-field runtime `config.transformFieldNames(name)` call.
                    case Some(cfg) => Expr(cfg.transformFieldNames(fName))
                    case None      =>
                      Expr.quote(Expr.splice(dctx.config).transformFieldNames(Expr.splice(Expr(fName))))
                  }
              }
              val aliases = getAllAnnotationStringArgs[avroAlias](param)
              val avroFixedSize = getAnnotationIntArg[avroFixed](param)
              val decimalOverride = getScalePrecision(param).map(sp => (sp.precision, sp.scale))

              // No alias (the usual case) and one alias have their own overloads: no list is built per decoded field.
              def fieldByNameOrAlias(record: Expr[GenericRecord]): Expr[Any] = aliases match {
                case Nil =>
                  Expr.quote(
                    AvroDerivationUtils.getFieldByNameOrAlias(Expr.splice(record), Expr.splice(avroFieldNameExpr))
                  )
                case alias :: Nil =>
                  Expr.quote(
                    AvroDerivationUtils.getFieldByNameOrAlias(
                      Expr.splice(record),
                      Expr.splice(avroFieldNameExpr),
                      Expr.splice(Expr(alias))
                    )
                  )
                case _ =>
                  val aliasesExpr: Expr[List[String]] =
                    aliases.foldRight(Expr.quote(List.empty[String])) { (a, acc) =>
                      Expr.quote(Expr.splice(Expr(a)) :: Expr.splice(acc))
                    }
                  Expr.quote(
                    AvroDerivationUtils.getFieldByNameOrAlias(
                      Expr.splice(record),
                      Expr.splice(avroFieldNameExpr),
                      Expr.splice(aliasesExpr)
                    )
                  )
              }
              // The same lookup, returning `AvroDerivationUtils.MissingField` when the record has no such field.
              def fieldOrMissing(record: Expr[GenericRecord]): Expr[Any] = aliases match {
                case Nil =>
                  Expr.quote(AvroDerivationUtils.fieldOrMissing(Expr.splice(record), Expr.splice(avroFieldNameExpr)))
                case alias :: Nil =>
                  Expr.quote(
                    AvroDerivationUtils.fieldOrMissing(
                      Expr.splice(record),
                      Expr.splice(avroFieldNameExpr),
                      Expr.splice(Expr(alias))
                    )
                  )
                case _ =>
                  val aliasesExpr: Expr[List[String]] =
                    aliases.foldRight(Expr.quote(List.empty[String])) { (a, acc) =>
                      Expr.quote(Expr.splice(Expr(a)) :: Expr.splice(acc))
                    }
                  Expr.quote(
                    AvroDerivationUtils.fieldOrMissing(
                      Expr.splice(record),
                      Expr.splice(avroFieldNameExpr),
                      Expr.splice(aliasesExpr)
                    )
                  )
              }
              val recordExpr: Expr[GenericRecord] = Expr.quote(Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord])

              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                (avroFixedSize, decimalOverride) match {
                  case (Some(_), _) if Type[Field] =:= Type.of[String] =>
                    MIO.pure {
                      val decodedExpr: Expr_?? = Expr.quote {
                        AvroDerivationUtils.decodeFixedString(Expr.splice(fieldByNameOrAlias(recordExpr))): String
                      }.as_??
                      (fName, decodedExpr, false)
                    }
                  case (Some(_), _) =>
                    val arrayByteType: Type[Array[Byte]] = DecTypes.ArrayByte
                    MIO.pure {
                      implicit val ArrayByteT: Type[Array[Byte]] = arrayByteType
                      val decodedExpr: Expr_?? = Expr.quote {
                        AvroDerivationUtils.decodeFixed(Expr.splice(fieldByNameOrAlias(recordExpr))): Array[Byte]
                      }.as_??
                      (fName, decodedExpr, false)
                    }
                  case (_, Some((_, scale))) =>
                    val bigDecimalType: Type.Lazy[BigDecimal] = Type.Lazy(Type.of[BigDecimal])
                    MIO.pure {
                      implicit val BigDecimalT: Type[BigDecimal] = bigDecimalType
                      val decodedExpr: Expr_?? = Expr.quote {
                        AvroDerivationUtils.decodeBigDecimal(
                          Expr.splice(fieldByNameOrAlias(recordExpr)),
                          Expr.splice(Expr(scale))
                        ): BigDecimal
                      }.as_??
                      (fName, decodedExpr, false)
                    }
                  case _ =>
                    // As avro4s: a field with a Scala default decodes to the default when the record has no such field
                    // (data written with an older schema) or when its value cannot be decoded.
                    val defaultExpr: Option[Expr[Field]] = param.defaultValue.flatMap { method =>
                      foldInstanceFree(method, "Default value")(
                        onTypes = _ => Map.empty,
                        onValues = _ => Map.empty
                      ).toOption.map(_.value.asInstanceOf[Expr[Field]])
                    }
                    defaultExpr match {
                      case Some(default) =>
                        // One lookup (as avro4s' `safeDecodeFieldValue`): the value, or the `MissingField` sentinel.
                        MIO.scoped { runSafe =>
                          val withDefault: Expr[Field] =
                            ValDefs
                              .createVal[Any](fieldOrMissing(recordExpr), FreshName.FromPrefix(s"avroField_$fName"))
                              .use { fieldValue =>
                                val decoded =
                                  runSafe(deriveDecoderRecursively[Field](using dctx.nest[Field](fieldValue)))
                                Expr.quote {
                                  if (AvroDerivationUtils.isMissingField(Expr.splice(fieldValue))) Expr.splice(default)
                                  else
                                    try Expr.splice(decoded)
                                    catch {
                                      case avroDecodeError: Throwable =>
                                        if (AvroDerivationUtils.isFatal(avroDecodeError)) throw avroDecodeError
                                        else Expr.splice(default)
                                    }
                                }
                              }
                          (fName, withDefault.as_??, true)
                        }
                      case None =>
                        deriveDecoderRecursively[Field](using dctx.nest[Field](fieldByNameOrAlias(recordExpr)))
                          .map(decoded => (fName, decoded.as_??, false))
                    }
                }
              }
            }
            .flatMap { fieldData =>
              // A field with a default decodes through a try/catch. Inlined as a constructor argument, every such
              // try/catch makes the compiler save all the previously evaluated arguments around it, so the decoding
              // method grows quadratically with the number of defaulted fields: past ~25 of them it is too large to be
              // JIT-compiled, past ~88 it exceeds the JVM's 64 KB limit ("Method too large", issue #308). Each
              // defaulted field is therefore decoded by its own local def, and the constructor only calls the defs.
              val boundFields: ValDefs[List[(String, Expr_??)]] =
                fieldData.toList.traverse[ValDefs, (String, Expr_??)] { case (fName, decodedExpr, ownDef) =>
                  if (ownDef) {
                    import decodedExpr.{Underlying as Field, value as fieldExpr}
                    ValDefs
                      .createDef[Field](fieldExpr, FreshName.FromPrefix(s"decodeField_$fName"))
                      .map(call => (fName, call.as_??))
                  } else ValDefs.traverse.pure((fName, decodedExpr))
                }
              def construct(fields: List[(String, Expr_??)]) =
                foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                  onTypes = _ => Map.empty,
                  onValues = _ => fields.toMap ++ transientDefaults
                )
              // The parameters are known up front, so a failure does not depend on how the fields are bound.
              construct(fieldData.toList.map { case (fName, decodedExpr, _) => (fName, decodedExpr) }) match {
                case Right(_) =>
                  val constructed: Expr[A] = boundFields.use { fields =>
                    construct(fields) match {
                      case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[A]]
                      case Left(error)          => throw new IllegalStateException(error) // checked above
                    }
                  }
                  MIO.pure(
                    Expr.quote {
                      val _ = AvroDerivationUtils.checkIsRecord(Expr.splice(dctx.avroValue))
                      Expr.splice(constructed)
                    }
                  )
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
