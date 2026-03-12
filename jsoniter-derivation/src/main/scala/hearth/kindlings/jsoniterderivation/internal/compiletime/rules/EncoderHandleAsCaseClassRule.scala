package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.annotations.{fieldName as fieldNameAnn, stringified, transientField}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter

trait EncoderHandleAsCaseClassRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              _ <- ectx.setHelper[A] { (value, writer, config) =>
                encodeCaseClassFields[A](caseClass)(using ectx.nestInCache(value, writer, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.writer, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    /** Encode only the key-value field pairs (no writeObjectStart/writeObjectEnd). */
    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def encodeCaseClassFieldsOnly[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Unit]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val AnyT: Type[Any] = CTypes.Any
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val allFields = caseClass.caseFieldValuesAt(ectx.value).toList

      // Singletons (case objects, parameterless enum cases) have no primary constructor.
      // Only access primaryConstructor when there are actual fields to process.
      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty) Map.empty
        else caseClass.primaryConstructor.parameters.flatten.toMap

      // Validate: @transientField on fields without defaults is a compile error
      paramsByName.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case None =>
          val nonTransientFields = allFields.filter { case (name, _) =>
            paramsByName.get(name).forall(p => !hasAnnotationType[transientField](p))
          }

          NonEmptyList.fromList(nonTransientFields) match {
            case Some(nonEmptyFields) =>
              nonEmptyFields
                .parTraverse { case (fName, fieldValue) =>
                  import fieldValue.{Underlying as Field, value as fieldExpr}
                  Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fName: ${Type[Field].prettyPrint}") {
                    val hasStringifiedAnnotation =
                      paramsByName.get(fName).exists(p => hasAnnotationType[stringified](p))
                    val fieldEncMIO: MIO[Expr[Unit]] = if (hasStringifiedAnnotation) {
                      deriveStringifiedEncoder[Field](fieldExpr, ectx.writer) match {
                        case Some(enc) => MIO.pure(enc)
                        case None      =>
                          val err = CodecDerivationError
                            .StringifiedOnNonNumeric(fName, Type[A].prettyPrint, Type[Field].prettyPrint)
                          Log.error(err.message) >> MIO.fail(err)
                      }
                    } else {
                      // When no @stringified annotation, check if global isStringified applies
                      deriveStringifiedEncoder[Field](fieldExpr, ectx.writer) match {
                        case Some(stringifiedEnc) =>
                          // Numeric field: generate both paths, select at runtime
                          deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { normalEnc =>
                            Expr.quote {
                              if (Expr.splice(ectx.config).isStringified)
                                Expr.splice(stringifiedEnc)
                              else
                                Expr.splice(normalEnc)
                            }
                          }
                        case None =>
                          // Non-numeric field: always normal encoding
                          deriveEncoderRecursively[Field](using ectx.nest(fieldExpr))
                      }
                    }
                    fieldEncMIO.map { fieldEnc =>
                      val nameOverride =
                        paramsByName.get(fName).flatMap(p => getAnnotationStringArg[fieldNameAnn](p))

                      // Build the writeKey + encode expression
                      val unconditionalWrite: Expr[Unit] = nameOverride match {
                        case Some(customName) =>
                          Expr.quote {
                            Expr.splice(ectx.writer).writeKey(Expr.splice(Expr(customName)))
                            Expr.splice(fieldEnc)
                          }
                        case None =>
                          Expr.quote {
                            Expr
                              .splice(ectx.writer)
                              .writeKey(Expr.splice(ectx.config).fieldNameMapper(Expr.splice(Expr(fName))))
                            Expr.splice(fieldEnc)
                          }
                      }

                      // Determine transient skip conditions based on field type
                      val param = paramsByName.get(fName)
                      val defaultAsAnyOpt: Option[Expr[Any]] = param.filter(_.hasDefault).flatMap { p =>
                        p.defaultValue.flatMap { existentialOuter =>
                          val methodOf = existentialOuter.value
                          methodOf.value match {
                            case noInstance: Method.NoInstance[?] =>
                              import noInstance.Returned
                              noInstance(Map.empty).toOption.map(_.upcast[Any])
                            case _ => None
                          }
                        }
                      }

                      val isOptionField = Type[Field] match {
                        case IsOption(_) => true
                        case _           => false
                      }
                      val isStringField = Type[Field] =:= CTypes.String
                      val isCollectionField = !isOptionField && {
                        val isMap = Type[Field] match { case IsMap(_) => true; case _ => false }
                        val isColl = Type[Field] match { case IsCollection(_) => true; case _ => false }
                        isMap || isColl
                      }
                      val isEmptyCapable = isStringField || isCollectionField

                      // Build skip conditions (only applicable ones per field type)
                      val conditions = List.newBuilder[Expr[Boolean]]

                      defaultAsAnyOpt.foreach { defaultExpr =>
                        conditions += Expr.quote {
                          Expr.splice(ectx.config).transientDefault &&
                          Expr.splice(fieldExpr).asInstanceOf[Any] == Expr.splice(defaultExpr)
                        }
                      }

                      if (isOptionField) {
                        conditions += Expr.quote {
                          Expr.splice(ectx.config).transientNone &&
                          !Expr.splice(fieldExpr).asInstanceOf[Option[Any]].isDefined
                        }
                      }

                      if (isEmptyCapable) {
                        if (isStringField) {
                          conditions += Expr.quote {
                            Expr.splice(ectx.config).transientEmpty &&
                            Expr.splice(fieldExpr).asInstanceOf[String].isEmpty
                          }
                        } else {
                          // Collection or Map — both extend Iterable
                          conditions += Expr.quote {
                            Expr.splice(ectx.config).transientEmpty &&
                            Expr.splice(fieldExpr).asInstanceOf[Iterable[Any]].isEmpty
                          }
                        }
                      }

                      val condList = conditions.result()
                      if (condList.isEmpty) {
                        unconditionalWrite
                      } else {
                        val skipExpr = condList.reduce { (a, b) =>
                          Expr.quote(Expr.splice(a) || Expr.splice(b))
                        }
                        Expr.quote {
                          if (!Expr.splice(skipExpr)) {
                            Expr.splice(unconditionalWrite)
                          }
                        }
                      }
                    }
                  }
                }
                .map { fieldExprs =>
                  fieldExprs.toList
                    .foldLeft(Expr.quote(()): Expr[Unit]) { (acc, field) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(field)
                      }
                    }
                }

            case None =>
              MIO.pure(Expr.quote(()): Expr[Unit])
          }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveStringifiedEncoder[F: Type](value: Expr[F], writer: Expr[JsonWriter])(implicit
        JsonWriterT: Type[JsonWriter],
        UnitT: Type[Unit]
    ): Option[Expr[Unit]] =
      if (Type[F] =:= CTypes.Int)
        Some(Expr.quote {
          Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Int])
        })
      else if (Type[F] =:= CTypes.Long)
        Some(Expr.quote {
          Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Long])
        })
      else if (Type[F] =:= CTypes.Double)
        Some(Expr.quote {
          Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Double])
        })
      else if (Type[F] =:= CTypes.Float)
        Some(Expr.quote {
          Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Float])
        })
      else if (Type[F] =:= CTypes.Short)
        Some(Expr.quote {
          Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Short])
        })
      else if (Type[F] =:= CTypes.Byte)
        Some(Expr.quote {
          Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Byte])
        })
      else if (Type[F] =:= CTypes.BigDecimal)
        Some(Expr.quote {
          Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[BigDecimal])
        })
      else if (Type[F] =:= CTypes.BigInt)
        Some(Expr.quote {
          Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[BigInt])
        })
      else None

    /** Encode a full JSON object: writeObjectStart + fields + writeObjectEnd. */
    @scala.annotation.nowarn("msg=is never used")
    private def encodeCaseClassFields[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Unit]] =
      encodeCaseClassFieldsOnly(caseClass).map { fieldsExpr =>
        Expr.quote {
          Expr.splice(ectx.writer).writeObjectStart()
          Expr.splice(fieldsExpr)
          Expr.splice(ectx.writer).writeObjectEnd()
        }
      }
  }

}
