package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonWriter
import hearth.kindlings.ubjsonderivation.annotations.{fieldName as fieldNameAnn, transientField}

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
      implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val AnyT: Type[Any] = CTypes.Any
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

      val allFields = caseClass.caseFieldValuesAt(ectx.value).toList

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
                    deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldEnc =>
                      val nameOverride =
                        paramsByName.get(fName).flatMap(p => getAnnotationStringArg[fieldNameAnn](p))

                      val unconditionalWrite: Expr[Unit] = nameOverride match {
                        case Some(customName) =>
                          Expr.quote {
                            Expr.splice(ectx.writer).writeFieldName(Expr.splice(Expr(customName)))
                            Expr.splice(fieldEnc)
                          }
                        case None =>
                          Expr.quote {
                            Expr
                              .splice(ectx.writer)
                              .writeFieldName(Expr.splice(ectx.config).fieldNameMapper(Expr.splice(Expr(fName))))
                            Expr.splice(fieldEnc)
                          }
                      }

                      // Determine transient skip conditions
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
                        val isMapF = Type[Field] match { case IsMap(_) => true; case _ => false }
                        val isCollF = Type[Field] match { case IsCollection(_) => true; case _ => false }
                        isMapF || isCollF
                      }
                      val isEmptyCapable = isStringField || isCollectionField

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
