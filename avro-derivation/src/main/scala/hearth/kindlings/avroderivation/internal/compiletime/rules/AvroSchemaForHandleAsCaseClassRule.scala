package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.AvroConfig
import hearth.kindlings.avroderivation.annotations.{
  avroAlias,
  avroDefault,
  avroDoc,
  avroError,
  avroFixed,
  avroNamespace,
  avroNoDefault,
  avroProp,
  fieldName,
  transientField
}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroSchemaForHandleAsCaseClassRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForHandleAsCaseClassRule extends SchemaDerivationRule("handle as case class when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            deriveCaseClassSchema[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveCaseClassSchema[A: SchemaForCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Schema]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      implicit val StringT: Type[String] = SfTypes.String
      implicit val fieldNameT: Type[fieldName] = SfTypes.FieldName
      implicit val transientFieldT: Type[transientField] = SfTypes.TransientField
      implicit val avroDocT: Type[avroDoc] = SfTypes.AvroDoc
      implicit val avroNamespaceT: Type[avroNamespace] = SfTypes.AvroNamespace
      implicit val avroDefaultT: Type[avroDefault] = SfTypes.AvroDefault
      implicit val avroFixedT: Type[avroFixed] = SfTypes.AvroFixed
      implicit val avroErrorT: Type[avroError] = SfTypes.AvroError
      implicit val avroPropT: Type[avroProp] = SfTypes.AvroProp
      implicit val avroAliasT: Type[avroAlias] = SfTypes.AvroAlias
      implicit val avroNoDefaultT: Type[avroNoDefault] = SfTypes.AvroNoDefault
      implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig

      // Read class-level annotations
      val classDoc: Option[String] = getTypeAnnotationStringArg[avroDoc, A]
      val classNamespace: Option[String] = getTypeAnnotationStringArg[avroNamespace, A]
      val isError: Boolean = hasTypeAnnotation[avroError, A]
      val classProps: List[(String, String)] = getAllTypeAnnotationTwoStringArgs[avroProp, A]
      val classAliases: List[String] = getAllTypeAnnotationStringArgs[avroAlias, A]

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList
      val typeName = Type[A].shortName

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = SchemaDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        case None => // OK
      }

      // Validate: @avroNoDefault and @avroDefault on the same field is a compile error
      fieldsList.collectFirst {
        case (name, param)
            if hasAnnotationType[avroNoDefault](param) && getAnnotationStringArg[avroDefault](param).isDefined =>
          name
      } match {
        case Some(name) =>
          val err = SchemaDerivationError.ConflictingDefaultAnnotations(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        case None => // OK
      }

      // Filter out transient fields
      val nonTransientFields = fieldsList.filter { case (_, param) => !hasAnnotationType[transientField](param) }

      // Build the namespace expression
      val namespaceExpr: Expr[String] = classNamespace match {
        case Some(ns) => Expr(ns)
        case None     => Expr.quote(Expr.splice(sfctx.config).namespace.getOrElse(""))
      }

      // Build the doc expression (null if no @avroDoc)
      val docExprOrNull: Expr[String] = classDoc match {
        case Some(doc) => Expr(doc)
        case None      => Expr.quote(null: String)
      }

      // Step 1: Register a self-record (empty record without fields) to break recursion cycles.
      // Recursive field derivation finds this via SfCheckSelfRecordRule.
      val emptyRecordExpr: Expr[Schema] = Expr.quote {
        AvroDerivationUtils.createEmptyRecord(
          Expr.splice(Expr(typeName)),
          Expr.splice(docExprOrNull),
          Expr.splice(namespaceExpr),
          Expr.splice(Expr(isError))
        )
      }

      for {
        _ <- sfctx.setSelfRecordSchema[A](emptyRecordExpr)
        selfRecordRef <- sfctx.getSelfRecordSchema[A].map(_.get)

        // Step 2: Derive field schemas (recursive calls find the self-record via SfCheckSelfRecordRule)
        schemaBody <- NonEmptyList.fromList(nonTransientFields) match {
          case None =>
            // No fields — setFields with empty list
            MIO.pure(
              applyClassAnnotations(
                Expr.quote {
                  AvroDerivationUtils.setRecordFields(
                    Expr.splice(selfRecordRef),
                    java.util.Collections.emptyList[Schema.Field]()
                  )
                },
                classProps,
                classAliases
              )
            )
          case Some(fields) =>
            fields
              .parTraverse { case (fName, param) =>
                import param.tpe.Underlying as Field
                val nameOverride = getAnnotationStringArg[fieldName](param)
                val fieldDoc = getAnnotationStringArg[avroDoc](param)
                val fieldDefault =
                  if (hasAnnotationType[avroNoDefault](param)) None
                  else getAnnotationStringArg[avroDefault](param)
                val avroFixedSize = getAnnotationIntArg[avroFixed](param)
                val fieldProps = getAllAnnotationTwoStringArgs[avroProp](param)
                val fieldAliases = getAllAnnotationStringArgs[avroAlias](param)
                Log.namedScope(s"Deriving schema for field $fName: ${Type[Field].prettyPrint}") {
                  avroFixedSize match {
                    case Some(_) if !(Type[Field] =:= Type.of[Array[Byte]]) =>
                      val err = SchemaDerivationError.AvroFixedOnNonByteArray(
                        fName,
                        Type[A].prettyPrint,
                        Type[Field].prettyPrint
                      )
                      Log.error(err.message) >> MIO.fail(err)
                    case Some(size) =>
                      val fixedName = nameOverride.getOrElse(fName)
                      MIO.pure {
                        val fieldSchema = Expr.quote {
                          AvroDerivationUtils.createFixed(
                            Expr.splice(Expr(fixedName)),
                            Expr.splice(sfctx.config).namespace.getOrElse(""),
                            Expr.splice(Expr(size))
                          )
                        }
                        (fName, fieldSchema, nameOverride, fieldDoc, fieldDefault, fieldProps, fieldAliases)
                      }
                    case None =>
                      deriveSchemaRecursively[Field](using sfctx.nest[Field]).map { fieldSchema =>
                        (fName, fieldSchema, nameOverride, fieldDoc, fieldDefault, fieldProps, fieldAliases)
                      }
                  }
                }
              }
              .map { fieldPairs =>
                val fieldsListExpr = buildFieldsExpr(fieldPairs.toList)
                // Step 3: setRecordFieldsFromList on the self-record and apply annotations
                applyClassAnnotations(
                  Expr.quote {
                    AvroDerivationUtils.setRecordFieldsFromList(
                      Expr.splice(selfRecordRef),
                      Expr.splice(fieldsListExpr)
                    )
                  },
                  classProps,
                  classAliases
                )
              }
        }

        // Step 4: Cache the complete schema as a lazy val (body = setRecordFields + annotations)
        _ <- sfctx.setCachedSchema[A](schemaBody)
        result <- sfctx.getCachedSchema[A].map(_.get)
      } yield result
    }

    private def buildFieldsExpr[A: SchemaForCtx](
        fieldPairs: List[
          (String, Expr[Schema], Option[String], Option[String], Option[String], List[(String, String)], List[String])
        ]
    )(implicit SchemaT: Type[Schema], StringT: Type[String], AvroConfigT: Type[AvroConfig]): Expr[List[Schema.Field]] =
      fieldPairs.foldRight(
        Expr.quote(List.empty[Schema.Field])
      ) {
        case (
              (fName, fieldSchema, nameOverride, fieldDoc, fieldDefault, fieldProps, fieldAliases),
              acc
            ) =>
          val nameExpr: Expr[String] = nameOverride match {
            case Some(customName) => Expr(customName)
            case None             =>
              Expr.quote {
                Expr.splice(sfctx.config).transformFieldNames(Expr.splice(Expr(fName)))
              }
          }
          val baseFieldExpr: Expr[Schema.Field] = (fieldDoc, fieldDefault) match {
            case (Some(doc), Some(default)) =>
              Expr.quote {
                AvroDerivationUtils.createFieldWithDocAndDefault(
                  Expr.splice(nameExpr),
                  Expr.splice(fieldSchema),
                  Expr.splice(Expr(doc)),
                  Expr.splice(Expr(default))
                )
              }
            case (Some(doc), None) =>
              Expr.quote {
                AvroDerivationUtils.createFieldWithDoc(
                  Expr.splice(nameExpr),
                  Expr.splice(fieldSchema),
                  Expr.splice(Expr(doc))
                )
              }
            case (None, Some(default)) =>
              Expr.quote {
                AvroDerivationUtils.createFieldWithDefault(
                  Expr.splice(nameExpr),
                  Expr.splice(fieldSchema),
                  Expr.splice(Expr(default))
                )
              }
            case (None, None) =>
              Expr.quote {
                AvroDerivationUtils.createField(
                  Expr.splice(nameExpr),
                  Expr.splice(fieldSchema)
                )
              }
          }
          // Apply field-level @avroProp annotations
          val fieldWithPropsExpr: Expr[Schema.Field] =
            if (fieldProps.isEmpty) baseFieldExpr
            else {
              val propsListExpr = fieldProps.foldRight(Expr.quote(List.empty[(String, String)])) {
                case ((k, v), listAcc) =>
                  Expr.quote((Expr.splice(Expr(k)), Expr.splice(Expr(v))) :: Expr.splice(listAcc))
              }
              Expr.quote {
                val f = Expr.splice(baseFieldExpr)
                Expr.splice(propsListExpr).foreach { case (k, v) =>
                  AvroDerivationUtils.addFieldProp(f, k, v)
                }
                f
              }
            }
          // Apply field-level @avroAlias annotations
          val fieldExpr: Expr[Schema.Field] =
            if (fieldAliases.isEmpty) fieldWithPropsExpr
            else {
              val aliasesListExpr = fieldAliases.foldRight(Expr.quote(List.empty[String])) { (alias, listAcc) =>
                Expr.quote(Expr.splice(Expr(alias)) :: Expr.splice(listAcc))
              }
              Expr.quote {
                val f = Expr.splice(fieldWithPropsExpr)
                Expr.splice(aliasesListExpr).foreach(a => AvroDerivationUtils.addFieldAlias(f, a))
                f
              }
            }
          Expr.quote(Expr.splice(fieldExpr) :: Expr.splice(acc))
      }

    private def applyClassAnnotations(
        schemaExpr: Expr[Schema],
        classProps: List[(String, String)],
        classAliases: List[String]
    )(implicit SchemaT: Type[Schema], StringT: Type[String]): Expr[Schema] = {
      // Apply class-level @avroProp annotations
      val withPropsExpr: Expr[Schema] =
        if (classProps.isEmpty) schemaExpr
        else {
          val propsListExpr = classProps.foldRight(Expr.quote(List.empty[(String, String)])) { case ((k, v), listAcc) =>
            Expr.quote((Expr.splice(Expr(k)), Expr.splice(Expr(v))) :: Expr.splice(listAcc))
          }
          Expr.quote {
            val s = Expr.splice(schemaExpr)
            Expr.splice(propsListExpr).foreach { case (k, v) =>
              AvroDerivationUtils.addSchemaProp(s, k, v)
            }
            s
          }
        }
      // Apply class-level @avroAlias annotations
      if (classAliases.isEmpty) withPropsExpr
      else {
        val aliasesListExpr = classAliases.foldRight(Expr.quote(List.empty[String])) { (alias, listAcc) =>
          Expr.quote(Expr.splice(Expr(alias)) :: Expr.splice(listAcc))
        }
        Expr.quote {
          val s = Expr.splice(withPropsExpr)
          Expr.splice(aliasesListExpr).foreach(a => AvroDerivationUtils.addSchemaAlias(s, a))
          s
        }
      }
    }
  }
}
