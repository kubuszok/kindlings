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
  avroScalePrecision,
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
      implicit val avroScalePrecisionT: Type[avroScalePrecision] = SfTypes.AvroScalePrecision
      implicit val avroErrorT: Type[avroError] = SfTypes.AvroError
      implicit val avroPropT: Type[avroProp] = SfTypes.AvroProp
      implicit val avroAliasT: Type[avroAlias] = SfTypes.AvroAlias
      implicit val avroNoDefaultT: Type[avroNoDefault] = SfTypes.AvroNoDefault
      implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig

      // Read class-level annotations
      val classDoc: Option[String] = getTypeAnnotationStringArg[avroDoc, A]
      val isError: Boolean = hasTypeAnnotation[avroError, A]
      val classProps: List[(String, String)] = getAllTypeAnnotationTwoStringArgs[avroProp, A]
      val classAliases: List[String] = getAllTypeAnnotationStringArgs[avroAlias, A]

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList
      val typeNameExpr = computeAvroNameExpr[A]

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && transientFieldHasNoFallback(param) => name
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

      // Build the namespace expression: field-level @avroNamespace > type-level @avroNamespace > config.namespace > package name > ""
      val namespaceExpr: Expr[String] = computeNamespaceExpr[A]

      // After consuming namespaceOverride for this record, clear it so it does not leak into nested field derivations.
      val cleanCtx: SchemaForCtx[A] = sfctx[A].copy(namespaceOverride = None)

      // Build the doc expression (null if no @avroDoc)
      val docExprOrNull: Expr[String] = classDoc match {
        case Some(doc) => Expr(doc)
        case None      => Expr.quote(null: String)
      }

      // Step 1: Register a self-record (empty record without fields) to break recursion cycles.
      // Recursive field derivation finds this via SfCheckSelfRecordRule.
      val emptyRecordExpr: Expr[Schema] = Expr.quote {
        AvroDerivationUtils.createEmptyRecord(
          Expr.splice(typeNameExpr),
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
                // as avro4s: the @avroDoc of a value class documents the fields of that type (it has no schema of
                // its own to carry the doc)
                val fieldDoc = getAnnotationStringArg[avroDoc](param).orElse {
                  if (Type[Field] <:< Type.of[AnyVal]) getTypeAnnotationStringArg[avroDoc, Field] else None
                }
                val fieldDefault =
                  if (hasAnnotationType[avroNoDefault](param)) None
                  else getAnnotationStringArg[avroDefault](param)
                val avroFixedSize = getAnnotationIntArg[avroFixed](param)
                val decimalOverride = getAnnotationTwoIntArgs[avroScalePrecision](param)
                val fieldProps = getAllAnnotationTwoStringArgs[avroProp](param)
                val fieldAliases = getAllAnnotationStringArgs[avroAlias](param)
                val fieldNamespaceOverride = getAnnotationStringArg[avroNamespace](param)
                // The Scala default value, used as the schema default when `AvroConfig.scalaDefaultsInSchema` is on
                // (unless `@avroDefault`/`@avroNoDefault` decide the default).
                val scalaDefault: Option[Expr[Any]] =
                  if (fieldDefault.isDefined || hasAnnotationType[avroNoDefault](param)) None
                  else
                    param.defaultValue.flatMap { method =>
                      foldInstanceFree(method, "Default value")(
                        onTypes = _ => Map.empty,
                        onValues = _ => Map.empty
                      ).toOption.map { defaultExpr =>
                        import defaultExpr.Underlying as Default
                        Expr.quote(Expr.splice(defaultExpr.value): Any)
                      }
                    }
                Log.namedScope(s"Deriving schema for field $fName: ${Type[Field].prettyPrint}") {
                  (avroFixedSize, decimalOverride) match {
                    case (Some(_), _) if !(Type[Field] =:= Type.of[Array[Byte]] || Type[Field] =:= Type.of[String]) =>
                      val err = SchemaDerivationError.AvroFixedOnNonByteArray(
                        fName,
                        Type[A].prettyPrint,
                        Type[Field].prettyPrint
                      )
                      Log.error(err.message) >> MIO.fail(err)
                    case (Some(size), _) =>
                      val fixedName = nameOverride.getOrElse(fName)
                      // as avro4s: the fixed type lives in the field's `@avroNamespace` or the enclosing record's
                      val fixedNamespaceExpr: Expr[String] = fieldNamespaceOverride match {
                        case Some(ns) => Expr(ns)
                        case None     => namespaceExpr
                      }
                      MIO.pure {
                        val fieldSchema = Expr.quote {
                          AvroDerivationUtils.createFixed(
                            Expr.splice(Expr(fixedName)),
                            Expr.splice(fixedNamespaceExpr),
                            Expr.splice(Expr(size))
                          )
                        }
                        (
                          fName,
                          fieldSchema,
                          nameOverride,
                          fieldDoc,
                          fieldDefault,
                          fieldProps,
                          fieldAliases,
                          scalaDefault
                        )
                      }
                    case (_, Some((precision, scale))) =>
                      MIO.pure {
                        val fieldSchema = Expr.quote {
                          AvroDerivationUtils.decimalSchema(
                            Expr.splice(Expr(precision)),
                            Expr.splice(Expr(scale))
                          )
                        }
                        (
                          fName,
                          fieldSchema,
                          nameOverride,
                          fieldDoc,
                          fieldDefault,
                          fieldProps,
                          fieldAliases,
                          scalaDefault
                        )
                      }
                    case _ =>
                      // as avro4s: the field's schema is derived as usual and then every named type in it is moved
                      // to the field's namespace (at runtime: the types' names are not all known before)
                      deriveSchemaRecursively[Field](using cleanCtx.nest[Field]).map { derivedFieldSchema =>
                        val namespacedFieldSchema: Expr[Schema] = fieldNamespaceOverride match {
                          case Some(ns) =>
                            Expr.quote {
                              AvroDerivationUtils.overrideNamespace(
                                Expr.splice(derivedFieldSchema),
                                Expr.splice(Expr(ns))
                              )
                            }
                          case None => derivedFieldSchema
                        }
                        // as avro4s 5: @avroError on a field makes the field's record an error record
                        val fieldSchema: Expr[Schema] =
                          if (hasAnnotationType[avroError](param))
                            Expr.quote(AvroDerivationUtils.asErrorRecord(Expr.splice(namespacedFieldSchema)))
                          else namespacedFieldSchema
                        (
                          fName,
                          fieldSchema,
                          nameOverride,
                          fieldDoc,
                          fieldDefault,
                          fieldProps,
                          fieldAliases,
                          scalaDefault
                        )
                      }
                  }
                }
              }
              .map { fieldPairs =>
                val addFields = buildFieldsExpr(fieldPairs.toList)
                // Step 3: set the fields on the self-record and apply annotations. The fields are added one statement
                // at a time (instead of `f0 :: f1 :: ... :: Nil`), so no field is built in the middle of an expression:
                // that would make the compiler keep every previous field on the stack and emit a stack map frame per
                // branch listing all of them.
                applyClassAnnotations(
                  Expr.quote {
                    val recordFields = new java.util.ArrayList[Schema.Field](Expr.splice(Expr(fieldPairs.toList.size)))
                    Expr.splice(addFields(Expr.quote(recordFields)))
                    AvroDerivationUtils.setRecordFields(Expr.splice(selfRecordRef), recordFields)
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
          (
              String,
              Expr[Schema],
              Option[String],
              Option[String],
              Option[String],
              List[(String, String)],
              List[String],
              Option[Expr[Any]]
          )
        ]
    )(implicit
        SchemaT: Type[Schema],
        StringT: Type[String],
        AvroConfigT: Type[AvroConfig]
    ): Expr[java.util.ArrayList[Schema.Field]] => Expr[Unit] = recordFields => {
      implicit val SchemaFieldT: Type[Schema.Field] = SfTypes.SchemaField
      fieldPairs.foldLeft(Expr.quote(()): Expr[Unit]) {
        case (
              acc,
              (fName, fieldSchema, nameOverride, fieldDoc, fieldDefault, fieldProps, fieldAliases, scalaDefault)
            ) =>
          val nameExpr: Expr[String] = nameOverride match {
            case Some(customName) => Expr(customName)
            case None             =>
              sfctx.evaluatedConfig match {
                // Config statically known: map the field name at compile time to a constant string,
                // dropping the per-field runtime `config.transformFieldNames(name)` call.
                case Some(cfg) => Expr(cfg.transformFieldNames(fName))
                case None      =>
                  Expr.quote {
                    Expr.splice(sfctx.config).transformFieldNames(Expr.splice(Expr(fName)))
                  }
              }
          }
          def annotatedFieldExpr(fieldSchema: Expr[Schema]): Expr[Schema.Field] = (fieldDoc, fieldDefault) match {
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
          val baseFieldExpr: Expr[Schema.Field] = scalaDefault match {
            case Some(default) =>
              val docExpr: Expr[String] = fieldDoc match {
                case Some(doc) => Expr(doc)
                case None      => Expr.quote(null: String)
              }
              def withScalaDefault(fieldSchema: Expr[Schema]): Expr[Schema.Field] = Expr.quote {
                AvroDerivationUtils.createFieldWithScalaDefault(
                  Expr.splice(nameExpr),
                  Expr.splice(fieldSchema),
                  Expr.splice(docExpr),
                  Expr.splice(default)
                )
              }
              sfctx.evaluatedConfig match {
                // Config statically known: emit only the branch it selects.
                case Some(cfg) =>
                  if (cfg.scalaDefaultsInSchema) withScalaDefault(fieldSchema) else annotatedFieldExpr(fieldSchema)
                // Otherwise build the field schema once and choose at runtime, in a local def of its own: a branch
                // inline in the schema initialiser would need a stack map frame listing all the locals before it.
                case None =>
                  ValDefs
                    .createDef[Schema.Field](
                      Expr.quote {
                        val sharedFieldSchema = Expr.splice(fieldSchema)
                        if (Expr.splice(sfctx.config).scalaDefaultsInSchema)
                          Expr.splice(withScalaDefault(Expr.quote(sharedFieldSchema)))
                        else Expr.splice(annotatedFieldExpr(Expr.quote(sharedFieldSchema)))
                      },
                      FreshName.FromPrefix(s"schemaField_$fName")
                    )
                    .use(call => call)
              }
            case None => annotatedFieldExpr(fieldSchema)
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
          Expr.quote {
            Expr.splice(acc)
            AvroDerivationUtils.addRecordField(Expr.splice(recordFields), Expr.splice(fieldExpr))
          }
      }
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
