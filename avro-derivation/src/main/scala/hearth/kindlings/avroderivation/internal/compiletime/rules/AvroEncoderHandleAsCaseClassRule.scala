package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.annotations.{
  avroFixed,
  avroNamespace,
  avroScalePrecision,
  fieldName,
  transientField
}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema
import org.apache.avro.generic.GenericData

trait AvroEncoderHandleAsCaseClassRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  private def cachedSchemaForEncode[A: Type](implicit ctx: EncoderCtx[A]): MIO[Expr[Schema]] =
    cachedEncodeSchema[A]

  @scala.annotation.nowarn("msg=is never used")
  private def inlineBuiltInAvroEncode[Field: Type](value: Expr[Field])(implicit AnyT: Type[Any]): Option[Expr[Any]] =
    if (Type[Field] =:= Type.of[Boolean]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else if (Type[Field] =:= Type.of[Int]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else if (Type[Field] =:= Type.of[Long]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else if (Type[Field] =:= Type.of[Float]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else if (Type[Field] =:= Type.of[Double]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else if (Type[Field] =:= Type.of[String]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else None

  // in a method of its own: a path-dependent type (`param.tpe.Underlying`) cannot be referred to inside a quote
  private def productElementAs[A: Type, Field: Type](value: Expr[A], index: Int): Expr[Field] =
    Expr.quote(AvroDerivationUtils.productElement(Expr.splice(value), Expr.splice(Expr(index))).asInstanceOf[Field])

  object AvroEncoderHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            encodeCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeCaseClassFields[A: EncoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Any]] = {
      implicit val AnyT: Type[Any] = EncTypes.Any
      implicit val StringT: Type[String] = EncTypes.String
      implicit val SchemaT: Type[Schema] = EncTypes.Schema
      implicit val fieldNameT: Type[fieldName] = EncTypes.FieldName
      implicit val transientFieldT: Type[transientField] = EncTypes.TransientField
      implicit val avroFixedT: Type[avroFixed] = EncTypes.AvroFixed
      implicit val avroScalePrecisionT: Type[avroScalePrecision] = SfTypes.AvroScalePrecision

      implicit val avroNamespaceT: Type[avroNamespace] = SfTypes.AvroNamespace

      val accessibleFields = caseClass.caseFieldValuesAt(ectx.value)

      // Singletons (case objects, parameterless enum cases) have no primary constructor.
      // Only access primaryConstructor when there are actual fields to process.
      val isSingleton = accessibleFields.isEmpty && SingletonValue.unapply(Type[A]).isDefined
      val constructorParams: List[(String, Parameter)] =
        if (isSingleton) Nil else caseClass.primaryConstructor.totalParameters.headOption.toList.flatten
      val paramsByName: Map[String, Parameter] =
        if (isSingleton) Map.empty else caseClass.primaryConstructor.totalParameters.flatten.toMap

      // Fields without an accessor (`private val`) are still part of the record (as with avro4s): they are read
      // through `Product`, by position.
      val allFields: List[(String, Expr_??)] =
        if (constructorParams.forall { case (name, _) => accessibleFields.contains(name) }) accessibleFields.toList
        else
          constructorParams.zipWithIndex.map { case ((name, param), index) =>
            accessibleFields.get(name) match {
              case Some(fieldValue) => name -> fieldValue
              case None             =>
                import param.tpe.Underlying as Field
                name -> productElementAs[A, Field](ectx.value, index).as_??
            }
          }

      // Validate: @transientField on fields without defaults is a compile error
      paramsByName.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && transientFieldHasNoFallback(param) => name
      } match {
        case Some(name) =>
          val err = EncoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case None =>
          val nonTransientFields = allFields.filter { case (name, _) =>
            paramsByName.get(name).forall(p => !hasAnnotationType[transientField](p))
          }

          NonEmptyList.fromList(nonTransientFields) match {
            case Some(fields) =>
              fields
                .parTraverse { case (fName, fieldValue) =>
                  import fieldValue.{Underlying as Field, value as fieldExpr}
                  val param = paramsByName.get(fName)
                  val avroFixedSize = param.flatMap(p => getAnnotationIntArg[avroFixed](p))
                  val decimalOverride = param.flatMap(p => getAnnotationTwoIntArgs[avroScalePrecision](p))
                  Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fName: ${Type[Field].prettyPrint}") {
                    val encodeMIO: MIO[Expr[Any]] = (avroFixedSize, decimalOverride) match {
                      case (Some(size), _) if Type[Field] =:= Type.of[String] =>
                        MIO.pure(Expr.quote {
                          AvroDerivationUtils.wrapStringAsFixed(
                            Expr.splice(fieldExpr).asInstanceOf[String],
                            Expr.splice(Expr(size))
                          ): Any
                        })
                      case (Some(size), _) =>
                        MIO.pure(Expr.quote {
                          AvroDerivationUtils.wrapByteArrayAsFixed(
                            Expr.splice(fieldExpr).asInstanceOf[Array[Byte]],
                            Expr.splice(Expr(size))
                          ): Any
                        })
                      case (_, Some((precision, scale))) =>
                        MIO.pure(
                          encodeDecimalExpr(
                            Expr.quote(Expr.splice(fieldExpr).asInstanceOf[BigDecimal]),
                            precision,
                            scale,
                            ectx.config,
                            ectx.evaluatedConfig
                          )
                        )
                      case _ =>
                        inlineBuiltInAvroEncode[Field](fieldExpr) match {
                          case Some(enc) => MIO.pure(enc)
                          case None      => deriveEncoderRecursively[Field](using ectx.nest(fieldExpr))
                        }
                    }
                    encodeMIO.map { fieldEncoded =>
                      // the named types of a field with @avroNamespace live in that namespace in the record's schema
                      val movedToNamespace =
                        avroFixedSize.isEmpty && param.exists(p => getAnnotationStringArg[avroNamespace](p).isDefined)
                      (fName, fieldEncoded, movedToNamespace)
                    }
                  }
                }
                .flatMap { fieldPairs =>
                  val fieldPutExprs: List[(Int, Expr[Any], Boolean)] =
                    fieldPairs.toList.zipWithIndex.map { case ((_, fieldEncoded, movedToNamespace), idx) =>
                      (idx, fieldEncoded, movedToNamespace)
                    }

                  cachedSchemaForEncode[A].map { schemaExpr =>
                    val recordPuts: Expr[GenericData.Record] => Expr[Unit] = { recordExpr =>
                      fieldPutExprs.foldLeft(Expr.quote(()): Expr[Unit]) {
                        case (acc, (idx, fieldEncoded, false)) =>
                          Expr.quote {
                            Expr.splice(acc)
                            Expr.splice(recordExpr).put(Expr.splice(Expr(idx)), Expr.splice(fieldEncoded))
                          }
                        case (acc, (idx, fieldEncoded, true)) =>
                          Expr.quote {
                            Expr.splice(acc)
                            Expr
                              .splice(recordExpr)
                              .put(
                                Expr.splice(Expr(idx)),
                                AvroDerivationUtils.rebindToSchema(
                                  Expr.splice(fieldEncoded),
                                  Expr.splice(recordExpr).getSchema.getFields.get(Expr.splice(Expr(idx))).schema
                                )
                              )
                          }
                      }
                    }
                    Expr.quote {
                      val record = new GenericData.Record(Expr.splice(schemaExpr))
                      Expr.splice(recordPuts(Expr.quote(record)))
                      record: Any
                    }
                  }
                }
            case None =>
              cachedSchemaForEncode[A].map { schemaExpr =>
                Expr.quote {
                  val record = new GenericData.Record(Expr.splice(schemaExpr))
                  record: Any
                }
              }
          }
      }
    }
  }
}
