package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.annotations.{avroFixed, fieldName, transientField}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema
import org.apache.avro.generic.GenericData

trait AvroEncoderHandleAsCaseClassRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              _ <- ectx.setHelper[A] { (value, config) =>
                encodeCaseClassFields[A](caseClass)(using ectx.nestInCache(value, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

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
                  Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fName: ${Type[Field].prettyPrint}") {
                    val encodeMIO: MIO[Expr[Any]] = avroFixedSize match {
                      case Some(size) =>
                        MIO.pure(Expr.quote {
                          AvroDerivationUtils.wrapByteArrayAsFixed(
                            Expr.splice(fieldExpr).asInstanceOf[Array[Byte]],
                            Expr.splice(Expr(size))
                          ): Any
                        })
                      case None =>
                        deriveEncoderRecursively[Field](using ectx.nest(fieldExpr))
                    }
                    encodeMIO.map { fieldEncoded =>
                      val nameOverride = param.flatMap(p => getAnnotationStringArg[fieldName](p))
                      (fName, fieldEncoded, nameOverride)
                    }
                  }
                }
                .flatMap { fieldPairs =>
                  val fieldsListExpr = fieldPairs.toList.foldRight(
                    Expr.quote(List.empty[(String, Any)])
                  ) {
                    case ((fName, fieldEncoded, Some(customName)), acc) =>
                      Expr.quote {
                        (
                          Expr.splice(Expr(customName)),
                          Expr.splice(fieldEncoded)
                        ) :: Expr.splice(acc)
                      }
                    case ((fName, fieldEncoded, None), acc) =>
                      Expr.quote {
                        (
                          Expr.splice(ectx.config).transformFieldNames(Expr.splice(Expr(fName))),
                          Expr.splice(fieldEncoded)
                        ) :: Expr.splice(acc)
                      }
                  }

                  deriveSelfContainedSchema[A](ectx.config).map { schemaExpr =>
                    Expr.quote {
                      val schema = Expr.splice(schemaExpr)
                      val fields = Expr.splice(fieldsListExpr)
                      val record = new GenericData.Record(schema)
                      fields.foreach { case (name, value) =>
                        record.put(name, value)
                      }
                      record: Any
                    }
                  }
                }
            case None =>
              deriveSelfContainedSchema[A](ectx.config).map { schemaExpr =>
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
