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

  private def cachedSchemaForEncode[A: Type](implicit ctx: EncoderCtx[A]): MIO[Expr[Schema]] =
    ctx.precomputedSchema match {
      case Some(schemaExpr) =>
        Log.info(s"Using precomputed schema for ${Type[A].prettyPrint}") >>
          MIO.pure(schemaExpr)
      case None =>
        ctx.getCachedSchemaForEncode[A].flatMap {
          case Some(cached) =>
            Log.info(s"Using encoder-cached schema for ${Type[A].prettyPrint}") >>
              MIO.pure(cached)
          case None =>
            for {
              schema <- deriveSelfContainedSchema[A](ctx.schemaConfig)
              _ <- ctx.setCachedSchemaForEncode[A](schema)
              result <- ctx.getCachedSchemaForEncode[A]
            } yield result.get
        }
    }

  @scala.annotation.nowarn("msg=is never used")
  private def inlineBuiltInAvroEncode[Field: Type](value: Expr[Field])(implicit AnyT: Type[Any]): Option[Expr[Any]] =
    if (Type[Field] =:= Type.of[Boolean]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else if (Type[Field] =:= Type.of[Int]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else if (Type[Field] =:= Type.of[Long]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else if (Type[Field] =:= Type.of[Float]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else if (Type[Field] =:= Type.of[Double]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else if (Type[Field] =:= Type.of[String]) Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
    else None

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
                        inlineBuiltInAvroEncode[Field](fieldExpr) match {
                          case Some(enc) => MIO.pure(enc)
                          case None      => deriveEncoderRecursively[Field](using ectx.nest(fieldExpr))
                        }
                    }
                    encodeMIO.map { fieldEncoded =>
                      val nameOverride = param.flatMap(p => getAnnotationStringArg[fieldName](p))
                      (fName, fieldEncoded, nameOverride)
                    }
                  }
                }
                .flatMap { fieldPairs =>
                  val fieldPutExprs: List[(Int, Expr[Any])] =
                    fieldPairs.toList.zipWithIndex.map { case ((_, fieldEncoded, _), idx) =>
                      (idx, fieldEncoded)
                    }

                  cachedSchemaForEncode[A].map { schemaExpr =>
                    val recordPuts: Expr[GenericData.Record] => Expr[Unit] = { recordExpr =>
                      fieldPutExprs.foldLeft(Expr.quote(()): Expr[Unit]) { case (acc, (idx, fieldEncoded)) =>
                        Expr.quote {
                          Expr.splice(acc)
                          Expr.splice(recordExpr).put(Expr.splice(Expr(idx)), Expr.splice(fieldEncoded))
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
