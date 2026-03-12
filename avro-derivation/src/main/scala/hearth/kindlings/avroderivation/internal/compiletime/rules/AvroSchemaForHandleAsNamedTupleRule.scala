package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.AvroConfig
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroSchemaForHandleAsNamedTupleRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForHandleAsNamedTupleRule extends SchemaDerivationRule("handle as named tuple when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              schemaExpr <- deriveNamedTupleSchema[A](namedTuple.primaryConstructor)
              _ <- sfctx.setCachedSchema[A](schemaExpr)
              result <- sfctx.getCachedSchema[A].flatMap {
                case Some(cachedSchema) => MIO.pure(Rule.matched(cachedSchema))
                case None               => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveNamedTupleSchema[A: SchemaForCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[Schema]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      implicit val StringT: Type[String] = SfTypes.String
      implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig

      val fields = constructor.parameters.flatten.toList
      val typeName = Type[A].shortName

      NonEmptyList.fromList(fields) match {
        case None =>
          MIO.pure(Expr.quote {
            AvroDerivationUtils.createRecord(
              Expr.splice(Expr(typeName)),
              Expr.splice(sfctx.config).namespace.getOrElse(""),
              java.util.Collections.emptyList[Schema.Field]()
            )
          })
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving schema for named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveSchemaRecursively[Field](using sfctx.nest[Field]).map { fieldSchema =>
                  (fName, fieldSchema)
                }
              }
            }
            .map { fieldPairs =>
              val javaFieldsExpr = fieldPairs.toList.foldRight(
                Expr.quote(List.empty[Schema.Field])
              ) { case ((fName, fieldSchema), acc) =>
                val nameExpr: Expr[String] = Expr.quote {
                  Expr.splice(sfctx.config).transformFieldNames(Expr.splice(Expr(fName)))
                }
                val fieldExpr: Expr[Schema.Field] = Expr.quote {
                  AvroDerivationUtils.createField(
                    Expr.splice(nameExpr),
                    Expr.splice(fieldSchema)
                  )
                }
                Expr.quote(Expr.splice(fieldExpr) :: Expr.splice(acc))
              }
              val fieldsExpr = Expr.quote {
                val fieldsList = Expr.splice(javaFieldsExpr)
                val javaFields = new java.util.ArrayList[Schema.Field](fieldsList.size)
                fieldsList.foreach(javaFields.add)
                (javaFields: java.util.List[Schema.Field])
              }
              Expr.quote {
                AvroDerivationUtils.createRecord(
                  Expr.splice(Expr(typeName)),
                  Expr.splice(sfctx.config).namespace.getOrElse(""),
                  Expr.splice(fieldsExpr)
                )
              }
            }
      }
    }
  }
}
