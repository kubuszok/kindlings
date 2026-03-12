package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.apache.avro.Schema
import org.apache.avro.generic.GenericData

trait AvroEncoderHandleAsNamedTupleRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderHandleAsNamedTupleRule extends EncoderDerivationRule("handle as named tuple when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- ectx.setHelper[A] { (value, config) =>
                encodeNamedTupleFields[A](namedTuple.primaryConstructor)(using ectx.nestInCache(value, config))
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
    private def encodeNamedTupleFields[A: EncoderCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[Any]] = {
      implicit val AnyT: Type[Any] = EncTypes.Any
      implicit val StringT: Type[String] = EncTypes.String
      implicit val SchemaT: Type[Schema] = EncTypes.Schema
      implicit val ProductType: Type[Product] = EncTypes.Product
      implicit val IntType: Type[Int] = EncTypes.Int

      val fields = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fields) match {
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val fieldExpr: Expr[Field] = Expr.quote {
                Expr
                  .splice(ectx.value)
                  .asInstanceOf[Product]
                  .productElement(Expr.splice(Expr(param.index)))
                  .asInstanceOf[Field]
              }
              Log.namedScope(s"Encoding named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldEncoded =>
                  (fName, fieldEncoded)
                }
              }
            }
            .flatMap { fieldPairs =>
              val fieldsListExpr = fieldPairs.toList.foldRight(
                Expr.quote(List.empty[(String, Any)])
              ) { case ((fName, fieldEncoded), acc) =>
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
