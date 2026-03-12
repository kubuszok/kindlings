package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.apache.avro.Schema

trait AvroSchemaForHandleAsCollectionRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForHandleAsCollectionRule extends SchemaDerivationRule("handle as collection when possible") {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            for {
              itemSchema <- deriveSchemaRecursively[Item](using sfctx.nest[Item])
            } yield Rule.matched(Expr.quote {
              Schema.createArray(Expr.splice(itemSchema))
            })

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
