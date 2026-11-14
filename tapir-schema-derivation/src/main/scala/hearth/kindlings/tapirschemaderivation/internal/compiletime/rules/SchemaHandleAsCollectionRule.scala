package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import hearth.kindlings.tapirschemaderivation.internal.runtime.TapirSchemaUtils
import sttp.tapir.Schema

trait SchemaHandleAsCollectionRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaHandleAsCollectionRule extends SchemaDerivationRule("handle as collection when possible") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Element
            Log.info(s"Deriving Schema for collection element: ${Type[Element].prettyPrint}") >>
              deriveSchemaRecursively[Element](using sctx.nest[Element]).map { elementSchema =>
                // Same as tapir's schemaForSet: sets are marked as having unique items
                val schema: Expr[Schema[A]] =
                  if (Type[A] <:< Type.of[scala.collection.Set[Element]])
                    Expr.quote {
                      TapirSchemaUtils.setSchema[Element](Expr.splice(elementSchema)).asInstanceOf[Schema[A]]
                    }
                  else if (Type[A] <:< Type.of[Iterable[Element]])
                    Expr.quote {
                      TapirSchemaUtils.collectionSchema[Element](Expr.splice(elementSchema)).asInstanceOf[Schema[A]]
                    }
                  else {
                    // not an Iterable (e.g. cats' NonEmptyList): its elements are reached through the provider
                    val toIterable: Expr[Any => Iterable[Element]] = Expr.quote { (value: Any) =>
                      Expr.splice(isCollection.value.asIterable(Expr.quote(value.asInstanceOf[A])))
                    }
                    Expr.quote {
                      TapirSchemaUtils
                        .collectionSchemaVia[Element](Expr.splice(elementSchema), Expr.splice(toIterable))
                        .asInstanceOf[Schema[A]]
                    }
                  }
                // Non-empty collections (e.g. cats' NonEmptyList): required, with minItems: 1
                Rule.matched(nonEmptyIfNeeded[A, Element](isCollection.value, schema))
              }
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
