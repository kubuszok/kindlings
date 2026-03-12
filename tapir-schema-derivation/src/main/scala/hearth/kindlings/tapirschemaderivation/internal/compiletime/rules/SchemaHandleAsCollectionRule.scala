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
                Rule.matched(Expr.quote {
                  TapirSchemaUtils.collectionSchema[Element](Expr.splice(elementSchema)).asInstanceOf[Schema[A]]
                })
              }
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
