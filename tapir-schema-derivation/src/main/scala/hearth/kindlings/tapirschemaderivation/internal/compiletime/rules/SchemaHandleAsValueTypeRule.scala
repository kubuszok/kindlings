package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema

trait SchemaHandleAsValueTypeRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaHandleAsValueTypeRule extends SchemaDerivationRule("handle as value type when possible") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            Log.info(s"Deriving Schema for value type ${Type[A].prettyPrint} as ${Type[Inner].prettyPrint}") >>
              deriveSchemaRecursively[Inner](using sctx.nest[Inner]).map { innerSchema =>
                Rule.matched(Expr.quote {
                  Expr.splice(innerSchema).asInstanceOf[Schema[A]]
                })
              }
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }
}
