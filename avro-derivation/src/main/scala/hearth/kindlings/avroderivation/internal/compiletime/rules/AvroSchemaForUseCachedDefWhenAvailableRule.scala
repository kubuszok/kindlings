package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.apache.avro.Schema

trait AvroSchemaForUseCachedDefWhenAvailableRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForUseCachedDefWhenAvailableRule extends SchemaDerivationRule("use cached def when available") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to use cached schema for ${Type[A].prettyPrint}") >>
        sfctx.getCachedSchema[A].flatMap {
          case Some(cachedSchema) =>
            Log.info(s"Found cached schema for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(cachedSchema))
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached schema"))
        }
  }
}
