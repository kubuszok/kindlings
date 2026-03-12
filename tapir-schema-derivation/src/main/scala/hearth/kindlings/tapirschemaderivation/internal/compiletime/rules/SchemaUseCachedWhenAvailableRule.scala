package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema

trait SchemaUseCachedWhenAvailableRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaUseCachedWhenAvailableRule extends SchemaDerivationRule("use cached schema when available") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] = {
      implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
      Log.info(s"Attempting to use cached schema for ${Type[A].prettyPrint}") >>
        sctx.cache.get0Ary[Schema[A]](sctx.cacheKey).flatMap {
          case Some(cached) =>
            Log.info(s"Using cached Schema for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(cached))
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached schema"))
        }
    }
  }
}
