package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema

trait SchemaHandleAsSingletonRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaHandleAsSingletonRule extends SchemaDerivationRule("handle as singleton when possible") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(_) =>
            Log.info(s"Deriving Schema for singleton ${Type[A].prettyPrint}") >>
              sctx.inProgress.get.flatMap { inProgressSet =>
                (for {
                  _ <- sctx.inProgress.set(inProgressSet + sctx.cacheKey)
                  result <- deriveSingletonSchema[A]
                  _ <- sctx.inProgress.set(inProgressSet)
                } yield result).map(Rule.matched)
              }
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
