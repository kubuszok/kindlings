package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema

trait SchemaHandleAsEnumRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaHandleAsEnumRule extends SchemaDerivationRule("handle as enum when possible") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as enum") >> {
        Enum.parse[A].toEither match {
          case Right(e) =>
            Log.info(s"Deriving Schema for enum/sealed ${Type[A].prettyPrint}") >>
              sctx.inProgress.get.flatMap { inProgressSet =>
                (for {
                  _ <- sctx.inProgress.set(inProgressSet + sctx.cacheKey)
                  result <- deriveEnumSchema[A](e)
                  _ <- sctx.inProgress.set(inProgressSet)
                } yield result).map(Rule.matched)
              }
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason.toString))
        }
      }
  }
}
