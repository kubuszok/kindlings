package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema

trait SchemaHandleAsCaseClassRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaHandleAsCaseClassRule extends SchemaDerivationRule("handle as case class when possible") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(cc) =>
            Log.info(s"Deriving Schema for case class ${Type[A].prettyPrint}") >>
              sctx.inProgress.get.flatMap { inProgressSet =>
                (for {
                  _ <- sctx.inProgress.set(inProgressSet + sctx.cacheKey)
                  result <- deriveCaseClassSchema[A](cc)
                  _ <- sctx.inProgress.set(inProgressSet)
                } yield result).map(Rule.matched)
              }
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
