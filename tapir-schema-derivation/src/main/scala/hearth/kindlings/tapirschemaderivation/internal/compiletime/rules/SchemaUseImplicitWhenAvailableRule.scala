package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema

trait SchemaUseImplicitWhenAvailableRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaUseImplicitWhenAvailableRule extends SchemaDerivationRule("use implicit when available") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to use implicit Schema for ${Type[A].prettyPrint}") >> {
        if (sctx.derivedType.exists(_.Underlying =:= Type[A])) {
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search"))
        } else {
          implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
          Type[Schema[A]].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(expr) =>
              Log.info(s"Using summoned implicit Schema for ${Type[A].prettyPrint}") >>
                setCachedAndGet[A](sctx.cache, expr).map(Rule.matched)
            case Left(_) =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have an implicit Schema"))
          }
        }
      }
  }
}
