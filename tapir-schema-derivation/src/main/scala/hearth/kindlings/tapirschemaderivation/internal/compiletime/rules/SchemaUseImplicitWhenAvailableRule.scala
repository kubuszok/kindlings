package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import hearth.kindlings.tapirschemaderivation.KindlingsSchema
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
          implicit val KindlingsSchemaA: Type[KindlingsSchema[A]] = TsTypes.KindlingsSchemaOf[A]
          Type[Schema[A]].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(expr) =>
              Log.info(s"Using summoned implicit Schema for ${Type[A].prettyPrint}") >>
                setCachedAndGet[A](sctx.cache, expr).map(Rule.matched)
            case Left(_) =>
              // KindlingsSchema[A] cannot extend Schema[A] (a case class), so an explicitly provided KindlingsSchema[A]
              // (e.g. the `using KindlingsSchema[A]` evidence that Scala 3 synthesizes for `derives KindlingsSchema` on
              // a generic type, or `implicit def box[A: KindlingsSchema]: KindlingsSchema[Box[A]]`) has to be looked for
              // separately. The auto-derivation (KindlingsSchema.derived) is still ignored.
              Type[KindlingsSchema[A]].summonExprIgnoring(ignoredImplicits*).toEither match {
                case Right(kindlingsSchema) =>
                  Log.info(s"Using summoned implicit KindlingsSchema for ${Type[A].prettyPrint}") >>
                    setCachedAndGet[A](sctx.cache, Expr.quote(Expr.splice(kindlingsSchema).schema)).map(Rule.matched)
                case Left(_) =>
                  MIO.pure(
                    Rule.yielded(
                      s"The type ${Type[A].prettyPrint} does not have an implicit Schema nor KindlingsSchema"
                    )
                  )
              }
          }
        }
      }
  }
}
