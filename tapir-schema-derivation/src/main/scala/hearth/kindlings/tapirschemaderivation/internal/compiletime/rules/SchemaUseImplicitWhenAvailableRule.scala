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

    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[KindlingsSchema.type].methods.collect {
        case method if method.value.isImplicit => method.value.asUntyped
      } ++ Type.of[Schema.type].methods.collect {
        // For tapir's own Schema companion, only ignore the auto-derivation method,
        // not built-in schemas for primitive types (schemaForString, schemaForInt, etc.).
        case method if method.value.name == "derivedSchema" => method.value.asUntyped
      }

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to use implicit Schema for ${Type[A].prettyPrint}") >> {
        // Skip summoning for Map types — Tapir provides built-in Schema[Map[K,V]] but we need structural derivation
        val isMapType: Boolean = Type[A] match {
          case IsMap(_) => true
          case _        => false
        }
        if (isMapType) {
          Log.info(s"Map type detected, skipping summoning for ${Type[A].prettyPrint}") >>
            MIO.pure(Rule.yielded(s"Map type ${Type[A].prettyPrint} requires structural derivation"))
        } else if (sctx.derivedType.exists(_.Underlying =:= Type[A])) {
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
