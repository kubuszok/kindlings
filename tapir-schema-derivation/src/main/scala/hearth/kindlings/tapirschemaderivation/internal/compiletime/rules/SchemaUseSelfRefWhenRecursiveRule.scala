package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import hearth.kindlings.tapirschemaderivation.internal.runtime.TapirSchemaUtils
import sttp.tapir.Schema
import sttp.tapir.Schema.SName

trait SchemaUseSelfRefWhenRecursiveRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaUseSelfRefWhenRecursiveRule extends SchemaDerivationRule("use self-ref for recursive type") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Checking for recursive reference for ${Type[A].prettyPrint}") >>
        sctx.inProgress.get.flatMap { inProgressSet =>
          if (inProgressSet.contains(sctx.cacheKey)) {
            @scala.annotation.nowarn("msg=is never used")
            implicit val sNameT: Type[SName] = TsTypes.SNameType
            @scala.annotation.nowarn("msg=is never used")
            implicit val utilsT: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils
            val sNameExpr = computeSNameExpr[A](sctx.derivedType)
            Log.info(s"Recursive reference detected for ${Type[A].prettyPrint}, emitting SRef") >>
              MIO.pure(Rule.matched(Expr.quote {
                TapirSchemaUtils.refSchema[A](Expr.splice(sNameExpr))
              }))
          } else {
            MIO.pure(Rule.yielded(s"No recursive reference for ${Type[A].prettyPrint}"))
          }
        }
  }
}
