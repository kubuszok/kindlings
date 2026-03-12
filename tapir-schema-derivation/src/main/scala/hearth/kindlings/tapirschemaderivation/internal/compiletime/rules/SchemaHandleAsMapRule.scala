package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import hearth.kindlings.tapirschemaderivation.internal.runtime.TapirSchemaUtils
import sttp.tapir.Schema

trait SchemaHandleAsMapRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaHandleAsMapRule extends SchemaDerivationRule("handle as map when possible") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapSchema[A, Pair](isMap.value)
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapSchema[A: SchemaCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Schema[A]]]] = {
      import isMap.{Key, Value}
      implicit val stringT: Type[String] = TsTypes.StringType
      if (!(Key <:< Type[String]))
        MIO.fail(
          new Exception(
            s"Cannot derive tapir Schema for Map with non-String key type ${Key.prettyPrint}"
          )
        )
      else {
        val mapsAreArraysExpr: Expr[Boolean] = sctx.jsonCfg.mapsAreArrays
        Log.info(s"Deriving Schema for map value: ${Type[Value].prettyPrint}") >>
          deriveSchemaRecursively[Value](using sctx.nest[Value]).map { valueSchema =>
            Rule.matched(Expr.quote {
              if (Expr.splice(mapsAreArraysExpr))
                TapirSchemaUtils.mapAsArraySchema[Value](Expr.splice(valueSchema)).asInstanceOf[Schema[A]]
              else
                TapirSchemaUtils.mapSchema[Value](Expr.splice(valueSchema)).asInstanceOf[Schema[A]]
            })
          }
      }
    }
  }
}
