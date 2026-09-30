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
      val mapsAreArraysExpr: Expr[Boolean] = sctx.jsonCfg.mapsAreArrays
      if (Key <:< Type[String]) {
        Log.info(s"Deriving Schema for map value: ${Type[Value].prettyPrint}") >>
          deriveSchemaRecursively[Value](using sctx.nest[Value]).map { valueSchema =>
            Rule.matched(Expr.quote {
              if (Expr.splice(mapsAreArraysExpr))
                TapirSchemaUtils.mapAsArraySchema[Value](Expr.splice(valueSchema)).asInstanceOf[Schema[A]]
              else
                TapirSchemaUtils.mapSchema[Value](Expr.splice(valueSchema)).asInstanceOf[Schema[A]]
            })
          }
      } else
        keyToString[Key] match {
          case Some(keyToStringExpr) =>
            Log.info(s"Deriving Schema for map value: ${Type[Value].prettyPrint} (key: ${Key.prettyPrint})") >>
              deriveSchemaRecursively[Value](using sctx.nest[Value]).map { valueSchema =>
                Rule.matched(Expr.quote {
                  if (Expr.splice(mapsAreArraysExpr))
                    TapirSchemaUtils.mapAsArraySchema[Value](Expr.splice(valueSchema)).asInstanceOf[Schema[A]]
                  else
                    TapirSchemaUtils
                      .mapSchemaWithKeys[Key, Value](Expr.splice(valueSchema), Expr.splice(keyToStringExpr))
                      .asInstanceOf[Schema[A]]
                })
              }
          case None =>
            MIO.fail(
              new Exception(
                s"Cannot derive tapir Schema for Map with key type ${Key.prettyPrint}: the key must be a String " +
                  s"(sub)type or a value type wrapping one. Provide an implicit Schema[${Type[A].prettyPrint}] " +
                  s"(e.g. using Schema.schemaForMap[K, V](keyToString)) for other key types"
              )
            )
        }
    }

    /** Builds a `K => String` for map keys which are value types (AnyVal, opaque types, newtypes, ...) wrapping a
      * String (sub)type, possibly through several layers of value types.
      */
    private def keyToString[K: Type](implicit stringT: Type[String]): Option[Expr[K => String]] =
      if (Type[K] <:< Type[String]) Some(Expr.quote((k: K) => k.asInstanceOf[String]))
      else
        Type[K] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            keyToString[Inner].map(innerToString => unwrapKeyToString[K, Inner](isValueType.value, innerToString))
          case _ => None
        }

    /** Extracted as a helper because the unwrap closure captures path-dependent state from the [[IsValueType]]
      * instance.
      */
    private def unwrapKeyToString[K: Type, Inner: Type](
        isValueType: IsValueTypeOf[K, Inner],
        innerToString: Expr[Inner => String]
    ): Expr[K => String] =
      Expr.quote { (k: K) =>
        Expr.splice(innerToString).apply(Expr.splice(isValueType.unwrap(Expr.quote(k))))
      }
  }
}
