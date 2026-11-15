package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.derivation.compiletime.ValueTypeIntegrationHints
import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import hearth.kindlings.tapirschemaderivation.internal.runtime.TapirSchemaUtils
import sttp.tapir.Schema
import hearth.kindlings.derivation.compiletime.StableChildrenOrder

trait SchemaHandleAsMapRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaHandleAsMapRule extends SchemaDerivationRule("handle as map when possible") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            // Non-empty maps (e.g. cats' NonEmptyMap): required, with minItems/minProperties: 1
            deriveMapSchema[A, Pair](isMap.value).map(schema =>
              Rule.matched(nonEmptyIfNeeded[A, Pair](isMap.value, schema))
            )
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapSchema[A: SchemaCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Expr[Schema[A]]] = {
      import isMap.{Key, Value}
      implicit val stringT: Type[String] = TsTypes.StringType
      implicit val sNameT: Type[sttp.tapir.Schema.SName] = TsTypes.SNameType
      val mapsAreArraysExpr: Expr[Boolean] = sctx.jsonCfg.mapsAreArrays
      // Named like tapir's `Schema.schemaForMap`: `Map` with the key (unless a String) and the value as parameters.
      val valueNameExpr = computeFullNameExpr[Value](sctx.derivedType)
      val mapNameExpr: Expr[sttp.tapir.Schema.SName] =
        if (Key <:< Type[String]) Expr.quote(TapirSchemaUtils.parseSName("Map[" + Expr.splice(valueNameExpr) + "]"))
        else {
          val keyNameExpr = computeFullNameExpr[Key](sctx.derivedType)
          Expr.quote(
            TapirSchemaUtils.parseSName("Map[" + Expr.splice(keyNameExpr) + "," + Expr.splice(valueNameExpr) + "]")
          )
        }
      if (Key <:< Type[String]) {
        Log.info(s"Deriving Schema for map value: ${Type[Value].prettyPrint}") >>
          deriveSchemaRecursively[Value](using sctx.nest[Value]).map { valueSchema =>
            Expr.quote {
              if (Expr.splice(mapsAreArraysExpr))
                TapirSchemaUtils.mapAsArraySchema[Value](Expr.splice(valueSchema)).asInstanceOf[Schema[A]]
              else
                TapirSchemaUtils
                  .mapSchema[Value](Expr.splice(valueSchema), Expr.splice(mapNameExpr))
                  .asInstanceOf[Schema[A]]
            }
          }
      } else
        keyToString[A, Key] match {
          case Some(keyToStringExpr) =>
            Log.info(s"Deriving Schema for map value: ${Type[Value].prettyPrint} (key: ${Key.prettyPrint})") >>
              deriveSchemaRecursively[Value](using sctx.nest[Value]).map { valueSchema =>
                Expr.quote {
                  if (Expr.splice(mapsAreArraysExpr))
                    TapirSchemaUtils.mapAsArraySchema[Value](Expr.splice(valueSchema)).asInstanceOf[Schema[A]]
                  else
                    TapirSchemaUtils
                      .mapSchemaWithKeys[Key, Value](
                        Expr.splice(valueSchema),
                        Expr.splice(keyToStringExpr),
                        Expr.splice(mapNameExpr)
                      )
                      .asInstanceOf[Schema[A]]
                }
              }
          case None =>
            MIO.fail(
              new Exception(
                s"Cannot derive tapir Schema for Map with key type ${Key.prettyPrint}: the key must be a String, a " +
                  s"primitive, UUID, BigInt, BigDecimal or java.time type, a family of case objects, a type with the " +
                  s"JSON library's key codec, or a value type (AnyVal, opaque type, ...) wrapping one of them" +
                  ValueTypeIntegrationHints.hint(SchemaHandleAsMapRuleImpl.this)(Key) + ". " +
                  s"Provide an implicit Schema[${Type[A].prettyPrint}] " +
                  s"(e.g. using Schema.schemaForMap[K, V](keyToString)) for other key types"
              )
            )
        }
    }

    /** Builds a `K => String` producing the JSON object key the JSON library writes for a map key:
      *
      *   - the library's own conversion, if it has one for `K` (e.g. circe's `KeyEncoder`, also a user-provided one),
      *   - `String` (sub)types as they are,
      *   - value types (AnyVal, opaque types, newtypes, ...), possibly through several layers, as their underlying
      *     value's key,
      *   - primitives (and their boxes), `UUID`, `BigInt`, `BigDecimal` and `java.time` values with `toString`, as the
      *     JSON libraries write them.
      */
    private def keyToString[A: SchemaCtx, K: Type](implicit stringT: Type[String]): Option[Expr[K => String]] =
      sctx[A].jsonCfg.mapKeyToString[K].orElse {
        if (Type[K] <:< Type[String]) Some(Expr.quote((k: K) => k.asInstanceOf[String]))
        else
          Type[K] match {
            case IsValueType(isValueType) =>
              import isValueType.Underlying as Inner
              keyToString[A, Inner]
                .map(innerToString => unwrapKeyToString[K, Inner](isValueType.value, innerToString))
            case _ if isScalarKey[K] => Some(Expr.quote((k: K) => k.toString))
            case _                   => enumKeyToString[A, K]
          }
      }

    /** A family of case objects (sealed trait, Scala 3 `enum`, Java enum) is written as the name of the case, the same
      * name the JSON library uses for the case as a value (e.g. circe's `transformConstructorNames`, jsoniter-scala's
      * `adtLeafClassNameMapper`).
      */
    private def enumKeyToString[A: SchemaCtx, K: Type](implicit stringT: Type[String]): Option[Expr[K => String]] =
      Enum.parse[K].toOption.flatMap { enumm =>
        val children: List[(String, ??<:[K])] =
          if (sctx[A].jsonCfg.flattensNestedSubtypes)
            StableChildrenOrder
              .exhaustiveChildren(SchemaHandleAsMapRuleImpl.this)(enumm)
              .getOrElse(StableChildrenOrder.directChildren(SchemaHandleAsMapRuleImpl.this)(enumm))
          else StableChildrenOrder.directChildren(SchemaHandleAsMapRuleImpl.this)(enumm)
        val cases: List[Option[(Expr[K], Expr[String])]] = children.map { case (childName, child) =>
          import child.Underlying as Child
          SingletonValue.parse[Child].toEither.toOption.map { sv =>
            (sv.singletonExpr.asInstanceOf[Expr[K]], sctx[A].jsonCfg.resolveSubtypeName[K, Child](childName))
          }
        }
        if (cases.isEmpty || cases.exists(_.isEmpty)) None
        else
          Some(Expr.quote { (k: K) =>
            Expr.splice {
              cases.flatten.foldRight(Expr.quote(k.toString): Expr[String]) { case ((singleton, name), elseExpr) =>
                Expr.quote(if (k == Expr.splice(singleton)) Expr.splice(name) else Expr.splice(elseExpr))
              }
            }
          })
      }

    /** Key types which the JSON libraries write as their `toString`. */
    private def isScalarKey[K: Type]: Boolean = scalarKeyTypes.exists(t => Type[K] <:< t.Underlying)

    private lazy val scalarKeyTypes: List[??] = List(
      Type.of[Int].as_??,
      Type.of[Long].as_??,
      Type.of[Short].as_??,
      Type.of[Byte].as_??,
      Type.of[Char].as_??,
      Type.of[Boolean].as_??,
      Type.of[Double].as_??,
      Type.of[Float].as_??,
      Type.of[java.lang.Integer].as_??,
      Type.of[java.lang.Long].as_??,
      Type.of[java.lang.Short].as_??,
      Type.of[java.lang.Byte].as_??,
      Type.of[java.lang.Character].as_??,
      Type.of[java.lang.Boolean].as_??,
      Type.of[java.lang.Double].as_??,
      Type.of[java.lang.Float].as_??,
      Type.of[BigInt].as_??,
      Type.of[BigDecimal].as_??,
      Type.of[java.math.BigInteger].as_??,
      Type.of[java.math.BigDecimal].as_??,
      Type.of[java.util.UUID].as_??,
      Type.of[java.time.Instant].as_??,
      Type.of[java.time.LocalDate].as_??,
      Type.of[java.time.LocalDateTime].as_??,
      Type.of[java.time.LocalTime].as_??,
      Type.of[java.time.OffsetDateTime].as_??,
      Type.of[java.time.OffsetTime].as_??,
      Type.of[java.time.ZonedDateTime].as_??,
      Type.of[java.time.Duration].as_??,
      Type.of[java.time.Period].as_??,
      Type.of[java.time.Year].as_??,
      Type.of[java.time.YearMonth].as_??,
      Type.of[java.time.MonthDay].as_??,
      Type.of[java.time.ZoneId].as_??,
      Type.of[java.time.ZoneOffset].as_??
    )

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
