package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema

trait SchemaUseBuiltInSupportRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  /** Built-in (leaf) types, delegating to the same instances that tapir provides in the `Schema` companion.
    *
    * Nested occurrences of these types are usually resolved by the implicit rule already, but the implicit search is
    * skipped for the type being derived (to avoid `implicit val s: Schema[A] = KindlingsSchema.derived[A].schema`
    * summoning itself), so without this rule e.g. `KindlingsSchema.derived[Int]` would fail, and
    * `KindlingsSchema.derived[String]` would be handled as a collection of `Char`s. It also adds `Char` (encoded as a
    * String by the JSON libraries), for which tapir has no built-in Schema.
    */
  object SchemaUseBuiltInSupportRule extends SchemaDerivationRule("use built-in support for primitives") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> {
        builtInSchema[A] match {
          case Some(schemaExpr) =>
            Log.info(s"Found built-in schema for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(schemaExpr))
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def builtInSchema[A: SchemaCtx]: Option[Expr[Schema[A]]] = {
      implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
      val tpe = Type[A]
      // format: off
      if (tpe =:= Type.of[String]) Some(Expr.quote(Schema.schemaForString.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[Char]) Some(Expr.quote(Schema.string[A]))
      else if (tpe =:= Type.of[Byte]) Some(Expr.quote(Schema.schemaForByte.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[Short]) Some(Expr.quote(Schema.schemaForShort.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[Int]) Some(Expr.quote(Schema.schemaForInt.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[Long]) Some(Expr.quote(Schema.schemaForLong.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[Float]) Some(Expr.quote(Schema.schemaForFloat.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[Double]) Some(Expr.quote(Schema.schemaForDouble.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[Boolean]) Some(Expr.quote(Schema.schemaForBoolean.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[Unit]) Some(Expr.quote(Schema.schemaForUnit.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[BigDecimal]) Some(Expr.quote(Schema.schemaForBigDecimal.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[BigInt]) Some(Expr.quote(Schema.schemaForBigInt.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.math.BigDecimal]) Some(Expr.quote(Schema.schemaForJBigDecimal.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.math.BigInteger]) Some(Expr.quote(Schema.schemaForJBigInteger.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.util.UUID]) Some(Expr.quote(Schema.schemaForUUID.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[Array[Byte]]) Some(Expr.quote(Schema.schemaForByteArray.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.nio.ByteBuffer]) Some(Expr.quote(Schema.schemaForByteBuffer.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.Instant]) Some(Expr.quote(Schema.schemaForInstant.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.ZonedDateTime]) Some(Expr.quote(Schema.schemaForZonedDateTime.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.OffsetDateTime]) Some(Expr.quote(Schema.schemaForOffsetDateTime.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.util.Date]) Some(Expr.quote(Schema.schemaForDate.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.LocalDateTime]) Some(Expr.quote(Schema.schemaForLocalDateTime.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.LocalDate]) Some(Expr.quote(Schema.schemaForLocalDate.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.LocalTime]) Some(Expr.quote(Schema.schemaForLocalTime.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.OffsetTime]) Some(Expr.quote(Schema.schemaForOffsetTime.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.ZoneOffset]) Some(Expr.quote(Schema.schemaForZoneOffset.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.ZoneId]) Some(Expr.quote(Schema.schemaForZoneId.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.Duration]) Some(Expr.quote(Schema.schemaForJavaDuration.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[java.time.Period]) Some(Expr.quote(Schema.schemaForPeriod.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[scala.concurrent.duration.Duration]) Some(Expr.quote(Schema.schemaForScalaDuration.asInstanceOf[Schema[A]]))
      else if (tpe =:= Type.of[sttp.model.Uri]) Some(Expr.quote(Schema.schemaForUri.asInstanceOf[Schema[A]]))
      else None
      // format: on
    }
  }
}
