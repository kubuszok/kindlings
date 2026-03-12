package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, DecimalConfig}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroSchemaForUseBuiltInSupportRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForUseBuiltInSupportRule extends SchemaDerivationRule("use built-in support for primitives") {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
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
    private def builtInSchema[A: SchemaForCtx]: Option[Expr[Schema]] = {
      implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig
      implicit val DecimalConfigT: Type[DecimalConfig] = SfTypes.DecimalConfig
      val tpe = Type[A]
      if (tpe =:= Type.of[Boolean])
        Some(Expr.quote(AvroDerivationUtils.booleanSchema))
      else if (tpe =:= Type.of[Byte])
        Some(Expr.quote(AvroDerivationUtils.intSchema))
      else if (tpe =:= Type.of[Short])
        Some(Expr.quote(AvroDerivationUtils.intSchema))
      else if (tpe =:= Type.of[Int])
        Some(Expr.quote(AvroDerivationUtils.intSchema))
      else if (tpe =:= Type.of[Long])
        Some(Expr.quote(AvroDerivationUtils.longSchema))
      else if (tpe =:= Type.of[Float])
        Some(Expr.quote(AvroDerivationUtils.floatSchema))
      else if (tpe =:= Type.of[Double])
        Some(Expr.quote(AvroDerivationUtils.doubleSchema))
      else if (tpe =:= Type.of[Char])
        Some(Expr.quote(AvroDerivationUtils.stringSchema))
      else if (tpe =:= Type.of[String])
        Some(Expr.quote(AvroDerivationUtils.stringSchema))
      else if (tpe =:= Type.of[Array[Byte]])
        Some(Expr.quote(AvroDerivationUtils.bytesSchema))
      else if (tpe =:= Type.of[java.nio.ByteBuffer])
        Some(Expr.quote(AvroDerivationUtils.bytesSchema))
      else if (tpe =:= Type.of[BigDecimal])
        Some(Expr.quote {
          Expr.splice(sfctx.config).decimalConfig match {
            case Some(dc) => AvroDerivationUtils.decimalSchema(dc.precision, dc.scale)
            case None     => AvroDerivationUtils.stringSchema
          }
        })
      else if (tpe =:= Type.of[java.util.UUID])
        Some(Expr.quote(AvroDerivationUtils.uuidSchema))
      else if (tpe =:= Type.of[java.time.Instant])
        Some(Expr.quote(AvroDerivationUtils.timestampMillisSchema))
      else if (tpe =:= Type.of[java.time.LocalDate])
        Some(Expr.quote(AvroDerivationUtils.dateSchema))
      else if (tpe =:= Type.of[java.time.LocalTime])
        Some(Expr.quote(AvroDerivationUtils.timeMicrosSchema))
      else if (tpe =:= Type.of[java.time.LocalDateTime])
        Some(Expr.quote(AvroDerivationUtils.timestampMillisSchema))
      else
        None
    }
  }
}
