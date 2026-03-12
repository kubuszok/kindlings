package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, DecimalConfig}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils

trait AvroDecoderUseBuiltInSupportRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderUseBuiltInSupportRule extends DecoderDerivationRule("use built-in support for primitives") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use built-in decoder for ${Type[A].prettyPrint}") >> {
        builtInDecode[A] match {
          case Some(decodeExpr) =>
            Log.info(s"Found built-in decoder for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(decodeExpr))
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def builtInDecode[A: DecoderCtx]: Option[Expr[A]] = {
      implicit val AvroConfigT: Type[AvroConfig] = DecTypes.AvroConfig
      implicit val DecimalConfigT: Type[DecimalConfig] = DecTypes.DecimalConfig
      val tpe = Type[A]
      val value = dctx.avroValue
      if (tpe =:= Type.of[Boolean])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Boolean].asInstanceOf[A]))
      else if (tpe =:= Type.of[Int])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Int].asInstanceOf[A]))
      else if (tpe =:= Type.of[Long])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Long].asInstanceOf[A]))
      else if (tpe =:= Type.of[Float])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Float].asInstanceOf[A]))
      else if (tpe =:= Type.of[Double])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Double].asInstanceOf[A]))
      else if (tpe =:= Type.of[String])
        Some(Expr.quote(AvroDerivationUtils.decodeCharSequence(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[Byte])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Int].toByte.asInstanceOf[A]))
      else if (tpe =:= Type.of[Short])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Int].toShort.asInstanceOf[A]))
      else if (tpe =:= Type.of[Char])
        Some(Expr.quote(AvroDerivationUtils.decodeCharSequence(Expr.splice(value)).charAt(0).asInstanceOf[A]))
      else if (tpe =:= Type.of[Array[Byte]])
        Some(Expr.quote(AvroDerivationUtils.decodeByteBuffer(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[java.nio.ByteBuffer])
        Some(Expr.quote(AvroDerivationUtils.decodeToByteBuffer(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[BigDecimal])
        Some(Expr.quote {
          (Expr.splice(dctx.config).decimalConfig match {
            case Some(dc) => AvroDerivationUtils.decodeBigDecimal(Expr.splice(value), dc.scale)
            case None     => BigDecimal(AvroDerivationUtils.decodeCharSequence(Expr.splice(value)))
          }).asInstanceOf[A]
        })
      else if (tpe =:= Type.of[java.util.UUID])
        Some(Expr.quote(AvroDerivationUtils.decodeUUID(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[java.time.Instant])
        Some(Expr.quote(AvroDerivationUtils.decodeInstant(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[java.time.LocalDate])
        Some(Expr.quote(AvroDerivationUtils.decodeLocalDate(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[java.time.LocalTime])
        Some(Expr.quote(AvroDerivationUtils.decodeLocalTime(Expr.splice(value)).asInstanceOf[A]))
      else if (tpe =:= Type.of[java.time.LocalDateTime])
        Some(Expr.quote(AvroDerivationUtils.decodeLocalDateTime(Expr.splice(value)).asInstanceOf[A]))
      else
        None
    }
  }
}
