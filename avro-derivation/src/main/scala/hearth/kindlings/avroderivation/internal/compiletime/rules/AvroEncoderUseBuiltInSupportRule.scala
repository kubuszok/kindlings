package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.{AvroConfig, DecimalConfig}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils

trait AvroEncoderUseBuiltInSupportRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderUseBuiltInSupportRule extends EncoderDerivationRule("use built-in support for primitives") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to use built-in encoder for ${Type[A].prettyPrint}") >> {
        builtInEncode[A] match {
          case Some(encodeExpr) =>
            Log.info(s"Found built-in encoder for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(encodeExpr))
          case None =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def builtInEncode[A: EncoderCtx]: Option[Expr[Any]] = {
      implicit val AvroConfigT: Type[AvroConfig] = EncTypes.AvroConfig
      implicit val DecimalConfigT: Type[DecimalConfig] = EncTypes.DecimalConfig
      val tpe = Type[A]
      val value = ectx.value
      if (tpe =:= Type.of[Boolean])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[Int])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[Long])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[Float])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[Double])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[String])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Any]))
      else if (tpe =:= Type.of[Byte])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Byte].toInt: Any))
      else if (tpe =:= Type.of[Short])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Short].toInt: Any))
      else if (tpe =:= Type.of[Char])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[Char].toString: Any))
      else if (tpe =:= Type.of[Array[Byte]])
        Some(Expr.quote(AvroDerivationUtils.wrapByteArray(Expr.splice(value).asInstanceOf[Array[Byte]]): Any))
      else if (tpe =:= Type.of[java.nio.ByteBuffer])
        Some(Expr.quote(Expr.splice(value).asInstanceOf[java.nio.ByteBuffer]: Any))
      else if (tpe =:= Type.of[BigDecimal])
        Some(Expr.quote {
          val bd = Expr.splice(value).asInstanceOf[BigDecimal]
          (Expr.splice(ectx.config).decimalConfig match {
            case Some(dc) => AvroDerivationUtils.encodeBigDecimal(bd, dc.scale)
            case None     => bd.toString
          }): Any
        })
      else if (tpe =:= Type.of[java.util.UUID])
        Some(Expr.quote(AvroDerivationUtils.encodeUUID(Expr.splice(value).asInstanceOf[java.util.UUID]): Any))
      else if (tpe =:= Type.of[java.time.Instant])
        Some(Expr.quote(AvroDerivationUtils.encodeInstant(Expr.splice(value).asInstanceOf[java.time.Instant]): Any))
      else if (tpe =:= Type.of[java.time.LocalDate])
        Some(Expr.quote(AvroDerivationUtils.encodeLocalDate(Expr.splice(value).asInstanceOf[java.time.LocalDate]): Any))
      else if (tpe =:= Type.of[java.time.LocalTime])
        Some(Expr.quote(AvroDerivationUtils.encodeLocalTime(Expr.splice(value).asInstanceOf[java.time.LocalTime]): Any))
      else if (tpe =:= Type.of[java.time.LocalDateTime])
        Some(
          Expr.quote(
            AvroDerivationUtils.encodeLocalDateTime(Expr.splice(value).asInstanceOf[java.time.LocalDateTime]): Any
          )
        )
      else
        None
    }
  }
}
