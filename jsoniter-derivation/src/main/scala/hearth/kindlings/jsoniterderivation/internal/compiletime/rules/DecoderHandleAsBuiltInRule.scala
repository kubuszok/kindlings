package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils

trait DecoderHandleAsBuiltInRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsBuiltInRule extends DecoderDerivationRule("handle as built-in type") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a built-in type") >> {
        val reader = dctx.reader

        val result: Option[Expr[A]] =
          if (Type[A] =:= Type.of[Int])
            Some(Expr.quote(Expr.splice(reader).readInt().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Long])
            Some(Expr.quote(Expr.splice(reader).readLong().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Double])
            Some(Expr.quote(Expr.splice(reader).readDouble().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Float])
            Some(Expr.quote(Expr.splice(reader).readFloat().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Boolean])
            Some(Expr.quote(Expr.splice(reader).readBoolean().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[String])
            Some(Expr.quote(Expr.splice(reader).readString(null).asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Byte])
            Some(Expr.quote(Expr.splice(reader).readByte().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Short])
            Some(Expr.quote(Expr.splice(reader).readShort().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Char])
            Some(Expr.quote(Expr.splice(reader).readChar().asInstanceOf[A]))
          else if (Type[A] =:= Type.of[BigDecimal])
            Some(Expr.quote {
              JsoniterDerivationUtils
                .validateBigDecimal(
                  Expr.splice(reader),
                  Expr.splice(reader).readBigDecimal(null),
                  Expr.splice(dctx.config).bigDecimalPrecision,
                  Expr.splice(dctx.config).bigDecimalScaleLimit,
                  Expr.splice(dctx.config).bigDecimalDigitsLimit
                )
                .asInstanceOf[A]
            })
          else if (Type[A] =:= Type.of[BigInt])
            Some(Expr.quote {
              JsoniterDerivationUtils
                .validateBigInt(
                  Expr.splice(reader),
                  Expr.splice(reader).readBigInt(null),
                  Expr.splice(dctx.config).bigDecimalDigitsLimit
                )
                .asInstanceOf[A]
            })
          else if (Type[A] =:= CTypes.Instant)
            Some(Expr.quote(Expr.splice(reader).readInstant(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.LocalDate)
            Some(Expr.quote(Expr.splice(reader).readLocalDate(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.LocalTime)
            Some(Expr.quote(Expr.splice(reader).readLocalTime(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.LocalDateTime)
            Some(Expr.quote(Expr.splice(reader).readLocalDateTime(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.OffsetDateTime)
            Some(Expr.quote(Expr.splice(reader).readOffsetDateTime(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.ZonedDateTime)
            Some(Expr.quote(Expr.splice(reader).readZonedDateTime(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.Duration)
            Some(Expr.quote(Expr.splice(reader).readDuration(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.MonthDay)
            Some(Expr.quote(Expr.splice(reader).readMonthDay(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.OffsetTime)
            Some(Expr.quote(Expr.splice(reader).readOffsetTime(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.Period)
            Some(Expr.quote(Expr.splice(reader).readPeriod(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.Year)
            Some(Expr.quote(Expr.splice(reader).readYear(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.YearMonth)
            Some(Expr.quote(Expr.splice(reader).readYearMonth(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.ZoneId)
            Some(Expr.quote(Expr.splice(reader).readZoneId(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.ZoneOffset)
            Some(Expr.quote(Expr.splice(reader).readZoneOffset(null).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.UUID)
            Some(Expr.quote(Expr.splice(reader).readUUID(null).asInstanceOf[A]))
          else if (Type[A] =:= Type.of[Unit])
            Some(Expr.quote {
              Expr.splice(reader).skip()
              ().asInstanceOf[A]
            })
          // Java boxed primitives — decode as the underlying primitive, then box
          else if (Type[A] =:= CTypes.JavaByte)
            Some(Expr.quote(java.lang.Byte.valueOf(Expr.splice(reader).readByte()).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.JavaShort)
            Some(Expr.quote(java.lang.Short.valueOf(Expr.splice(reader).readShort()).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.JavaInteger)
            Some(Expr.quote(java.lang.Integer.valueOf(Expr.splice(reader).readInt()).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.JavaLong)
            Some(Expr.quote(java.lang.Long.valueOf(Expr.splice(reader).readLong()).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.JavaFloat)
            Some(Expr.quote(java.lang.Float.valueOf(Expr.splice(reader).readFloat()).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.JavaDouble)
            Some(Expr.quote(java.lang.Double.valueOf(Expr.splice(reader).readDouble()).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.JavaBoolean)
            Some(Expr.quote(java.lang.Boolean.valueOf(Expr.splice(reader).readBoolean()).asInstanceOf[A]))
          else if (Type[A] =:= CTypes.JavaCharacter)
            Some(Expr.quote(java.lang.Character.valueOf(Expr.splice(reader).readChar()).asInstanceOf[A]))
          // BitSet types — decode from JSON array of ints
          else if (Type[A] =:= CTypes.ImmutableBitSet)
            Some(Expr.quote {
              JsoniterDerivationUtils
                .readImmutableBitSet(
                  Expr.splice(reader),
                  Expr.splice(dctx.config).bitSetValueLimit
                )
                .asInstanceOf[A]
            })
          else if (Type[A] =:= CTypes.MutableBitSet)
            Some(Expr.quote {
              JsoniterDerivationUtils
                .readMutableBitSet(
                  Expr.splice(reader),
                  Expr.splice(dctx.config).bitSetValueLimit
                )
                .asInstanceOf[A]
            })
          else if (Type[A] =:= CTypes.CollectionBitSet)
            Some(Expr.quote {
              JsoniterDerivationUtils
                .readImmutableBitSet(
                  Expr.splice(reader),
                  Expr.splice(dctx.config).bitSetValueLimit
                )
                .asInstanceOf[A]
            })
          else
            None

        MIO.pure(result match {
          case Some(expr) => Rule.matched(expr)
          case None       => Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type")
        })
      }
  }

}
