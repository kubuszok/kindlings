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
        // `T | Null` (Scala 3) is `=:=` to a reference `T`: handled by the Option rule (null or a `T`)
        if (nullableUnionUnderlying[A].isDefined)
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is a nullable union type"))
        else {
          val reader = dctx.reader

          // Numbers and booleans are read from JSON strings with `@stringified` / `isStringified` (as in jsoniter-scala),
          // decided at compile time when possible.
          def stringifiable(plain: Expr[A], fromString: Expr[A]): Some[Expr[A]] = Some(dctx.stringifiedMode match {
            case Some(true)  => fromString
            case Some(false) => plain
            case None        =>
              Expr.quote(if (Expr.splice(dctx.config).isStringified) Expr.splice(fromString) else Expr.splice(plain))
          })

          val result: Option[Expr[A]] =
            if (Type[A] =:= Type.of[Int])
              stringifiable(
                Expr.quote(Expr.splice(reader).readInt().asInstanceOf[A]),
                Expr.quote(Expr.splice(reader).readStringAsInt().asInstanceOf[A])
              )
            else if (Type[A] =:= Type.of[Long])
              stringifiable(
                Expr.quote(Expr.splice(reader).readLong().asInstanceOf[A]),
                Expr.quote(Expr.splice(reader).readStringAsLong().asInstanceOf[A])
              )
            else if (Type[A] =:= Type.of[Double])
              stringifiable(
                Expr.quote(Expr.splice(reader).readDouble().asInstanceOf[A]),
                Expr.quote(Expr.splice(reader).readStringAsDouble().asInstanceOf[A])
              )
            else if (Type[A] =:= Type.of[Float])
              stringifiable(
                Expr.quote(Expr.splice(reader).readFloat().asInstanceOf[A]),
                Expr.quote(Expr.splice(reader).readStringAsFloat().asInstanceOf[A])
              )
            else if (Type[A] =:= Type.of[Boolean])
              stringifiable(
                Expr.quote(Expr.splice(reader).readBoolean().asInstanceOf[A]),
                Expr.quote(Expr.splice(reader).readStringAsBoolean().asInstanceOf[A])
              )
            else if (Type[A] =:= Type.of[String])
              Some(Expr.quote(Expr.splice(reader).readString(null).asInstanceOf[A]))
            else if (Type[A] =:= Type.of[Byte])
              stringifiable(
                Expr.quote(Expr.splice(reader).readByte().asInstanceOf[A]),
                Expr.quote(Expr.splice(reader).readStringAsByte().asInstanceOf[A])
              )
            else if (Type[A] =:= Type.of[Short])
              stringifiable(
                Expr.quote(Expr.splice(reader).readShort().asInstanceOf[A]),
                Expr.quote(Expr.splice(reader).readStringAsShort().asInstanceOf[A])
              )
            else if (Type[A] =:= Type.of[Char])
              Some(Expr.quote(Expr.splice(reader).readChar().asInstanceOf[A]))
            else if (Type[A] =:= Type.of[BigDecimal])
              // As in jsoniter-scala: rounded to `bigDecimalPrecision`, scale and digits limited by the reader.
              Some(dctx.evaluatedConfig match {
                case Some(c) =>
                  val mc = Expr.quote(JsoniterDerivationUtils.mathContext(Expr.splice(Expr(c.bigDecimalPrecision))))
                  stringifiable(
                    Expr.quote {
                      Expr
                        .splice(reader)
                        .readBigDecimal(
                          null,
                          Expr.splice(mc),
                          Expr.splice(Expr(c.bigDecimalScaleLimit)),
                          Expr.splice(Expr(c.bigDecimalDigitsLimit))
                        )
                        .asInstanceOf[A]
                    },
                    Expr.quote {
                      Expr
                        .splice(reader)
                        .readStringAsBigDecimal(
                          null,
                          Expr.splice(mc),
                          Expr.splice(Expr(c.bigDecimalScaleLimit)),
                          Expr.splice(Expr(c.bigDecimalDigitsLimit))
                        )
                        .asInstanceOf[A]
                    }
                  ).value
                case None =>
                  val config = dctx.config
                  stringifiable(
                    Expr.quote {
                      val c = Expr.splice(config)
                      Expr
                        .splice(reader)
                        .readBigDecimal(
                          null,
                          JsoniterDerivationUtils.mathContext(c.bigDecimalPrecision),
                          c.bigDecimalScaleLimit,
                          c.bigDecimalDigitsLimit
                        )
                        .asInstanceOf[A]
                    },
                    Expr.quote {
                      val c = Expr.splice(config)
                      Expr
                        .splice(reader)
                        .readStringAsBigDecimal(
                          null,
                          JsoniterDerivationUtils.mathContext(c.bigDecimalPrecision),
                          c.bigDecimalScaleLimit,
                          c.bigDecimalDigitsLimit
                        )
                        .asInstanceOf[A]
                    }
                  ).value
              })
            else if (Type[A] =:= Type.of[BigInt])
              stringifiable(
                Expr.quote {
                  Expr.splice(reader).readBigInt(null, Expr.splice(dctx.config).bigIntDigitsLimit).asInstanceOf[A]
                },
                Expr.quote {
                  Expr
                    .splice(reader)
                    .readStringAsBigInt(null, Expr.splice(dctx.config).bigIntDigitsLimit)
                    .asInstanceOf[A]
                }
              )
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
                JsoniterDerivationUtils.readUnit(Expr.splice(reader))
                ().asInstanceOf[A]
              })
            // Java boxed primitives — decode as the underlying primitive, then box
            else if (Type[A] =:= CTypes.JavaByte)
              stringifiable(
                Expr.quote(java.lang.Byte.valueOf(Expr.splice(reader).readByte()).asInstanceOf[A]),
                Expr.quote(java.lang.Byte.valueOf(Expr.splice(reader).readStringAsByte()).asInstanceOf[A])
              )
            else if (Type[A] =:= CTypes.JavaShort)
              stringifiable(
                Expr.quote(java.lang.Short.valueOf(Expr.splice(reader).readShort()).asInstanceOf[A]),
                Expr.quote(java.lang.Short.valueOf(Expr.splice(reader).readStringAsShort()).asInstanceOf[A])
              )
            else if (Type[A] =:= CTypes.JavaInteger)
              stringifiable(
                Expr.quote(java.lang.Integer.valueOf(Expr.splice(reader).readInt()).asInstanceOf[A]),
                Expr.quote(java.lang.Integer.valueOf(Expr.splice(reader).readStringAsInt()).asInstanceOf[A])
              )
            else if (Type[A] =:= CTypes.JavaLong)
              stringifiable(
                Expr.quote(java.lang.Long.valueOf(Expr.splice(reader).readLong()).asInstanceOf[A]),
                Expr.quote(java.lang.Long.valueOf(Expr.splice(reader).readStringAsLong()).asInstanceOf[A])
              )
            else if (Type[A] =:= CTypes.JavaFloat)
              stringifiable(
                Expr.quote(java.lang.Float.valueOf(Expr.splice(reader).readFloat()).asInstanceOf[A]),
                Expr.quote(java.lang.Float.valueOf(Expr.splice(reader).readStringAsFloat()).asInstanceOf[A])
              )
            else if (Type[A] =:= CTypes.JavaDouble)
              stringifiable(
                Expr.quote(java.lang.Double.valueOf(Expr.splice(reader).readDouble()).asInstanceOf[A]),
                Expr.quote(java.lang.Double.valueOf(Expr.splice(reader).readStringAsDouble()).asInstanceOf[A])
              )
            else if (Type[A] =:= CTypes.JavaBoolean)
              stringifiable(
                Expr.quote(java.lang.Boolean.valueOf(Expr.splice(reader).readBoolean()).asInstanceOf[A]),
                Expr.quote(java.lang.Boolean.valueOf(Expr.splice(reader).readStringAsBoolean()).asInstanceOf[A])
              )
            else if (Type[A] =:= CTypes.JavaCharacter)
              Some(Expr.quote(java.lang.Character.valueOf(Expr.splice(reader).readChar()).asInstanceOf[A]))
            // BitSet types — decode from JSON array of ints
            else if (Type[A] =:= CTypes.ImmutableBitSet)
              Some(Expr.quote {
                JsoniterDerivationUtils
                  .readImmutableBitSet(
                    Expr.splice(reader),
                    Expr.splice(dctx.config).bitSetValueLimit,
                    Expr.splice(dctx.stringifiedExpr)
                  )
                  .asInstanceOf[A]
              })
            else if (Type[A] =:= CTypes.MutableBitSet)
              Some(Expr.quote {
                JsoniterDerivationUtils
                  .readMutableBitSet(
                    Expr.splice(reader),
                    Expr.splice(dctx.config).bitSetValueLimit,
                    Expr.splice(dctx.stringifiedExpr)
                  )
                  .asInstanceOf[A]
              })
            else if (Type[A] =:= CTypes.CollectionBitSet)
              Some(Expr.quote {
                JsoniterDerivationUtils
                  .readImmutableBitSet(
                    Expr.splice(reader),
                    Expr.splice(dctx.config).bitSetValueLimit,
                    Expr.splice(dctx.stringifiedExpr)
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

}
