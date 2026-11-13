package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils

trait EncoderHandleAsBuiltInRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsBuiltInRule extends EncoderDerivationRule("handle as built-in type") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a built-in type") >> {
        // `T | Null` (Scala 3) is `=:=` to a reference `T`: handled by the Option rule (null or a `T`)
        if (nullableUnionUnderlying[A].isDefined)
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is a nullable union type"))
        else {
          val writer = ectx.writer
          val value = ectx.value

          // Numbers and booleans are written as JSON strings with `@stringified` / `isStringified` (as in
          // jsoniter-scala), decided at compile time when possible.
          def stringifiable(plain: Expr[Unit], asString: Expr[Unit]): Some[Expr[Unit]] =
            Some(ectx.stringifiedMode match {
              case Some(true)  => asString
              case Some(false) => plain
              case None        =>
                Expr.quote(if (Expr.splice(ectx.config).isStringified) Expr.splice(asString) else Expr.splice(plain))
            })

          val result: Option[Expr[Unit]] =
            if (Type[A] =:= Type.of[Int])
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Int])),
                Expr.quote(Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Int]))
              )
            else if (Type[A] =:= Type.of[Long])
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Long])),
                Expr.quote(Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Long]))
              )
            else if (Type[A] =:= Type.of[Double])
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Double])),
                Expr.quote(Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Double]))
              )
            else if (Type[A] =:= Type.of[Float])
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Float])),
                Expr.quote(Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Float]))
              )
            else if (Type[A] =:= Type.of[Boolean])
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Boolean])),
                Expr.quote(Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Boolean]))
              )
            else if (Type[A] =:= Type.of[String])
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[String])))
            else if (Type[A] =:= Type.of[Byte])
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Byte])),
                Expr.quote(Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Byte]))
              )
            else if (Type[A] =:= Type.of[Short])
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Short])),
                Expr.quote(Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[Short]))
              )
            else if (Type[A] =:= Type.of[Char])
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Char].toString)))
            else if (Type[A] =:= Type.of[BigDecimal])
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[BigDecimal])),
                Expr.quote(Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[BigDecimal]))
              )
            else if (Type[A] =:= Type.of[BigInt])
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[BigInt])),
                Expr.quote(Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[BigInt]))
              )
            else if (Type[A] =:= CTypes.Instant)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.Instant])))
            else if (Type[A] =:= CTypes.LocalDate)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.LocalDate])))
            else if (Type[A] =:= CTypes.LocalTime)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.LocalTime])))
            else if (Type[A] =:= CTypes.LocalDateTime)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.LocalDateTime])))
            else if (Type[A] =:= CTypes.OffsetDateTime)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.OffsetDateTime])))
            else if (Type[A] =:= CTypes.ZonedDateTime)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.ZonedDateTime])))
            else if (Type[A] =:= CTypes.Duration)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.Duration])))
            else if (Type[A] =:= CTypes.MonthDay)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.MonthDay])))
            else if (Type[A] =:= CTypes.OffsetTime)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.OffsetTime])))
            else if (Type[A] =:= CTypes.Period)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.Period])))
            else if (Type[A] =:= CTypes.Year)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.Year])))
            else if (Type[A] =:= CTypes.YearMonth)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.YearMonth])))
            else if (Type[A] =:= CTypes.ZoneId)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.ZoneId])))
            else if (Type[A] =:= CTypes.ZoneOffset)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.ZoneOffset])))
            else if (Type[A] =:= CTypes.UUID)
              Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.util.UUID])))
            else if (Type[A] =:= Type.of[Unit])
              Some(Expr.quote {
                Expr.splice(writer).writeObjectStart()
                Expr.splice(writer).writeObjectEnd()
              })
            // Java boxed primitives — unbox via *Value() methods, then write as primitive
            else if (Type[A] =:= CTypes.JavaByte)
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.lang.Byte].byteValue())),
                Expr.quote(
                  Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[java.lang.Byte].byteValue())
                )
              )
            else if (Type[A] =:= CTypes.JavaShort)
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.lang.Short].shortValue())),
                Expr.quote(
                  Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[java.lang.Short].shortValue())
                )
              )
            else if (Type[A] =:= CTypes.JavaInteger)
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.lang.Integer].intValue())),
                Expr.quote(
                  Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[java.lang.Integer].intValue())
                )
              )
            else if (Type[A] =:= CTypes.JavaLong)
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.lang.Long].longValue())),
                Expr.quote(
                  Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[java.lang.Long].longValue())
                )
              )
            else if (Type[A] =:= CTypes.JavaFloat)
              stringifiable(
                Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.lang.Float].floatValue())),
                Expr.quote(
                  Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[java.lang.Float].floatValue())
                )
              )
            else if (Type[A] =:= CTypes.JavaDouble)
              stringifiable(
                Expr.quote(
                  Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.lang.Double].doubleValue())
                ),
                Expr.quote(
                  Expr.splice(writer).writeValAsString(Expr.splice(value).asInstanceOf[java.lang.Double].doubleValue())
                )
              )
            else if (Type[A] =:= CTypes.JavaBoolean)
              stringifiable(
                Expr.quote(
                  Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.lang.Boolean].booleanValue())
                ),
                Expr.quote(
                  Expr
                    .splice(writer)
                    .writeValAsString(Expr.splice(value).asInstanceOf[java.lang.Boolean].booleanValue())
                )
              )
            else if (Type[A] =:= CTypes.JavaCharacter)
              Some(
                Expr.quote(
                  Expr
                    .splice(writer)
                    .writeVal(Expr.splice(value).asInstanceOf[java.lang.Character].charValue().toString)
                )
              )
            // BitSet types — encode as JSON array of ints
            else if (Type[A] =:= CTypes.ImmutableBitSet)
              Some(
                Expr.quote(
                  JsoniterDerivationUtils.writeBitSet(
                    Expr.splice(writer),
                    Expr.splice(value).asInstanceOf[scala.collection.BitSet],
                    Expr.splice(ectx.stringifiedExpr)
                  )
                )
              )
            else if (Type[A] =:= CTypes.MutableBitSet)
              Some(
                Expr.quote(
                  JsoniterDerivationUtils.writeBitSet(
                    Expr.splice(writer),
                    Expr.splice(value).asInstanceOf[scala.collection.BitSet],
                    Expr.splice(ectx.stringifiedExpr)
                  )
                )
              )
            else if (Type[A] =:= CTypes.CollectionBitSet)
              Some(
                Expr.quote(
                  JsoniterDerivationUtils.writeBitSet(
                    Expr.splice(writer),
                    Expr.splice(value).asInstanceOf[scala.collection.BitSet],
                    Expr.splice(ectx.stringifiedExpr)
                  )
                )
              )
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
