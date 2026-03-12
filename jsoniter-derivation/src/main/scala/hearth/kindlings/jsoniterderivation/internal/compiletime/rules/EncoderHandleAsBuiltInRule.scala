package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EncoderHandleAsBuiltInRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsBuiltInRule extends EncoderDerivationRule("handle as built-in type") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a built-in type") >> {
        val writer = ectx.writer
        val value = ectx.value

        val result: Option[Expr[Unit]] =
          if (Type[A] =:= Type.of[Int])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Int])))
          else if (Type[A] =:= Type.of[Long])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Long])))
          else if (Type[A] =:= Type.of[Double])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Double])))
          else if (Type[A] =:= Type.of[Float])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Float])))
          else if (Type[A] =:= Type.of[Boolean])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Boolean])))
          else if (Type[A] =:= Type.of[String])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[String])))
          else if (Type[A] =:= Type.of[Byte])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Byte])))
          else if (Type[A] =:= Type.of[Short])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Short])))
          else if (Type[A] =:= Type.of[Char])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Char].toString)))
          else if (Type[A] =:= Type.of[BigDecimal])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[BigDecimal])))
          else if (Type[A] =:= Type.of[BigInt])
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[BigInt])))
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
          else if (Type[A] =:= CTypes.Period)
            Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[java.time.Period])))
          else
            None

        MIO.pure(result match {
          case Some(expr) => Rule.matched(expr)
          case None       => Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type")
        })
      }
  }

}
