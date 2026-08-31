package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import io.circe.{Json, JsonObject}

trait EncoderHandleAsBuiltInTypeRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsBuiltInTypeRule extends EncoderDerivationRule("handle as built-in type") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a built-in type") >> {
        implicit val JsonT: Type[Json] = Types.Json
        val value = ectx.value

        val result: Option[Expr[Json]] =
          if (Type[A] =:= Type.of[String])
            Some(Expr.quote(Json.fromString(Expr.splice(value).asInstanceOf[String])))
          else if (Type[A] =:= Type.of[Int])
            Some(Expr.quote(Json.fromInt(Expr.splice(value).asInstanceOf[Int])))
          else if (Type[A] =:= Type.of[Long])
            Some(Expr.quote(Json.fromLong(Expr.splice(value).asInstanceOf[Long])))
          else if (Type[A] =:= Type.of[Double])
            Some(Expr.quote(Json.fromDoubleOrNull(Expr.splice(value).asInstanceOf[Double])))
          else if (Type[A] =:= Type.of[Float])
            Some(Expr.quote(Json.fromFloatOrNull(Expr.splice(value).asInstanceOf[Float])))
          else if (Type[A] =:= Type.of[Boolean])
            Some(Expr.quote(Json.fromBoolean(Expr.splice(value).asInstanceOf[Boolean])))
          else if (Type[A] =:= Type.of[Short])
            Some(Expr.quote(Json.fromInt(Expr.splice(value).asInstanceOf[Short].toInt)))
          else if (Type[A] =:= Type.of[Byte])
            Some(Expr.quote(Json.fromInt(Expr.splice(value).asInstanceOf[Byte].toInt)))
          else if (Type[A] =:= Type.of[Char])
            Some(Expr.quote(Json.fromString(Expr.splice(value).asInstanceOf[Char].toString)))
          else if (Type[A] =:= Type.of[BigDecimal])
            Some(Expr.quote(Json.fromBigDecimal(Expr.splice(value).asInstanceOf[BigDecimal])))
          else if (Type[A] =:= Type.of[BigInt])
            Some(Expr.quote(Json.fromBigInt(Expr.splice(value).asInstanceOf[BigInt])))
          else if (Type[A] =:= Type.of[Unit])
            Some(Expr.quote(Json.obj()))
          else if (Type[A] =:= Type.of[Json])
            Some(Expr.quote(Expr.splice(value).asInstanceOf[Json]))
          else if (Type[A] =:= Type.of[JsonObject])
            Some(Expr.quote(Json.fromJsonObject(Expr.splice(value).asInstanceOf[JsonObject])))
          else
            None

        MIO.pure(result match {
          case Some(expr) => Rule.matched(expr)
          case None       => Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type")
        })
      }
  }

}
