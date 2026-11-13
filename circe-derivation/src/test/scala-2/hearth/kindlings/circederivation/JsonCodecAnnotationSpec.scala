package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Encoder, Json}

// Scala 2 only: macro annotations (-Ymacro-annotations); Scala 3 uses `derives`.
@JsonCodec case class AnnPerson(firstName: String, age: Int)
@JsonCodec sealed trait AnnShape
@JsonCodec case class AnnCircle(r: Double) extends AnnShape
@JsonCodec(encodeOnly = true) case class AnnEncOnly(a: Int)
@JsonCodec(decodeOnly = true) case class AnnDecOnly(a: Int)
@JsonCodec case class AnnBox[A](value: A)
@JsonCodec case class AnnWithCompanion(a: Int)
object AnnWithCompanion { val extra: Int = 1 }

object AnnConfigured {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
  @ConfiguredJsonCodec case class AnnSnake(firstName: String)
  @JsonCodec case class AnnIgnoresConfig(firstName: String)
}

final class JsonCodecAnnotationSpec extends MacroSuite {

  group("@JsonCodec / @ConfiguredJsonCodec (Scala 2 macro annotations)") {

    test("codec for a case class and a sealed trait") {
      val json = Json.obj("firstName" -> Json.fromString("a"), "age" -> Json.fromInt(1))
      Encoder[AnnPerson].apply(AnnPerson("a", 1)) ==> json
      Decoder[AnnPerson].decodeJson(json) ==> Right(AnnPerson("a", 1))
      val shape: AnnShape = AnnCircle(1.0)
      Decoder[AnnShape].decodeJson(Encoder[AnnShape].apply(shape)) ==> Right(shape)
      assert(AnnWithCompanion.extra == 1)
      Encoder[AnnWithCompanion].apply(AnnWithCompanion(2)) ==> Json.obj("a" -> Json.fromInt(2))
    }

    test("encodeOnly / decodeOnly") {
      Encoder[AnnEncOnly].apply(AnnEncOnly(1)) ==> Json.obj("a" -> Json.fromInt(1))
      Decoder[AnnDecOnly].decodeJson(Json.obj("a" -> Json.fromInt(1))) ==> Right(AnnDecOnly(1))
      compileErrors("io.circe.Decoder[hearth.kindlings.circederivation.AnnEncOnly]").check("could not find implicit")
    }

    test("type parameters") {
      Encoder[AnnBox[Int]].apply(AnnBox(1)) ==> Json.obj("value" -> Json.fromInt(1))
      Decoder[AnnBox[String]].decodeJson(Json.obj("value" -> Json.fromString("x"))) ==> Right(AnnBox("x"))
    }

    test("@ConfiguredJsonCodec uses the configuration in scope, @JsonCodec does not") {
      import AnnConfigured.*
      Encoder[AnnSnake].apply(AnnSnake("a")) ==> Json.obj("first_name" -> Json.fromString("a"))
      Encoder[AnnIgnoresConfig].apply(AnnIgnoresConfig("a")) ==> Json.obj("firstName" -> Json.fromString("a"))
    }
  }
}
