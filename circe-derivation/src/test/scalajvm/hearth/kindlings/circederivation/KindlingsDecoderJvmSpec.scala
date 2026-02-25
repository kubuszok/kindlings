package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Decoder, Json}

final class KindlingsDecoderJvmSpec extends MacroSuite {

  group("KindlingsDecoder (JVM-only)") {

    group("Java enum decoder (enumAsStrings)") {

      test("decode Java enum value from string") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsDecoder.decode[JavaColor](Json.fromString("RED")) ==> Right(JavaColor.RED: JavaColor)
      }

      test("decode all Java enum values from strings") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsDecoder.decode[JavaColor](Json.fromString("GREEN")) ==> Right(JavaColor.GREEN: JavaColor)
        KindlingsDecoder.decode[JavaColor](Json.fromString("BLUE")) ==> Right(JavaColor.BLUE: JavaColor)
      }

      test("Java enum with name transform") {
        implicit val config: Configuration =
          Configuration(enumAsStrings = true, transformConstructorNames = _.toLowerCase)
        KindlingsDecoder.decode[JavaColor](Json.fromString("red")) ==> Right(JavaColor.RED: JavaColor)
      }
    }

    group("java.time as fields (user-provided implicits)") {

      test("case class with Instant field using user-provided decoder") {
        implicit val instantDecoder: Decoder[java.time.Instant] =
          Decoder.decodeLong.map(java.time.Instant.ofEpochMilli)
        val json = Json.obj("name" -> Json.fromString("event"), "ts" -> Json.fromLong(1700000000000L))
        KindlingsDecoder.decode[WithInstant](json) ==> Right(
          WithInstant("event", java.time.Instant.ofEpochMilli(1700000000000L))
        )
      }
    }
  }
}
