package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Encoder, Json}

final class KindlingsEncoderJvmSpec extends MacroSuite {

  group("KindlingsEncoder (JVM-only)") {

    group("Java enum (enumAsStrings)") {

      test("encode Java enum value as string") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsEncoder.encode[JavaColor](JavaColor.RED) ==> Json.fromString("RED")
      }

      test("encode all Java enum values as strings") {
        implicit val config: Configuration = Configuration(enumAsStrings = true)
        KindlingsEncoder.encode[JavaColor](JavaColor.GREEN) ==> Json.fromString("GREEN")
        KindlingsEncoder.encode[JavaColor](JavaColor.BLUE) ==> Json.fromString("BLUE")
      }

      test("Java enum with name transform") {
        implicit val config: Configuration =
          Configuration(enumAsStrings = true, transformConstructorNames = _.toLowerCase)
        KindlingsEncoder.encode[JavaColor](JavaColor.RED) ==> Json.fromString("red")
      }
    }

    group("java.time as fields (user-provided implicits)") {

      test("case class with Instant field using user-provided encoder") {
        implicit val instantEncoder: Encoder[java.time.Instant] =
          Encoder.encodeLong.contramap(_.toEpochMilli)
        KindlingsEncoder.encode(WithInstant("event", java.time.Instant.ofEpochMilli(1700000000000L))) ==>
          Json.obj("name" -> Json.fromString("event"), "ts" -> Json.fromLong(1700000000000L))
      }
    }
  }
}
