package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException}
import hearth.MacroSuite

case class WithInstant(ts: java.time.Instant)
case class WithLocalDate(date: java.time.LocalDate)
case class WithLocalTime(time: java.time.LocalTime)
case class WithLocalDateTime(dt: java.time.LocalDateTime)
case class WithOffsetDateTime(dt: java.time.OffsetDateTime)
case class WithZonedDateTime(dt: java.time.ZonedDateTime)
case class WithDuration(dur: java.time.Duration)
case class WithPeriod(per: java.time.Period)

final class KindlingsJsonValueCodecJvmSpec extends MacroSuite {

  group("KindlingsJsonValueCodec (JVM-only)") {

    group("Java enum (enumAsStrings)") {

      test("Java enum round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derive[JavaColor]
        val json = writeToString[JavaColor](JavaColor.RED)(codec)
        json ==> "\"RED\""
        readFromString[JavaColor](json)(codec) ==> JavaColor.RED
      }

      test("all Java enum values round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derive[JavaColor]
        Seq(JavaColor.RED, JavaColor.GREEN, JavaColor.BLUE).foreach { v =>
          readFromString[JavaColor](writeToString[JavaColor](v)(codec))(codec) ==> v
        }
      }
    }

    group("java.time types") {

      test("Instant round-trip") {
        val codec = KindlingsJsonValueCodec.derive[WithInstant]
        val instant = java.time.Instant.parse("2024-01-15T10:30:00Z")
        val value = WithInstant(instant)
        val json = writeToString(value)(codec)
        json ==> """{"ts":"2024-01-15T10:30:00Z"}"""
        readFromString[WithInstant](json)(codec) ==> value
      }

      test("LocalDate round-trip") {
        val codec = KindlingsJsonValueCodec.derive[WithLocalDate]
        val date = java.time.LocalDate.of(2024, 1, 15)
        val value = WithLocalDate(date)
        val json = writeToString(value)(codec)
        json ==> """{"date":"2024-01-15"}"""
        readFromString[WithLocalDate](json)(codec) ==> value
      }

      test("LocalTime round-trip") {
        val codec = KindlingsJsonValueCodec.derive[WithLocalTime]
        val time = java.time.LocalTime.of(10, 30, 0)
        val value = WithLocalTime(time)
        val json = writeToString(value)(codec)
        json ==> """{"time":"10:30"}"""
        readFromString[WithLocalTime](json)(codec) ==> value
      }

      test("LocalDateTime round-trip") {
        val codec = KindlingsJsonValueCodec.derive[WithLocalDateTime]
        val dt = java.time.LocalDateTime.of(2024, 1, 15, 10, 30, 0)
        val value = WithLocalDateTime(dt)
        val json = writeToString(value)(codec)
        json ==> """{"dt":"2024-01-15T10:30"}"""
        readFromString[WithLocalDateTime](json)(codec) ==> value
      }

      test("OffsetDateTime round-trip") {
        val codec = KindlingsJsonValueCodec.derive[WithOffsetDateTime]
        val dt = java.time.OffsetDateTime.of(2024, 1, 15, 10, 30, 0, 0, java.time.ZoneOffset.UTC)
        val value = WithOffsetDateTime(dt)
        val json = writeToString(value)(codec)
        json ==> """{"dt":"2024-01-15T10:30Z"}"""
        readFromString[WithOffsetDateTime](json)(codec) ==> value
      }

      test("ZonedDateTime round-trip") {
        val codec = KindlingsJsonValueCodec.derive[WithZonedDateTime]
        val dt = java.time.ZonedDateTime.of(2024, 1, 15, 10, 30, 0, 0, java.time.ZoneId.of("UTC"))
        val value = WithZonedDateTime(dt)
        val json = writeToString(value)(codec)
        readFromString[WithZonedDateTime](json)(codec) ==> value
      }

      test("Duration round-trip") {
        val codec = KindlingsJsonValueCodec.derive[WithDuration]
        val dur = java.time.Duration.ofHours(2).plusMinutes(30)
        val value = WithDuration(dur)
        val json = writeToString(value)(codec)
        json ==> """{"dur":"PT2H30M"}"""
        readFromString[WithDuration](json)(codec) ==> value
      }

      test("Period round-trip") {
        val codec = KindlingsJsonValueCodec.derive[WithPeriod]
        val per = java.time.Period.of(1, 6, 15)
        val value = WithPeriod(per)
        val json = writeToString(value)(codec)
        json ==> """{"per":"P1Y6M15D"}"""
        readFromString[WithPeriod](json)(codec) ==> value
      }

      test("invalid Instant string produces decode error") {
        val codec = KindlingsJsonValueCodec.derive[WithInstant]
        val json = """{"ts":"not-a-date"}"""
        intercept[JsonReaderException] {
          readFromString[WithInstant](json)(codec)
        }
      }
    }
  }
}
