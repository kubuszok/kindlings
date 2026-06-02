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
case class WithMonthDay(md: java.time.MonthDay)
case class WithOffsetTime(ot: java.time.OffsetTime)
case class WithPeriod(per: java.time.Period)
case class WithYear(y: java.time.Year)
case class WithYearMonth(ym: java.time.YearMonth)
case class WithZoneId(zid: java.time.ZoneId)
case class WithZoneOffset(zo: java.time.ZoneOffset)

case class WithBoxedPrimitives(
    b: java.lang.Byte,
    s: java.lang.Short,
    i: java.lang.Integer,
    l: java.lang.Long,
    f: java.lang.Float,
    d: java.lang.Double,
    bool: java.lang.Boolean,
    c: java.lang.Character
)

final class KindlingsJsonValueCodecJvmSpec extends MacroSuite {

  group("KindlingsJsonValueCodec (JVM-only)") {

    group("Java enum (enumAsStrings)") {

      test("Java enum round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derived[JavaColor]
        val json = writeToString[JavaColor](JavaColor.RED)(codec)
        json ==> "\"RED\""
        readFromString[JavaColor](json)(codec) ==> JavaColor.RED
      }

      test("all Java enum values round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derived[JavaColor]
        Seq(JavaColor.RED, JavaColor.GREEN, JavaColor.BLUE).foreach { v =>
          readFromString[JavaColor](writeToString[JavaColor](v)(codec))(codec) ==> v
        }
      }
    }

    group("java.time types") {

      test("Instant round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithInstant]
        val instant = java.time.Instant.parse("2024-01-15T10:30:00Z")
        val value = WithInstant(instant)
        val json = writeToString(value)(codec)
        json ==> """{"ts":"2024-01-15T10:30:00Z"}"""
        readFromString[WithInstant](json)(codec) ==> value
      }

      test("LocalDate round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithLocalDate]
        val date = java.time.LocalDate.of(2024, 1, 15)
        val value = WithLocalDate(date)
        val json = writeToString(value)(codec)
        json ==> """{"date":"2024-01-15"}"""
        readFromString[WithLocalDate](json)(codec) ==> value
      }

      test("LocalTime round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithLocalTime]
        val time = java.time.LocalTime.of(10, 30, 0)
        val value = WithLocalTime(time)
        val json = writeToString(value)(codec)
        json ==> """{"time":"10:30"}"""
        readFromString[WithLocalTime](json)(codec) ==> value
      }

      test("LocalDateTime round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithLocalDateTime]
        val dt = java.time.LocalDateTime.of(2024, 1, 15, 10, 30, 0)
        val value = WithLocalDateTime(dt)
        val json = writeToString(value)(codec)
        json ==> """{"dt":"2024-01-15T10:30"}"""
        readFromString[WithLocalDateTime](json)(codec) ==> value
      }

      test("OffsetDateTime round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithOffsetDateTime]
        val dt = java.time.OffsetDateTime.of(2024, 1, 15, 10, 30, 0, 0, java.time.ZoneOffset.UTC)
        val value = WithOffsetDateTime(dt)
        val json = writeToString(value)(codec)
        json ==> """{"dt":"2024-01-15T10:30Z"}"""
        readFromString[WithOffsetDateTime](json)(codec) ==> value
      }

      test("ZonedDateTime round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithZonedDateTime]
        val dt = java.time.ZonedDateTime.of(2024, 1, 15, 10, 30, 0, 0, java.time.ZoneId.of("UTC"))
        val value = WithZonedDateTime(dt)
        val json = writeToString(value)(codec)
        readFromString[WithZonedDateTime](json)(codec) ==> value
      }

      test("Duration round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithDuration]
        val dur = java.time.Duration.ofHours(2).plusMinutes(30)
        val value = WithDuration(dur)
        val json = writeToString(value)(codec)
        json ==> """{"dur":"PT2H30M"}"""
        readFromString[WithDuration](json)(codec) ==> value
      }

      test("Period round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithPeriod]
        val per = java.time.Period.of(1, 6, 15)
        val value = WithPeriod(per)
        val json = writeToString(value)(codec)
        json ==> """{"per":"P1Y6M15D"}"""
        readFromString[WithPeriod](json)(codec) ==> value
      }

      test("MonthDay round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithMonthDay]
        val md = java.time.MonthDay.of(12, 25)
        val value = WithMonthDay(md)
        val json = writeToString(value)(codec)
        json ==> """{"md":"--12-25"}"""
        readFromString[WithMonthDay](json)(codec) ==> value
      }

      test("OffsetTime round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithOffsetTime]
        val ot = java.time.OffsetTime.of(10, 15, 30, 0, java.time.ZoneOffset.ofHours(1))
        val value = WithOffsetTime(ot)
        val json = writeToString(value)(codec)
        json ==> """{"ot":"10:15:30+01:00"}"""
        readFromString[WithOffsetTime](json)(codec) ==> value
      }

      test("Year round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithYear]
        val y = java.time.Year.of(2024)
        val value = WithYear(y)
        val json = writeToString(value)(codec)
        json ==> """{"y":"2024"}"""
        readFromString[WithYear](json)(codec) ==> value
      }

      test("YearMonth round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithYearMonth]
        val ym = java.time.YearMonth.of(2024, 6)
        val value = WithYearMonth(ym)
        val json = writeToString(value)(codec)
        json ==> """{"ym":"2024-06"}"""
        readFromString[WithYearMonth](json)(codec) ==> value
      }

      test("ZoneId round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithZoneId]
        val zid = java.time.ZoneId.of("Europe/Warsaw")
        val value = WithZoneId(zid)
        val json = writeToString(value)(codec)
        json ==> """{"zid":"Europe/Warsaw"}"""
        readFromString[WithZoneId](json)(codec) ==> value
      }

      test("ZoneOffset round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithZoneOffset]
        val zo = java.time.ZoneOffset.ofHours(5)
        val value = WithZoneOffset(zo)
        val json = writeToString(value)(codec)
        json ==> """{"zo":"+05:00"}"""
        readFromString[WithZoneOffset](json)(codec) ==> value
      }

      test("ZoneOffset UTC round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithZoneOffset]
        val zo = java.time.ZoneOffset.UTC
        val value = WithZoneOffset(zo)
        val json = writeToString(value)(codec)
        json ==> """{"zo":"Z"}"""
        readFromString[WithZoneOffset](json)(codec) ==> value
      }

      test("invalid Instant string produces decode error") {
        val codec = KindlingsJsonValueCodec.derived[WithInstant]
        val json = """{"ts":"not-a-date"}"""
        intercept[JsonReaderException] {
          readFromString[WithInstant](json)(codec)
        }
      }

      test("invalid MonthDay string produces decode error") {
        val codec = KindlingsJsonValueCodec.derived[WithMonthDay]
        val json = """{"md":"not-a-month-day"}"""
        intercept[JsonReaderException] {
          readFromString[WithMonthDay](json)(codec)
        }
      }

      test("invalid OffsetTime string produces decode error") {
        val codec = KindlingsJsonValueCodec.derived[WithOffsetTime]
        val json = """{"ot":"not-a-time"}"""
        intercept[JsonReaderException] {
          readFromString[WithOffsetTime](json)(codec)
        }
      }

      test("invalid Year string produces decode error") {
        val codec = KindlingsJsonValueCodec.derived[WithYear]
        val json = """{"y":"not-a-year"}"""
        intercept[JsonReaderException] {
          readFromString[WithYear](json)(codec)
        }
      }

      test("invalid YearMonth string produces decode error") {
        val codec = KindlingsJsonValueCodec.derived[WithYearMonth]
        val json = """{"ym":"not-a-year-month"}"""
        intercept[JsonReaderException] {
          readFromString[WithYearMonth](json)(codec)
        }
      }

      test("invalid ZoneId string produces decode error") {
        val codec = KindlingsJsonValueCodec.derived[WithZoneId]
        val json = """{"zid":"Not/A/Real/Zone"}"""
        intercept[JsonReaderException] {
          readFromString[WithZoneId](json)(codec)
        }
      }

      test("invalid ZoneOffset string produces decode error") {
        val codec = KindlingsJsonValueCodec.derived[WithZoneOffset]
        val json = """{"zo":"not-an-offset"}"""
        intercept[JsonReaderException] {
          readFromString[WithZoneOffset](json)(codec)
        }
      }
    }

    group("Java boxed primitives") {

      test("java.lang.Byte round-trip") {
        val codec = KindlingsJsonValueCodec.derived[java.lang.Byte]
        val value: java.lang.Byte = java.lang.Byte.valueOf(42.toByte)
        val json = writeToString(value)(codec)
        json ==> "42"
        readFromString[java.lang.Byte](json)(codec) ==> value
      }

      test("java.lang.Short round-trip") {
        val codec = KindlingsJsonValueCodec.derived[java.lang.Short]
        val value: java.lang.Short = java.lang.Short.valueOf(1234.toShort)
        val json = writeToString(value)(codec)
        json ==> "1234"
        readFromString[java.lang.Short](json)(codec) ==> value
      }

      test("java.lang.Integer round-trip") {
        val codec = KindlingsJsonValueCodec.derived[java.lang.Integer]
        val value: java.lang.Integer = java.lang.Integer.valueOf(42)
        val json = writeToString(value)(codec)
        json ==> "42"
        readFromString[java.lang.Integer](json)(codec) ==> value
      }

      test("java.lang.Long round-trip") {
        val codec = KindlingsJsonValueCodec.derived[java.lang.Long]
        val value: java.lang.Long = java.lang.Long.valueOf(123456789L)
        val json = writeToString(value)(codec)
        json ==> "123456789"
        readFromString[java.lang.Long](json)(codec) ==> value
      }

      test("java.lang.Float round-trip") {
        val codec = KindlingsJsonValueCodec.derived[java.lang.Float]
        val value: java.lang.Float = java.lang.Float.valueOf(3.14f)
        val json = writeToString(value)(codec)
        readFromString[java.lang.Float](json)(codec) ==> value
      }

      test("java.lang.Double round-trip") {
        val codec = KindlingsJsonValueCodec.derived[java.lang.Double]
        val value: java.lang.Double = java.lang.Double.valueOf(2.71828)
        val json = writeToString(value)(codec)
        readFromString[java.lang.Double](json)(codec) ==> value
      }

      test("java.lang.Boolean round-trip") {
        val codec = KindlingsJsonValueCodec.derived[java.lang.Boolean]
        val value: java.lang.Boolean = java.lang.Boolean.TRUE
        val json = writeToString(value)(codec)
        json ==> "true"
        readFromString[java.lang.Boolean](json)(codec) ==> value
      }

      test("java.lang.Character round-trip") {
        val codec = KindlingsJsonValueCodec.derived[java.lang.Character]
        val value: java.lang.Character = java.lang.Character.valueOf('A')
        val json = writeToString(value)(codec)
        json ==> "\"A\""
        readFromString[java.lang.Character](json)(codec) ==> value
      }

      test("case class with boxed primitive fields round-trip") {
        val codec = KindlingsJsonValueCodec.derived[WithBoxedPrimitives]
        val value = WithBoxedPrimitives(
          b = java.lang.Byte.valueOf(1.toByte),
          s = java.lang.Short.valueOf(2.toShort),
          i = java.lang.Integer.valueOf(3),
          l = java.lang.Long.valueOf(4L),
          f = java.lang.Float.valueOf(5.5f),
          d = java.lang.Double.valueOf(6.6),
          bool = java.lang.Boolean.TRUE,
          c = java.lang.Character.valueOf('X')
        )
        val json = writeToString(value)(codec)
        readFromString[WithBoxedPrimitives](json)(codec) ==> value
      }

      test("Map with java.lang.Integer keys round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[java.lang.Integer, String]]
        val value = Map[java.lang.Integer, String](
          java.lang.Integer.valueOf(1) -> "one",
          java.lang.Integer.valueOf(2) -> "two"
        )
        val json = writeToString(value)(codec)
        readFromString[Map[java.lang.Integer, String]](json)(codec) ==> value
      }

      test("Map with java.lang.Long keys round-trip") {
        val codec = KindlingsJsonValueCodec.derived[Map[java.lang.Long, String]]
        val value = Map[java.lang.Long, String](
          java.lang.Long.valueOf(100L) -> "hundred",
          java.lang.Long.valueOf(200L) -> "two hundred"
        )
        val json = writeToString(value)(codec)
        readFromString[Map[java.lang.Long, String]](json)(codec) ==> value
      }
    }

    group("javaEnumValueNameMapper") {

      test("Java enum round-trip with snake_case mapper") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withEnumAsStrings
          .withJavaEnumValueNameMapper(JsoniterConfig.snakeCase)
        val codec = KindlingsJsonValueCodec.derived[JavaColor]
        val json = writeToString[JavaColor](JavaColor.RED)(codec)
        assert(json.contains("r_e_d") || json.contains("red"), s"expected mapped name in: $json")
      }

      test("Java enum round-trip with lowercase mapper") {
        implicit val config: JsoniterConfig = JsoniterConfig.default.withEnumAsStrings
          .withJavaEnumValueNameMapper(_.toLowerCase)
        val codec = KindlingsJsonValueCodec.derived[JavaColor]
        val json = writeToString[JavaColor](JavaColor.GREEN)(codec)
        json ==> "\"green\""
        val decoded = readFromString[JavaColor](json)(codec)
        decoded ==> JavaColor.GREEN
      }
    }
  }
}
