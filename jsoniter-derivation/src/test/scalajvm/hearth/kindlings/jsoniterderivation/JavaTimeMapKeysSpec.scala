package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

/** java.time map keys (jsoniter-scala `readKeyAsX` / `writeKey` overloads). JVM-only, like the other java.time tests
  * of this module (see `KindlingsJsonValueCodecJvmSpec`).
  */
final class JavaTimeMapKeysSpec extends MacroSuite {

  group("java.time map keys") {

    test("Instant, LocalDate, Duration and ZoneOffset keys round-trip") {
      val instants = KindlingsJsonValueCodec.derived[Map[java.time.Instant, Int]]
      val instantValue = Map(java.time.Instant.parse("2020-01-01T00:00:00Z") -> 1)
      writeToString(instantValue)(instants) ==> """{"2020-01-01T00:00:00Z":1}"""
      readFromString[Map[java.time.Instant, Int]]("""{"2020-01-01T00:00:00Z":1}""")(instants) ==> instantValue

      val dates = KindlingsJsonValueCodec.derived[Map[java.time.LocalDate, Int]]
      val dateValue = Map(java.time.LocalDate.of(2020, 1, 2) -> 2)
      writeToString(dateValue)(dates) ==> """{"2020-01-02":2}"""
      readFromString[Map[java.time.LocalDate, Int]]("""{"2020-01-02":2}""")(dates) ==> dateValue

      val durations = KindlingsJsonValueCodec.derived[Map[java.time.Duration, Int]]
      val durationValue = Map(java.time.Duration.ofSeconds(5) -> 3)
      readFromString[Map[java.time.Duration, Int]](writeToString(durationValue)(durations))(durations) ==> durationValue

      val offsets = KindlingsJsonValueCodec.derived[Map[java.time.ZoneOffset, Int]]
      val offsetValue = Map(java.time.ZoneOffset.ofHours(2) -> 4)
      readFromString[Map[java.time.ZoneOffset, Int]](writeToString(offsetValue)(offsets))(offsets) ==> offsetValue
    }
  }
}
