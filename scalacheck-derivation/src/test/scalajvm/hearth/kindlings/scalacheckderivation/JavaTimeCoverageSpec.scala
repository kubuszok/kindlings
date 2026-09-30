package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}
import org.scalacheck.rng.Seed
import hearth.kindlings.scalacheckderivation.extensions.*

import java.time.*
import java.time.temporal.{ChronoField, ChronoUnit}

object JavaTimeCoverageSpec {
  // Every java.time / legacy java.util date type ScalaCheck 1.20 has an `Arbitrary` for.
  final case class ArbTime(
      instant: Instant,
      duration: Duration,
      period: Period,
      localDate: LocalDate,
      localTime: LocalTime,
      localDateTime: LocalDateTime,
      offsetTime: OffsetTime,
      offsetDateTime: OffsetDateTime,
      zonedDateTime: ZonedDateTime,
      zoneId: ZoneId,
      zoneOffset: ZoneOffset,
      year: Year,
      yearMonth: YearMonth,
      monthDay: MonthDay,
      date: java.util.Date,
      calendar: java.util.Calendar
  )
  // Every java.time type ScalaCheck 1.20 has a `Cogen` for.
  final case class CogenTime(
      instant: Instant,
      duration: Duration,
      period: Period,
      localDate: LocalDate,
      localTime: LocalTime,
      localDateTime: LocalDateTime,
      offsetTime: OffsetTime,
      offsetDateTime: OffsetDateTime,
      zonedDateTime: ZonedDateTime,
      zoneId: ZoneId,
      zoneOffset: ZoneOffset,
      year: Year,
      yearMonth: YearMonth,
      monthDay: MonthDay,
      month: Month,
      dayOfWeek: DayOfWeek,
      chronoField: ChronoField,
      chronoUnit: ChronoUnit
  )
  final case class ShrinkTime(duration: Duration, period: Period)
}

@scala.annotation.nowarn
final class JavaTimeCoverageSpec extends munit.FunSuite {
  import JavaTimeCoverageSpec.*

  test("Arbitrary: every ScalaCheck java.time type") {
    val gen = Arbitrary.derived[ArbTime].arbitrary
    val values = (0 until 50).toList.flatMap(i => gen.apply(Gen.Parameters.default, Seed(i.toLong)))
    assertEquals(values.size, 50)
  }

  test("Cogen: every ScalaCheck java.time type") {
    val cogen = Cogen.derived[CogenTime]
    val value = CogenTime(Instant.EPOCH, Duration.ofSeconds(1), Period.ofDays(1), LocalDate.of(2020, 1, 1),
      LocalTime.NOON, LocalDateTime.of(2020, 1, 1, 12, 0), OffsetTime.of(LocalTime.NOON, ZoneOffset.UTC),
      OffsetDateTime.of(2020, 1, 1, 12, 0, 0, 0, ZoneOffset.UTC), ZonedDateTime.of(2020, 1, 1, 12, 0, 0, 0, ZoneOffset.UTC),
      ZoneOffset.UTC, ZoneOffset.UTC, Year.of(2020), YearMonth.of(2020, 1), MonthDay.of(1, 1), Month.MAY,
      DayOfWeek.MONDAY, ChronoField.YEAR, ChronoUnit.DAYS)
    val seed = Seed(0L)
    assertEquals(cogen.perturb(seed, value), cogen.perturb(seed, value))
    assertNotEquals(cogen.perturb(seed, value), cogen.perturb(seed, value.copy(year = Year.of(2021))))
  }

  test("Shrink: every ScalaCheck java.time type") {
    val result = Shrink.derived[ShrinkTime].shrink(ShrinkTime(Duration.ofSeconds(100), Period.ofDays(100))).toList
    assert(result.exists(_.duration != Duration.ofSeconds(100)))
    assert(result.exists(_.period != Period.ofDays(100)))
  }
}
