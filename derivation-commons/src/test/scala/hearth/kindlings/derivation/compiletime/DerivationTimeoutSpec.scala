package hearth.kindlings.derivation.compiletime

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

final class DerivationTimeoutSpec extends hearth.MacroSuite {

  private val Pat = DerivationTimeout.DurationPattern

  private def matches(input: String): Boolean = input match {
    case Pat(_, _) => true
    case _         => false
  }

  private def extract(input: String): (String, String) = input match {
    case Pat(num, unit) => (num, unit)
    case _              => fail(s"DurationPattern did not match: '$input'")
  }

  group("DerivationTimeout.Default") {

    test("default timeout is 5 seconds") {
      DerivationTimeout.Default ==> FiniteDuration(5, TimeUnit.SECONDS)
    }

    test("default timeout toSeconds is 5") {
      DerivationTimeout.Default.toSeconds ==> 5L
    }
  }

  group("DerivationTimeout.DurationPattern") {

    // Seconds formats
    test("matches Ns format") {
      val (num, unit) = extract("30s")
      num ==> "30"
      unit ==> "s"
    }

    test("matches Nsecond format") {
      val (num, unit) = extract("30second")
      num ==> "30"
      unit ==> "second"
    }

    test("matches Nseconds format") {
      val (num, unit) = extract("30seconds")
      num ==> "30"
      unit ==> "seconds"
    }

    // Milliseconds formats
    test("matches Nms format") {
      val (num, unit) = extract("5000ms")
      num ==> "5000"
      unit ==> "ms"
    }

    test("matches Nmillis format") {
      val (num, unit) = extract("5000millis")
      num ==> "5000"
      unit ==> "millis"
    }

    test("matches Nmilliseconds format") {
      val (num, unit) = extract("5000milliseconds")
      num ==> "5000"
      unit ==> "milliseconds"
    }

    // Minutes formats
    test("matches Nm format") {
      val (num, unit) = extract("1m")
      num ==> "1"
      unit ==> "m"
    }

    test("matches Nminute format") {
      val (num, unit) = extract("1minute")
      num ==> "1"
      unit ==> "minute"
    }

    test("matches Nminutes format") {
      val (num, unit) = extract("1minutes")
      num ==> "1"
      unit ==> "minutes"
    }

    // Whitespace handling
    test("matches with leading and trailing whitespace") {
      val (num, unit) = extract("  30s  ")
      num ==> "30"
      unit ==> "s"
    }

    test("matches with whitespace between number and unit") {
      val (num, unit) = extract("30 s")
      num ==> "30"
      unit ==> "s"
    }

    // Zero value (matches pattern but parseDurationString would reject as non-positive)
    test("matches zero value syntactically") {
      val (num, unit) = extract("0s")
      num ==> "0"
      unit ==> "s"
    }

    // Invalid formats do NOT match
    test("does not match plain integer without unit") {
      assert(!matches("30"))
    }

    test("does not match invalid unit") {
      assert(!matches("30h"))
    }

    test("does not match empty string") {
      assert(!matches(""))
    }

    test("does not match text only") {
      assert(!matches("seconds"))
    }

    test("does not match negative number") {
      assert(!matches("-5s"))
    }

    test("does not match decimal number") {
      assert(!matches("1.5s"))
    }
  }
}
