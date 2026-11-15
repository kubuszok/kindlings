package hearth.kindlings.jsoniterjson

import hearth.Suite

/** `toInt`/`toLong`/`toBigInt` follow circe's `JsonNumber`: `Some` whenever the number is an integer within range,
  * however it is written, `None` only for non-integral or out-of-range values.
  */
final class JsonNumberSpec extends Suite {

  private def num(s: String): JsonNumber = JsonNumber.fromString(s).get

  group("integral values written as plain integers") {

    test("-546") {
      num("-546").toInt ==> Some(-546)
      num("-546").toLong ==> Some(-546L)
      num("-546").toBigInt ==> Some(BigInt(-546))
    }

    test("0 and -0") {
      num("0").toInt ==> Some(0)
      num("-0").toLong ==> Some(0L)
      num("0.0").toInt ==> Some(0)
      num("0e5").toBigInt ==> Some(BigInt(0))
    }
  }

  group("integral values written with a decimal point or exponent") {

    test("-546.0") {
      num("-546.0").toInt ==> Some(-546)
      num("-546.0").toLong ==> Some(-546L)
      num("-546.0").toBigInt ==> Some(BigInt(-546))
    }

    test("1e3") {
      num("1e3").toInt ==> Some(1000)
      num("1e3").toLong ==> Some(1000L)
      num("1e3").toBigInt ==> Some(BigInt(1000))
    }

    test("1.5e1") {
      num("1.5e1").toInt ==> Some(15)
      num("1.5e1").toLong ==> Some(15L)
      num("1.5e1").toBigInt ==> Some(BigInt(15))
    }

    test("1500e-2 and 1.000") {
      num("1500e-2").toInt ==> Some(15)
      num("1.000").toLong ==> Some(1L)
      num("1E+2").toBigInt ==> Some(BigInt(100))
    }
  }

  group("non-integral values") {

    test("5.5") {
      num("5.5").toInt ==> None
      num("5.5").toLong ==> None
      num("5.5").toBigInt ==> None
    }

    test("1e-3 and 1.0000001") {
      num("1e-3").toInt ==> None
      num("1.0000001").toLong ==> None
      num("1.0000001").toBigInt ==> None
    }
  }

  group("range edges") {

    test("Int edges") {
      num("2147483647").toInt ==> Some(Int.MaxValue)
      num("-2147483648").toInt ==> Some(Int.MinValue)
      num("2147483647.0").toInt ==> Some(Int.MaxValue)
      num("-2.147483648e9").toInt ==> Some(Int.MinValue)
      num("2147483648").toInt ==> None
      num("2.147483648e9").toInt ==> None
      num("-2147483649.0").toInt ==> None
      num("2147483648").toLong ==> Some(2147483648L)
    }

    test("Long edges") {
      num("9223372036854775807").toLong ==> Some(Long.MaxValue)
      num("-9223372036854775808").toLong ==> Some(Long.MinValue)
      num("9223372036854775807.0").toLong ==> Some(Long.MaxValue)
      num("-9.223372036854775808e18").toLong ==> Some(Long.MinValue)
      num("9223372036854775808").toLong ==> None
      num("9.223372036854775808e18").toLong ==> None
      num("1e19").toLong ==> None
      num("1e19").toBigInt ==> Some(BigInt("10000000000000000000"))
    }

    test("huge exponents do not blow up") {
      num("1e1000000000").toInt ==> None
      num("1e1000000000").toLong ==> None
      num("1e1000000000").toBigInt ==> None
      num("1e-1000000000").toLong ==> None
      num("1e-1000000000").toBigInt ==> None
      // precision - scale overflows an Int here
      num("1e2147483647").toLong ==> None
      num("1e2147483647").toBigInt ==> None
    }

    test("big integers written with an exponent stay exact") {
      num("1.2345678901234567890123e22").toBigInt ==> Some(BigInt("12345678901234567890123"))
      num("123456789012345678901234567890").toBigInt ==> Some(BigInt("123456789012345678901234567890"))
    }
  }

  group("fromDouble") {

    test("integral doubles can be read back as Int/Long/BigInt") {
      val d = JsonNumber.fromDouble(-546.0).get
      d.toInt ==> Some(-546)
      d.toLong ==> Some(-546L)
      d.toBigInt ==> Some(BigInt(-546))
      JsonNumber.fromDouble(1.0e10).get.toLong ==> Some(10000000000L)
      JsonNumber.fromDouble(0.0).get.toInt ==> Some(0)
    }

    test("non-integral and out-of-range doubles are None") {
      JsonNumber.fromDouble(5.5).get.toLong ==> None
      JsonNumber.fromDouble(1.0e20).get.toLong ==> None
      JsonNumber.fromDouble(1.0e20).get.toBigInt ==> Some(BigInt("100000000000000000000"))
    }
  }

  group("fromString accepts JSON numbers only") {

    test("valid JSON numbers keep their text") {
      List("0", "-0", "-0.0", "12", "-1.50", "1e3", "1E+3", "1.5e-3", "1e2147483648").foreach { s =>
        JsonNumber.fromString(s).map(_.toString) ==> Some(s)
      }
    }

    test("texts BigDecimal accepts but JSON does not are rejected") {
      List("+1", "1.", ".5", "01", "-01", "1e", "1e+", "0x10", "NaN", "Infinity", " 1", "1 ", "", "-", "1_000").foreach {
        s => JsonNumber.fromString(s) ==> None
      }
    }

    test("toBigDecimal of exponents java.math.BigDecimal may reject is the same on every JDK (like circe)") {
      JsonNumber.fromString("1e2147483648").flatMap(_.toBigDecimal) ==>
        Some(BigDecimal(new java.math.BigDecimal(java.math.BigInteger.ONE, Int.MinValue)))
      JsonNumber.fromString("1.500e2147483649").flatMap(_.toBigDecimal) ==>
        Some(BigDecimal(new java.math.BigDecimal(java.math.BigInteger.valueOf(15), Int.MinValue)))
      JsonNumber.fromString("-0e99999999999").flatMap(_.toBigDecimal) ==> Some(BigDecimal(0))
      JsonNumber.fromString("1e2147483649").flatMap(_.toBigDecimal) ==> None
    }

    test("an exponent too large for BigDecimal is accepted, like circe") {
      val n = JsonNumber.fromString("1e-2147483649").get
      n.toBigDecimal ==> None
      n.toDouble ==> Some(0.0)
      n.toLong ==> None
      n.toBigInt ==> None
    }
  }
}
