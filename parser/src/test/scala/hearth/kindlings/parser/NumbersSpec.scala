package hearth.kindlings.parser

import hearth.MacroSuite

final class NumbersSpec extends MacroSuite {

  private def double(s: String): Double = Numbers.double("<<" + s + ">>", 2, 2 + s.length)
  private def long(s: String): Long = Numbers.long("<<" + s + ">>", 2, 2 + s.length)

  group("Numbers") {

    test("double agrees with java.lang.Double.parseDouble") {
      val samples = List(
        "0",
        "-0",
        "0.0",
        "-0.0",
        "1",
        "12.5",
        "-12.5e3",
        "1e22",
        "1e23",
        "1e-22",
        "1e-23",
        "123456789012345",
        "1234567890123456",
        "12345678901234567890",
        "0.1",
        "0.3",
        "3.141592653589793",
        "2.2250738585072014E-308",
        "1.7976931348623157e308",
        "4.9e-324",
        "5.",
        ".5",
        "0000123.4500",
        "9007199254740993",
        "1.5E+10"
      )
      val random = new scala.util.Random(3)
      val generated = List.fill(2000) {
        val digits = 1 + random.nextInt(17)
        val mantissa = (1 to digits).map(_ => ('0' + random.nextInt(10)).toChar).mkString
        val point = random.nextInt(digits + 1)
        val body = if (point == digits) mantissa else mantissa.take(point).padTo(1, '0') + "." + mantissa.drop(point)
        val exp = if (random.nextBoolean()) "" else "e" + (random.nextInt(60) - 30)
        (if (random.nextBoolean()) "-" else "") + body + exp
      }
      (samples ++ generated).foreach { s =>
        val expected = java.lang.Double.parseDouble(s)
        val actual = double(s)
        assert(java.lang.Double.compare(actual, expected) == 0, s"$s: $actual vs $expected")
      }
    }

    test("double rejects malformed numbers like parseDouble") {
      List("", "-", ".", "1e", "1e+", "abc").foreach(s => intercept[NumberFormatException](double(s)))
    }

    test("long and int agree with toLong and toInt, including limits") {
      List("0", "-0", "+7", "123", "-9223372036854775808", "9223372036854775807", "999999999999999999").foreach { s =>
        long(s) ==> s.toLong
      }
      val _ = intercept[NumberFormatException](long("9223372036854775808"))
      val _ = intercept[NumberFormatException](long("12a"))
      Numbers.int("x2147483647", 1, 11) ==> Int.MaxValue
      val _ = intercept[NumberFormatException](Numbers.int("2147483648", 0, 10))
    }

    test("work with mapSlice") {
      val p = Grammar.grammar[Double, Id] { g =>
        import g.*
        val sum = nonTerminal[Double]
        skip(" +")
        sum ::= all(rep(terminal("-?[0-9]+(\\.[0-9]+)?").mapSlice(Numbers.double))).pure(xs => xs.sum)
        sum
      }
      p.parse("1 2.5 -0.5") ==> 3.0
    }
  }
}
