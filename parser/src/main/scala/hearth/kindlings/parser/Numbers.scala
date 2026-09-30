package hearth.kindlings.parser

/** Number conversions for [[Terminal.mapSlice]]: they read the digits straight from the input, without copying the token
  * into a `String` and re-validating it (`terminal("[0-9]+").mapSlice(Numbers.int)`). They are methods, so that the
  * macro inlines them as direct calls (a `Function3` value would box the bounds and the result).
  *
  * They expect the token to be a well-formed number (which the terminal's pattern guarantees) and give the same
  * results as `toInt` / `toLong` / `toDouble` of the token's text, throwing `NumberFormatException` on overflow or
  * malformed text.
  */
object Numbers {

  /** `input.substring(start, end).toInt`: an optional sign followed by decimal digits. */
  def int(input: String, start: Int, end: Int): Int = {
    val value = long(input, start, end)
    if (value < Int.MinValue || value > Int.MaxValue) throw new NumberFormatException(slice(input, start, end))
    value.toInt
  }

  /** `input.substring(start, end).toLong`: an optional sign followed by decimal digits. */
  def long(input: String, start: Int, end: Int): Long = {
    var i = start
    val negative = i < end && input.charAt(i) == '-'
    if (i < end && (negative || input.charAt(i) == '+')) i += 1
    if (i == end || end - i > 18) java.lang.Long.parseLong(slice(input, start, end)) // empty or maybe overflowing
    else {
      var value = 0L
      while (i < end) {
        val digit = input.charAt(i) - '0'
        if (digit < 0 || digit > 9) throw new NumberFormatException(slice(input, start, end))
        value = value * 10 + digit
        i += 1
      }
      if (negative) -value else value
    }
  }

  /** `input.substring(start, end).toDouble` for decimal numbers (`-12.5e3`).
    *
    * Clinger's fast path: when the significand has at most 15 digits and the decimal exponent is within ±22, the
    * significand and the power of ten are exact doubles, so one multiplication or division gives the correctly rounded
    * result. Anything else (more digits, larger exponents, `NaN`, hexadecimal, ...) goes to `java.lang.Double`.
    */
  def double(input: String, start: Int, end: Int): Double = {
    var i = start
    val negative = i < end && input.charAt(i) == '-'
    if (i < end && (negative || input.charAt(i) == '+')) i += 1
    var significand = 0L
    var digits = 0 // significant digits in `significand` (leading zeros skipped)
    var mantissa = 0 // all digits before the exponent
    var exponent = 0
    var ok = i < end
    var c = 0
    // integer part
    while (i < end && { c = input.charAt(i) - '0'; c >= 0 && c <= 9 }) {
      if (digits > 0 || c != 0) { significand = significand * 10 + c; digits += 1 }
      mantissa += 1
      i += 1
    }
    // fraction
    if (i < end && input.charAt(i) == '.') {
      i += 1
      while (i < end && { c = input.charAt(i) - '0'; c >= 0 && c <= 9 }) {
        if (digits > 0 || c != 0) { significand = significand * 10 + c; digits += 1 }
        exponent -= 1
        mantissa += 1
        i += 1
      }
    }
    // exponent
    if (i < end && (input.charAt(i) == 'e' || input.charAt(i) == 'E')) {
      i += 1
      val negativeExp = i < end && input.charAt(i) == '-'
      if (i < end && (negativeExp || input.charAt(i) == '+')) i += 1
      var exp = 0
      ok = ok && i < end
      while (i < end && { c = input.charAt(i) - '0'; c >= 0 && c <= 9 }) {
        if (exp < 100000) exp = exp * 10 + c
        i += 1
      }
      exponent += (if (negativeExp) -exp else exp)
    }
    if (ok && mantissa > 0 && i == end && digits <= 15 && exponent >= -22 && exponent <= 22) {
      val magnitude =
        if (exponent >= 0) significand.toDouble * Powers(exponent) else significand.toDouble / Powers(-exponent)
      if (negative) -magnitude else magnitude
    } else java.lang.Double.parseDouble(slice(input, start, end))
  }

  /** 10^0 .. 10^22, all exact doubles. */
  private val Powers: Array[Double] = Array.iterate(1.0, 23)(_ * 10)

  private def slice(input: String, start: Int, end: Int): String = input.substring(start, end)
}
