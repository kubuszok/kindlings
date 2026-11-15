package hearth.kindlings.jsoniterjson

/** String-backed number representation to preserve precision.
  *
  * Numbers are stored as their string representation to avoid precision loss that can occur with Double/Float
  * conversions. Conversion methods are provided to extract values in specific numeric types.
  */
final case class JsonNumber private (value: String) {

  /** `Some` whenever the number is an integer within the `Int` range, however it is written (`-546`, `-546.0`, `1e3`,
    * `1.5e1`); `None` for non-integral or out-of-range values. Like circe's `JsonNumber.toInt`.
    */
  def toInt: Option[Int] = toLong match {
    case Some(l) if l.isValidInt => Some(l.toInt)
    case _                       => None
  }

  /** `Some` whenever the number is an integer within the `Long` range, however it is written (`-546`, `-546.0`, `1e3`,
    * `1.5e1`); `None` for non-integral or out-of-range values. Like circe's `JsonNumber.toLong`.
    */
  def toLong: Option[Long] = try Some(value.toLong)
  catch {
    case _: NumberFormatException =>
      JsonNumber.integral(value) match {
        // at most 19 integer digits, so `longValueExact` never has to materialize a huge number
        case Some(bd) if bd.precision.toLong - bd.scale <= 19 =>
          try Some(bd.longValueExact)
          catch { case _: ArithmeticException => None }
        case _ => None
      }
  }

  def toDouble: Option[Double] = try {
    val d = value.toDouble
    if (d.isNaN || d.isInfinite) None else Some(d)
  } catch { case _: NumberFormatException => None }

  def toFloat: Option[Float] = try {
    val f = value.toFloat
    if (f.isNaN || f.isInfinite) None else Some(f)
  } catch { case _: NumberFormatException => None }

  def toBigDecimal: Option[BigDecimal] = try Some(BigDecimal(value))
  catch { case _: NumberFormatException => None }

  /** `Some` whenever the number is an integer, however it is written (`-546`, `-546.0`, `1e3`, `1.5e1`); `None` for
    * non-integral values. Like circe's `JsonNumber.toBigInt`, a number written with a fraction or an exponent is
    * converted only if its integer has at most [[JsonNumber.MaxBigIntDigits]] digits, so that e.g. `1e1000000000`
    * cannot exhaust the memory.
    */
  def toBigInt: Option[BigInt] = try Some(BigInt(value))
  catch {
    case _: NumberFormatException =>
      JsonNumber.integral(value) match {
        case Some(bd) if bd.precision.toLong - bd.scale <= JsonNumber.MaxBigIntDigits =>
          Some(BigInt(bd.toBigIntegerExact))
        case _ => None
      }
  }

  override def toString: String = value
}
object JsonNumber {

  /** The largest number of integer digits [[JsonNumber.toBigInt]] materializes for a number written with a fraction or
    * an exponent (the same limit as circe's).
    */
  final val MaxBigIntDigits = 1 << 18

  /** The number as a `java.math.BigDecimal` without trailing zeros, if it is an integer (scale <= 0). */
  private def integral(value: String): Option[java.math.BigDecimal] =
    try {
      val bd = new java.math.BigDecimal(value)
      if (bd.signum == 0) Some(java.math.BigDecimal.ZERO)
      else {
        val stripped = bd.stripTrailingZeros
        if (stripped.scale <= 0) Some(stripped) else None
      }
    } catch { case _: NumberFormatException => None }

  def fromInt(n: Int): JsonNumber = new JsonNumber(n.toString)
  def fromLong(n: Long): JsonNumber = new JsonNumber(n.toString)
  def fromDouble(n: Double): Option[JsonNumber] =
    if (n.isNaN || n.isInfinite) None else Some(new JsonNumber(n.toString))
  def fromFloat(n: Float): Option[JsonNumber] =
    if (n.isNaN || n.isInfinite) None else Some(new JsonNumber(n.toString))
  def fromBigDecimal(n: BigDecimal): JsonNumber = new JsonNumber(n.toString)
  def fromBigInt(n: BigInt): JsonNumber = new JsonNumber(n.toString)

  /** `Some` if `s` is a JSON number (`-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?`), like circe's
    * `JsonNumber.fromString`: e.g. `+1`, `1.`, `.5`, `01`, `NaN` and surrounding whitespace are rejected. The text is
    * kept as is (and written as is), also when its exponent is too large for a `BigDecimal` (then `toBigDecimal` is
    * `None` and `toDouble` is `0.0` or `None`).
    */
  def fromString(s: String): Option[JsonNumber] =
    if (isJsonNumber(s)) Some(new JsonNumber(s)) else None

  private def isJsonNumber(s: String): Boolean = {
    val len = s.length
    var i = 0
    def isDigit(at: Int): Boolean = at < len && s.charAt(at) >= '0' && s.charAt(at) <= '9'
    def digits(): Boolean = // at least one digit
      if (!isDigit(i)) false
      else {
        while (isDigit(i)) i += 1
        true
      }
    if (i < len && s.charAt(i) == '-') i += 1
    if (i < len && s.charAt(i) == '0') i += 1
    else if (!digits()) return false
    if (i < len && s.charAt(i) == '.') {
      i += 1
      if (!digits()) return false
    }
    if (i < len && (s.charAt(i) == 'e' || s.charAt(i) == 'E')) {
      i += 1
      if (i < len && (s.charAt(i) == '+' || s.charAt(i) == '-')) i += 1
      if (!digits()) return false
    }
    i == len
  }
}
