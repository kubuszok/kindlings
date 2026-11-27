package hearth.kindlings.jsoniterjson

/** A JSON number that keeps what it was made from, like circe's `JsonNumber`: a `Long`, a `Float`, a `Double`, a
  * `java.math.BigDecimal` (exact) or the text of a JSON number. Nothing is converted until asked for, so no precision
  * is lost; [[value]] (also `toString`) is the text of the number, and two numbers are equal when their texts are.
  */
final class JsonNumber private (
    private[jsoniterjson] val kind: Int,
    private val bits: Long, // the `Long`, or the bits of the `Double` / `Float`
    private val big: java.math.BigDecimal,
    private var text: String // the text, once it is known
) extends Product
    with Serializable {

  /** The text of the number, as it is written by default. */
  def value: String = {
    var t = text
    if (t eq null) {
      t = (kind: @scala.annotation.switch) match {
        case JsonNumber.LongKind   => java.lang.Long.toString(bits)
        case JsonNumber.DoubleKind =>
          com.github.plokhotnyuk.jsoniter_scala.core.writeToStringReentrant(doubleValue)(JsonNumber.DoubleText)
        case JsonNumber.FloatKind =>
          com.github.plokhotnyuk.jsoniter_scala.core.writeToStringReentrant(floatValue)(JsonNumber.FloatText)
        case _ => big.toString
      }
      text = t // (a benign race: every thread computes the same immutable text)
    }
    t
  }

  private[jsoniterjson] def longValue: Long = bits
  private[jsoniterjson] def doubleValue: Double = java.lang.Double.longBitsToDouble(bits)
  private[jsoniterjson] def floatValue: Float = java.lang.Float.intBitsToFloat(bits.toInt)
  private[jsoniterjson] def bigDecimalValue: java.math.BigDecimal = big

  // (what the former `case class JsonNumber(value: String)` provided)
  def copy(value: String = this.value): JsonNumber = JsonNumber.unsafeFromJsonText(value)
  def _1: String = value
  override def productPrefix: String = "JsonNumber"
  def productArity: Int = 1
  def productElement(n: Int): Any = if (n == 0) value else throw new IndexOutOfBoundsException(n.toString)
  def canEqual(that: Any): Boolean = that.isInstanceOf[JsonNumber]
  override def equals(that: Any): Boolean = that match {
    case n: JsonNumber => (this eq n) || value == n.value
    case _             => false
  }
  override def hashCode: Int = value.hashCode

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

  /** `Some` whenever the number's scale (without trailing zeros) fits an `Int`, like circe's `JsonNumber.toBigDecimal`:
    * also for texts that `java.math.BigDecimal`'s parser rejects (e.g. `1e2147483648`, which some JDKs parse and others
    * do not), so the result does not depend on the JDK.
    */
  def toBigDecimal: Option[BigDecimal] = try Some(BigDecimal(value))
  catch { case _: NumberFormatException => JsonNumber.fromUnscaledAndScale(value) }

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
object JsonNumber extends JsonNumberCompanionCompat {

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

  private val MaxInt = java.math.BigInteger.valueOf(Int.MaxValue.toLong)
  private val MinInt = java.math.BigInteger.valueOf(Int.MinValue.toLong)

  /** circe's `BiggerDecimal.toBigDecimal`: the digits without trailing zeros as the unscaled value, with a `BigInteger`
    * scale; `None` if the scale does not fit an `Int`. `value` is a valid JSON number.
    */
  private def fromUnscaledAndScale(value: String): Option[BigDecimal] = {
    val expAt = math.max(value.indexOf('e'), value.indexOf('E'))
    val mantissa = if (expAt < 0) value else value.substring(0, expAt)
    val exponent =
      if (expAt < 0) java.math.BigInteger.ZERO
      else new java.math.BigInteger(value.substring(expAt + 1).stripPrefix("+"))
    val dot = mantissa.indexOf('.')
    val fractionDigits = if (dot < 0) 0 else mantissa.length - dot - 1
    val digits = if (dot < 0) mantissa else mantissa.substring(0, dot) + mantissa.substring(dot + 1)
    var unscaled = new java.math.BigInteger(digits)
    if (unscaled.signum == 0) Some(BigDecimal(java.math.BigDecimal.ZERO))
    else {
      var trailingZeros = 0L
      var division = unscaled.divideAndRemainder(java.math.BigInteger.TEN)
      while (division(1).signum == 0) {
        unscaled = division(0)
        trailingZeros += 1
        division = unscaled.divideAndRemainder(java.math.BigInteger.TEN)
      }
      val scale = java.math.BigInteger.valueOf(fractionDigits - trailingZeros).subtract(exponent)
      if (scale.compareTo(MaxInt) <= 0 && scale.compareTo(MinInt) >= 0)
        Some(BigDecimal(new java.math.BigDecimal(unscaled, scale.intValue)))
      else None
    }
  }

  /** Doubles and floats are written by jsoniter-scala's writer (the shortest text that reads back as the same value),
    * like jsoniter-scala-circe writes circe's numbers, so that the text is the same on the JVM, Scala.js (where
    * `Double.toString` writes e.g. `1e10` as `10000000000`) and Scala Native. (`writeToStringReentrant`: numbers may be
    * created inside another codec's `encodeValue`/`decodeValue`.)
    */
  private object DoubleText extends com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[Double] {
    def nullValue: Double = 0.0
    def decodeValue(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader, default: Double): Double =
      in.readDouble()
    def encodeValue(x: Double, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit = out.writeVal(x)
  }
  private object FloatText extends com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[Float] {
    def nullValue: Float = 0.0f
    def decodeValue(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader, default: Float): Float = in.readFloat()
    def encodeValue(x: Float, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit = out.writeVal(x)
  }

  // What a number was made from (as circe's `JsonLong`, `JsonFloat`, `JsonDouble`, `JsonBigDecimal`, `JsonDecimal`).
  final private[jsoniterjson] val LongKind = 0
  final private[jsoniterjson] val FloatKind = 1
  final private[jsoniterjson] val DoubleKind = 2
  final private[jsoniterjson] val BigDecimalKind = 3
  final private[jsoniterjson] val TextKind = 4

  /** `text` must be a JSON number (what `JsonReader` or a number's `toString` produced): not validated. */
  private[jsoniterjson] def unsafeFromJsonText(text: String): JsonNumber =
    new JsonNumber(TextKind, 0L, null, text)

  private[jsoniterjson] def fromJavaBigDecimal(n: java.math.BigDecimal): JsonNumber =
    new JsonNumber(BigDecimalKind, 0L, n, null)

  def fromInt(n: Int): JsonNumber = new JsonNumber(LongKind, n.toLong, null, null)
  def fromLong(n: Long): JsonNumber = new JsonNumber(LongKind, n, null, null)
  def fromDouble(n: Double): Option[JsonNumber] =
    if (n.isNaN || n.isInfinite) None
    else Some(new JsonNumber(DoubleKind, java.lang.Double.doubleToRawLongBits(n), null, null))
  def fromFloat(n: Float): Option[JsonNumber] =
    if (n.isNaN || n.isInfinite) None
    else Some(new JsonNumber(FloatKind, java.lang.Float.floatToRawIntBits(n).toLong, null, null))
  def fromBigDecimal(n: BigDecimal): JsonNumber = fromJavaBigDecimal(n.bigDecimal)
  def fromBigInt(n: BigInt): JsonNumber = fromJavaBigDecimal(new java.math.BigDecimal(n.bigInteger))

  /** `Some` if `s` is a JSON number (`-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?`), like circe's
    * `JsonNumber.fromString`: e.g. `+1`, `1.`, `.5`, `01`, `NaN` and surrounding whitespace are rejected. The text is
    * kept as is (and written as is), also when its exponent is too large for a `BigDecimal` (then `toBigDecimal` is
    * `None` when the scale does not fit an `Int`, and `toDouble` is `0.0` or `None`).
    */
  def fromString(s: String): Option[JsonNumber] =
    if (isJsonNumber(s)) Some(unsafeFromJsonText(s)) else None

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
