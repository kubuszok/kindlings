package hearth.kindlings.jsoniterjson

/** String-backed number representation to preserve precision.
  *
  * Numbers are stored as their string representation to avoid precision loss that can occur with Double/Float
  * conversions. Conversion methods are provided to extract values in specific numeric types.
  */
final case class JsonNumber private (value: String) {

  def toInt: Option[Int] = try Some(value.toInt)
  catch { case _: NumberFormatException => None }

  def toLong: Option[Long] = try Some(value.toLong)
  catch { case _: NumberFormatException => None }

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

  def toBigInt: Option[BigInt] = try Some(BigInt(value))
  catch { case _: NumberFormatException => None }

  override def toString: String = value
}
object JsonNumber {

  def fromInt(n: Int): JsonNumber = new JsonNumber(n.toString)
  def fromLong(n: Long): JsonNumber = new JsonNumber(n.toString)
  def fromDouble(n: Double): Option[JsonNumber] =
    if (n.isNaN || n.isInfinite) None else Some(new JsonNumber(n.toString))
  def fromFloat(n: Float): Option[JsonNumber] =
    if (n.isNaN || n.isInfinite) None else Some(new JsonNumber(n.toString))
  def fromBigDecimal(n: BigDecimal): JsonNumber = new JsonNumber(n.toString)
  def fromBigInt(n: BigInt): JsonNumber = new JsonNumber(n.toString)
  def fromString(s: String): Option[JsonNumber] = try {
    val _ = BigDecimal(s) // validate
    Some(new JsonNumber(s))
  } catch { case _: NumberFormatException => None }
}
