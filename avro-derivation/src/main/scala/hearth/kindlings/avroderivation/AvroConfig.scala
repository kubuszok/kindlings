package hearth.kindlings.avroderivation

/** The `decimal` logical type `BigDecimal` is encoded as (BYTES).
  *
  * A value with more decimal places than `scale` is rounded with `roundingMode` (as avro4s 4's `RoundingMode`); the
  * default, `UNNECESSARY`, rejects such a value with an `ArithmeticException` instead of rounding it.
  */
final case class DecimalConfig(precision: Int, scale: Int) {

  // Not a constructor parameter: adding one would change the binary signatures of this case class (`apply`, `copy`,
  // `unapply`) that code compiled against earlier versions links to.
  private var rounding: java.math.RoundingMode = java.math.RoundingMode.UNNECESSARY

  /** How a value with more decimal places than `scale` is rounded (default: `UNNECESSARY`, i.e. rejected). */
  def roundingMode: java.math.RoundingMode = rounding

  def withRoundingMode(roundingMode: java.math.RoundingMode): DecimalConfig = {
    val updated = new DecimalConfig(precision, scale)
    updated.rounding = roundingMode
    updated
  }

  def copy(precision: Int = this.precision, scale: Int = this.scale): DecimalConfig =
    new DecimalConfig(precision, scale).withRoundingMode(rounding)

  override def equals(other: Any): Boolean = other match {
    case that: DecimalConfig => precision == that.precision && scale == that.scale && rounding == that.rounding
    case _                   => false
  }
  override def hashCode: Int = (precision, scale, rounding).hashCode
  override def toString: String =
    if (rounding == java.math.RoundingMode.UNNECESSARY) s"DecimalConfig($precision,$scale)"
    else s"DecimalConfig($precision,$scale,$rounding)"
}

/** The precision of a timestamp encoded as an Avro `long` (the `timestamp-millis`, `timestamp-micros` or
  * `timestamp-nanos` logical type).
  */
sealed abstract class TimestampPrecision extends Product with Serializable
object TimestampPrecision {
  case object Millis extends TimestampPrecision
  case object Micros extends TimestampPrecision
  case object Nanos extends TimestampPrecision
}

/** Derivation configuration.
  *
  * Defaults match avro4s except where `docs/user-guide/feature-parity.md` documents a difference: `BigDecimal` is a
  * STRING (avro4s: `decimal(8, 2)`) and `OffsetDateTime` is a `timestamp-millis` long (avro4s: an ISO-8601 string).
  * [[AvroConfig.avro4sDefaults]] switches both to avro4s' behaviour.
  *
  * @param instantPrecision
  *   precision of `java.time.Instant` and `java.sql.Timestamp` (as avro4s: `Millis`)
  * @param localDateTimePrecision
  *   precision of `java.time.LocalDateTime` (as avro4s: `Nanos`)
  * @param offsetDateTimeAsIsoString
  *   encode `java.time.OffsetDateTime` as avro4s does: an ISO-8601 string with the `datetime-with-offset` logical type
  *   (default: a UTC `timestamp-millis` long, which drops the offset)
  * @param scalaDefaultsInSchema
  *   turn case class fields' Scala default values into Avro schema defaults, as avro4s does (default: on;
  *   `@avroDefault` and `@avroNoDefault` take precedence; [[withoutScalaDefaultsInSchema]] turns it off)
  */
final case class AvroConfig(
    namespace: Option[String] = None,
    transformFieldNames: String => String = identity,
    transformConstructorNames: String => String = identity,
    decimalConfig: Option[DecimalConfig] = None,
    instantPrecision: TimestampPrecision = TimestampPrecision.Millis,
    localDateTimePrecision: TimestampPrecision = TimestampPrecision.Nanos,
    offsetDateTimeAsIsoString: Boolean = false,
    scalaDefaultsInSchema: Boolean = true
) {

  def withNamespace(ns: String): AvroConfig = copy(namespace = Some(ns))
  def withTransformFieldNames(f: String => String): AvroConfig = copy(transformFieldNames = f)
  def withTransformConstructorNames(f: String => String): AvroConfig = copy(transformConstructorNames = f)
  def withSnakeCaseFieldNames: AvroConfig = copy(transformFieldNames = AvroConfig.snakeCase)
  def withKebabCaseFieldNames: AvroConfig = copy(transformFieldNames = AvroConfig.kebabCase)
  def withPascalCaseFieldNames: AvroConfig = copy(transformFieldNames = AvroConfig.pascalCase)
  def withDecimalConfig(precision: Int, scale: Int): AvroConfig =
    copy(decimalConfig = Some(DecimalConfig(precision, scale)))

  /** `BigDecimal` as a `decimal(precision, scale)`, rounding values with more decimal places than `scale` with
    * `roundingMode` (as avro4s 4's implicit `RoundingMode`) instead of rejecting them. The rounding mode also applies
    * to the fields annotated with `@avroScalePrecision`.
    */
  def withDecimalConfig(precision: Int, scale: Int, roundingMode: java.math.RoundingMode): AvroConfig =
    copy(decimalConfig = Some(DecimalConfig(precision, scale).withRoundingMode(roundingMode)))

  /** Precision of `java.time.Instant` and `java.sql.Timestamp` (default: `Millis`, as avro4s). */
  def withInstantPrecision(precision: TimestampPrecision): AvroConfig = copy(instantPrecision = precision)

  /** Precision of `java.time.LocalDateTime` (default: `Nanos`, as avro4s). */
  def withLocalDateTimePrecision(precision: TimestampPrecision): AvroConfig = copy(localDateTimePrecision = precision)

  /** Encode `java.time.OffsetDateTime` as avro4s does: an ISO-8601 string (`datetime-with-offset` logical type). */
  def withOffsetDateTimeAsIsoString: AvroConfig = copy(offsetDateTimeAsIsoString = true)

  /** Turn Scala default values of case class fields into Avro schema defaults, as avro4s does. */
  def withScalaDefaultsInSchema: AvroConfig = copy(scalaDefaultsInSchema = true)

  /** Whether Scala default values of case class fields become Avro schema defaults (default: `true`, as avro4s):
    * `withScalaDefaultsInSchema(false)` is [[withoutScalaDefaultsInSchema]].
    */
  def withScalaDefaultsInSchema(enabled: Boolean): AvroConfig = copy(scalaDefaultsInSchema = enabled)

  /** Only `@avroDefault` becomes a schema default; Scala default values are used only when decoding. */
  def withoutScalaDefaultsInSchema: AvroConfig = copy(scalaDefaultsInSchema = false)
}
object AvroConfig {

  implicit val default: AvroConfig = AvroConfig()

  /** avro4s' behaviour where Kindlings' defaults differ (see the class documentation): `BigDecimal` as
    * `decimal(precision = 8, scale = 2)` and `OffsetDateTime` as an ISO-8601 string.
    */
  def avro4sDefaults: AvroConfig = AvroConfig(
    decimalConfig = Some(DecimalConfig(precision = 8, scale = 2)),
    offsetDateTimeAsIsoString = true
  )

  private[avroderivation] val snakeCase: String => String = { s =>
    val sb = new StringBuilder
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c.isUpper) {
        if (i > 0) sb.append('_')
        sb.append(c.toLower)
      } else sb.append(c)
      i += 1
    }
    sb.toString
  }

  private[avroderivation] val kebabCase: String => String = { s =>
    val sb = new StringBuilder
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c.isUpper) {
        if (i > 0) sb.append('-')
        sb.append(c.toLower)
      } else sb.append(c)
      i += 1
    }
    sb.toString
  }

  private[avroderivation] val pascalCase: String => String = { s =>
    if (s.isEmpty) s
    else s.charAt(0).toUpper.toString + s.substring(1)
  }
}
