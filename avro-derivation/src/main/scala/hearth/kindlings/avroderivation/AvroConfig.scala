package hearth.kindlings.avroderivation

final case class DecimalConfig(precision: Int, scale: Int)

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

  /** Precision of `java.time.Instant` and `java.sql.Timestamp` (default: `Millis`, as avro4s). */
  def withInstantPrecision(precision: TimestampPrecision): AvroConfig = copy(instantPrecision = precision)

  /** Precision of `java.time.LocalDateTime` (default: `Nanos`, as avro4s). */
  def withLocalDateTimePrecision(precision: TimestampPrecision): AvroConfig = copy(localDateTimePrecision = precision)

  /** Encode `java.time.OffsetDateTime` as avro4s does: an ISO-8601 string (`datetime-with-offset` logical type). */
  def withOffsetDateTimeAsIsoString: AvroConfig = copy(offsetDateTimeAsIsoString = true)

  /** Turn Scala default values of case class fields into Avro schema defaults, as avro4s does. */
  def withScalaDefaultsInSchema: AvroConfig = copy(scalaDefaultsInSchema = true)

  /** Only `@avroDefault` becomes a schema default; Scala default values are used only when decoding. */
  def withoutScalaDefaultsInSchema: AvroConfig = copy(scalaDefaultsInSchema = false)

  // Written out by hand (it is what the compiler would synthesize): Scala 2 does not synthesize a case class `copy`
  // when the class declares any other `copy`, and the deprecated overload below is needed for binary compatibility.
  def copy(
      namespace: Option[String] = this.namespace,
      transformFieldNames: String => String = this.transformFieldNames,
      transformConstructorNames: String => String = this.transformConstructorNames,
      decimalConfig: Option[DecimalConfig] = this.decimalConfig,
      instantPrecision: TimestampPrecision = this.instantPrecision,
      localDateTimePrecision: TimestampPrecision = this.localDateTimePrecision,
      offsetDateTimeAsIsoString: Boolean = this.offsetDateTimeAsIsoString,
      scalaDefaultsInSchema: Boolean = this.scalaDefaultsInSchema
  ): AvroConfig = new AvroConfig(
    namespace,
    transformFieldNames,
    transformConstructorNames,
    decimalConfig,
    instantPrecision,
    localDateTimePrecision,
    offsetDateTimeAsIsoString,
    scalaDefaultsInSchema
  )

  /** The `copy` of Kindlings 0.3.x (without the fields added since); the other fields keep their values. */
  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; use copy with named arguments",
    "0.3.5"
  )
  def copy(
      namespace: Option[String],
      transformFieldNames: String => String,
      transformConstructorNames: String => String,
      decimalConfig: Option[DecimalConfig]
  ): AvroConfig = new AvroConfig(
    namespace,
    transformFieldNames,
    transformConstructorNames,
    decimalConfig,
    instantPrecision,
    localDateTimePrecision,
    offsetDateTimeAsIsoString,
    scalaDefaultsInSchema
  )

  /** The constructor of Kindlings 0.3.x (without the fields added since, which get their defaults). */
  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; use the primary constructor",
    "0.3.5"
  )
  def this(
      namespace: Option[String],
      transformFieldNames: String => String,
      transformConstructorNames: String => String,
      decimalConfig: Option[DecimalConfig]
  ) = this(
    namespace,
    transformFieldNames,
    transformConstructorNames,
    decimalConfig,
    TimestampPrecision.Millis,
    TimestampPrecision.Nanos,
    false,
    true
  )
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

  /** The `apply` of Kindlings 0.3.x (without the fields added since, which get their defaults). */
  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; use apply with named arguments",
    "0.3.5"
  )
  def apply(
      namespace: Option[String],
      transformFieldNames: String => String,
      transformConstructorNames: String => String,
      decimalConfig: Option[DecimalConfig]
  ): AvroConfig = new AvroConfig(namespace, transformFieldNames, transformConstructorNames, decimalConfig)

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
