package hearth.kindlings.ubjsonderivation

final case class UBJsonConfig(
    fieldNameMapper: String => String = identity,
    adtLeafClassNameMapper: String => String = identity,
    discriminatorFieldName: Option[String] = None,
    skipUnexpectedFields: Boolean = true,
    enumAsStrings: Boolean = false,
    transientDefault: Boolean = false,
    transientEmpty: Boolean = false,
    transientNone: Boolean = false,
    requireCollectionFields: Boolean = false,
    requireDefaultFields: Boolean = false,
    checkFieldDuplication: Boolean = false,
    bigDecimalPrecision: Int = 34,
    bigDecimalScaleLimit: Int = 6178,
    bigDecimalDigitsLimit: Int = 308,
    mapMaxInsertNumber: Int = Int.MaxValue,
    setMaxInsertNumber: Int = Int.MaxValue
) {

  def withFieldNameMapper(f: String => String): UBJsonConfig = copy(fieldNameMapper = f)
  def withAdtLeafClassNameMapper(f: String => String): UBJsonConfig = copy(adtLeafClassNameMapper = f)
  def withSnakeCaseFieldNames: UBJsonConfig = copy(fieldNameMapper = UBJsonConfig.snakeCase)
  def withKebabCaseFieldNames: UBJsonConfig = copy(fieldNameMapper = UBJsonConfig.kebabCase)
  def withPascalCaseFieldNames: UBJsonConfig = copy(fieldNameMapper = UBJsonConfig.pascalCase)
  def withScreamingSnakeCaseFieldNames: UBJsonConfig =
    copy(fieldNameMapper = UBJsonConfig.screamingSnakeCase)
  def withSnakeCaseAdtLeafClassNames: UBJsonConfig =
    copy(adtLeafClassNameMapper = UBJsonConfig.snakeCase)
  def withKebabCaseAdtLeafClassNames: UBJsonConfig =
    copy(adtLeafClassNameMapper = UBJsonConfig.kebabCase)
  def withDiscriminator(field: String): UBJsonConfig = copy(discriminatorFieldName = Some(field))
  def withSkipUnexpectedFields(skip: Boolean): UBJsonConfig = copy(skipUnexpectedFields = skip)
  def withEnumAsStrings: UBJsonConfig = copy(enumAsStrings = true)
  def withTransientDefault: UBJsonConfig = copy(transientDefault = true)
  def withTransientEmpty: UBJsonConfig = copy(transientEmpty = true)
  def withTransientNone: UBJsonConfig = copy(transientNone = true)
  def withRequireCollectionFields: UBJsonConfig = copy(requireCollectionFields = true)
  def withRequireDefaultFields: UBJsonConfig = copy(requireDefaultFields = true)
  def withCheckFieldDuplication: UBJsonConfig = copy(checkFieldDuplication = true)
  def withBigDecimalPrecision(precision: Int): UBJsonConfig = copy(bigDecimalPrecision = precision)
  def withBigDecimalScaleLimit(scaleLimit: Int): UBJsonConfig =
    copy(bigDecimalScaleLimit = scaleLimit)
  def withBigDecimalDigitsLimit(digitsLimit: Int): UBJsonConfig =
    copy(bigDecimalDigitsLimit = digitsLimit)
  def withMapMaxInsertNumber(max: Int): UBJsonConfig = copy(mapMaxInsertNumber = max)
  def withSetMaxInsertNumber(max: Int): UBJsonConfig = copy(setMaxInsertNumber = max)
}
object UBJsonConfig {

  implicit val default: UBJsonConfig = UBJsonConfig()

  private[ubjsonderivation] val snakeCase: String => String = { s =>
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

  private[ubjsonderivation] val kebabCase: String => String = { s =>
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

  private[ubjsonderivation] val pascalCase: String => String = { s =>
    if (s.isEmpty) s
    else s.charAt(0).toUpper.toString + s.substring(1)
  }

  private[ubjsonderivation] val screamingSnakeCase: String => String = { s =>
    val sb = new StringBuilder
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c.isUpper) {
        if (i > 0) sb.append('_')
        sb.append(c)
      } else sb.append(c.toUpper)
      i += 1
    }
    sb.toString
  }
}
