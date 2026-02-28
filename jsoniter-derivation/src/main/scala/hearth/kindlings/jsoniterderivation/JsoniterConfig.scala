package hearth.kindlings.jsoniterderivation

final case class JsoniterConfig(
    fieldNameMapper: String => String = identity,
    adtLeafClassNameMapper: String => String = identity,
    discriminatorFieldName: Option[String] = None,
    skipUnexpectedFields: Boolean = true,
    enumAsStrings: Boolean = false,
    mapAsArray: Boolean = false,
    isStringified: Boolean = false,
    decodingOnly: Boolean = false,
    encodingOnly: Boolean = false,
    circeLikeObjectEncoding: Boolean = false,
    useScalaEnumValueId: Boolean = false,
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

  def withFieldNameMapper(f: String => String): JsoniterConfig = copy(fieldNameMapper = f)
  def withAdtLeafClassNameMapper(f: String => String): JsoniterConfig = copy(adtLeafClassNameMapper = f)
  def withSnakeCaseFieldNames: JsoniterConfig = copy(fieldNameMapper = JsoniterConfig.snakeCase)
  def withKebabCaseFieldNames: JsoniterConfig = copy(fieldNameMapper = JsoniterConfig.kebabCase)
  def withPascalCaseFieldNames: JsoniterConfig = copy(fieldNameMapper = JsoniterConfig.pascalCase)
  def withScreamingSnakeCaseFieldNames: JsoniterConfig =
    copy(fieldNameMapper = JsoniterConfig.screamingSnakeCase)
  def withSnakeCaseAdtLeafClassNames: JsoniterConfig =
    copy(adtLeafClassNameMapper = JsoniterConfig.snakeCase)
  def withKebabCaseAdtLeafClassNames: JsoniterConfig =
    copy(adtLeafClassNameMapper = JsoniterConfig.kebabCase)
  def withDiscriminator(field: String): JsoniterConfig = copy(discriminatorFieldName = Some(field))
  def withSkipUnexpectedFields(skip: Boolean): JsoniterConfig = copy(skipUnexpectedFields = skip)
  def withEnumAsStrings: JsoniterConfig = copy(enumAsStrings = true)
  def withMapAsArray: JsoniterConfig = copy(mapAsArray = true)
  def withStringified: JsoniterConfig = copy(isStringified = true)
  def withDecodingOnly: JsoniterConfig = copy(decodingOnly = true)
  def withEncodingOnly: JsoniterConfig = copy(encodingOnly = true)
  def withCirceLikeObjectEncoding: JsoniterConfig = copy(circeLikeObjectEncoding = true)
  def withUseScalaEnumValueId: JsoniterConfig = copy(useScalaEnumValueId = true)
  def withTransientDefault: JsoniterConfig = copy(transientDefault = true)
  def withTransientEmpty: JsoniterConfig = copy(transientEmpty = true)
  def withTransientNone: JsoniterConfig = copy(transientNone = true)
  def withRequireCollectionFields: JsoniterConfig = copy(requireCollectionFields = true)
  def withRequireDefaultFields: JsoniterConfig = copy(requireDefaultFields = true)
  def withCheckFieldDuplication: JsoniterConfig = copy(checkFieldDuplication = true)
  def withBigDecimalPrecision(precision: Int): JsoniterConfig = copy(bigDecimalPrecision = precision)
  def withBigDecimalScaleLimit(scaleLimit: Int): JsoniterConfig =
    copy(bigDecimalScaleLimit = scaleLimit)
  def withBigDecimalDigitsLimit(digitsLimit: Int): JsoniterConfig =
    copy(bigDecimalDigitsLimit = digitsLimit)
  def withMapMaxInsertNumber(max: Int): JsoniterConfig = copy(mapMaxInsertNumber = max)
  def withSetMaxInsertNumber(max: Int): JsoniterConfig = copy(setMaxInsertNumber = max)
}
object JsoniterConfig {

  implicit val default: JsoniterConfig = JsoniterConfig()

  private[jsoniterderivation] val snakeCase: String => String = { s =>
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

  private[jsoniterderivation] val kebabCase: String => String = { s =>
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

  private[jsoniterderivation] val pascalCase: String => String = { s =>
    if (s.isEmpty) s
    else s.charAt(0).toUpper.toString + s.substring(1)
  }

  private[jsoniterderivation] val screamingSnakeCase: String => String = { s =>
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
