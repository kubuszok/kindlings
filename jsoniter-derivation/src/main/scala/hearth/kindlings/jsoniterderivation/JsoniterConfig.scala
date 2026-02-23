package hearth.kindlings.jsoniterderivation

final case class JsoniterConfig(
    fieldNameMapper: String => String = identity,
    adtLeafClassNameMapper: String => String = identity,
    discriminatorFieldName: Option[String] = None,
    skipUnexpectedFields: Boolean = true
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
