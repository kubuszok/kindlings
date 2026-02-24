package hearth.kindlings.avroderivation

final case class AvroConfig(
    namespace: Option[String] = None,
    transformFieldNames: String => String = identity,
    transformConstructorNames: String => String = identity
) {

  def withNamespace(ns: String): AvroConfig = copy(namespace = Some(ns))
  def withTransformFieldNames(f: String => String): AvroConfig = copy(transformFieldNames = f)
  def withTransformConstructorNames(f: String => String): AvroConfig = copy(transformConstructorNames = f)
  def withSnakeCaseFieldNames: AvroConfig = copy(transformFieldNames = AvroConfig.snakeCase)
  def withKebabCaseFieldNames: AvroConfig = copy(transformFieldNames = AvroConfig.kebabCase)
}
object AvroConfig {

  implicit val default: AvroConfig = AvroConfig()

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
}
