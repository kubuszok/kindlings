package hearth.kindlings.xmlderivation

final case class XmlConfig(
    defaultFieldMode: XmlFieldMode = XmlFieldMode.Element,
    fieldNameMapper: String => String = identity,
    constructorNameMapper: String => String = identity,
    discriminatorAttribute: Option[String] = Some("type"),
    enumAsStrings: Boolean = false,
    useDefaults: Boolean = false,
    transientNone: Boolean = false,
    transientEmpty: Boolean = false
) {

  def withDefaultFieldMode(mode: XmlFieldMode): XmlConfig = copy(defaultFieldMode = mode)
  def withAttributesByDefault: XmlConfig = copy(defaultFieldMode = XmlFieldMode.Attribute)
  def withElementsByDefault: XmlConfig = copy(defaultFieldMode = XmlFieldMode.Element)
  def withFieldNameMapper(f: String => String): XmlConfig = copy(fieldNameMapper = f)
  def withConstructorNameMapper(f: String => String): XmlConfig = copy(constructorNameMapper = f)
  def withSnakeCaseFieldNames: XmlConfig = copy(fieldNameMapper = XmlConfig.snakeCase)
  def withKebabCaseFieldNames: XmlConfig = copy(fieldNameMapper = XmlConfig.kebabCase)
  def withPascalCaseFieldNames: XmlConfig = copy(fieldNameMapper = XmlConfig.pascalCase)
  def withScreamingSnakeCaseFieldNames: XmlConfig =
    copy(fieldNameMapper = XmlConfig.screamingSnakeCase)
  def withSnakeCaseConstructorNames: XmlConfig = copy(constructorNameMapper = XmlConfig.snakeCase)
  def withKebabCaseConstructorNames: XmlConfig = copy(constructorNameMapper = XmlConfig.kebabCase)
  def withDiscriminator(attr: String): XmlConfig = copy(discriminatorAttribute = Some(attr))
  def withNoDiscriminator: XmlConfig = copy(discriminatorAttribute = None)
  def withEnumAsStrings: XmlConfig = copy(enumAsStrings = true)
  def withUseDefaults: XmlConfig = copy(useDefaults = true)
  def withTransientNone: XmlConfig = copy(transientNone = true)
  def withTransientEmpty: XmlConfig = copy(transientEmpty = true)
}
object XmlConfig {

  implicit val default: XmlConfig = XmlConfig()

  private[xmlderivation] val snakeCase: String => String = { s =>
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

  private[xmlderivation] val kebabCase: String => String = { s =>
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

  private[xmlderivation] val pascalCase: String => String = { s =>
    if (s.isEmpty) s
    else s.charAt(0).toUpper.toString + s.substring(1)
  }

  private[xmlderivation] val screamingSnakeCase: String => String = { s =>
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
