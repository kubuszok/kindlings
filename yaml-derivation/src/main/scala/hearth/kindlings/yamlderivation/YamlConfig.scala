package hearth.kindlings.yamlderivation

final case class YamlConfig(
    transformMemberNames: String => String = identity,
    transformConstructorNames: String => String = identity,
    discriminator: Option[String] = None,
    enumAsStrings: Boolean = false
) {

  def withTransformMemberNames(f: String => String): YamlConfig = copy(transformMemberNames = f)
  def withTransformConstructorNames(f: String => String): YamlConfig = copy(transformConstructorNames = f)
  def withSnakeCaseMemberNames: YamlConfig = copy(transformMemberNames = YamlConfig.snakeCase)
  def withKebabCaseMemberNames: YamlConfig = copy(transformMemberNames = YamlConfig.kebabCase)
  def withPascalCaseMemberNames: YamlConfig = copy(transformMemberNames = YamlConfig.pascalCase)
  def withScreamingSnakeCaseMemberNames: YamlConfig =
    copy(transformMemberNames = YamlConfig.screamingSnakeCase)
  def withSnakeCaseConstructorNames: YamlConfig = copy(transformConstructorNames = YamlConfig.snakeCase)
  def withKebabCaseConstructorNames: YamlConfig = copy(transformConstructorNames = YamlConfig.kebabCase)
  def withDiscriminator(field: String): YamlConfig = copy(discriminator = Some(field))
  def withEnumAsStrings: YamlConfig = copy(enumAsStrings = true)
}
object YamlConfig {

  implicit val default: YamlConfig = YamlConfig()

  private[yamlderivation] val snakeCase: String => String = { s =>
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

  private[yamlderivation] val kebabCase: String => String = { s =>
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

  private[yamlderivation] val pascalCase: String => String = { s =>
    if (s.isEmpty) s
    else s.charAt(0).toUpper.toString + s.substring(1)
  }

  private[yamlderivation] val screamingSnakeCase: String => String = { s =>
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
