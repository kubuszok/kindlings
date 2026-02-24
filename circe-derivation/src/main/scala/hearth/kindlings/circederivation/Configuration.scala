package hearth.kindlings.circederivation

final case class Configuration(
    transformMemberNames: String => String = identity,
    transformConstructorNames: String => String = identity,
    useDefaults: Boolean = false,
    discriminator: Option[String] = None,
    strictDecoding: Boolean = false,
    enumAsStrings: Boolean = false
) {

  def withTransformMemberNames(f: String => String): Configuration = copy(transformMemberNames = f)
  def withTransformConstructorNames(f: String => String): Configuration = copy(transformConstructorNames = f)
  def withSnakeCaseMemberNames: Configuration = copy(transformMemberNames = Configuration.snakeCase)
  def withKebabCaseMemberNames: Configuration = copy(transformMemberNames = Configuration.kebabCase)
  def withPascalCaseMemberNames: Configuration = copy(transformMemberNames = Configuration.pascalCase)
  def withScreamingSnakeCaseMemberNames: Configuration =
    copy(transformMemberNames = Configuration.screamingSnakeCase)
  def withSnakeCaseConstructorNames: Configuration = copy(transformConstructorNames = Configuration.snakeCase)
  def withKebabCaseConstructorNames: Configuration = copy(transformConstructorNames = Configuration.kebabCase)
  def withDefaults: Configuration = copy(useDefaults = true)
  def withDiscriminator(field: String): Configuration = copy(discriminator = Some(field))
  def withStrictDecoding: Configuration = copy(strictDecoding = true)
  def withEnumAsStrings: Configuration = copy(enumAsStrings = true)
}
object Configuration {

  implicit val default: Configuration = Configuration()

  private[circederivation] val snakeCase: String => String = { s =>
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

  private[circederivation] val kebabCase: String => String = { s =>
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

  private[circederivation] val pascalCase: String => String = { s =>
    if (s.isEmpty) s
    else s.charAt(0).toUpper.toString + s.substring(1)
  }

  private[circederivation] val screamingSnakeCase: String => String = { s =>
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
