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
  def withPascalCaseConstructorNames: Configuration = copy(transformConstructorNames = Configuration.pascalCase)
  def withScreamingSnakeCaseConstructorNames: Configuration =
    copy(transformConstructorNames = Configuration.screamingSnakeCase)
  def withDefaults: Configuration = copy(useDefaults = true)
  def withoutDefaults: Configuration = copy(useDefaults = false)
  def withDiscriminator(field: String): Configuration = copy(discriminator = Some(field))
  def withoutDiscriminator: Configuration = copy(discriminator = None)
  def withStrictDecoding: Configuration = copy(strictDecoding = true)
  def withoutStrictDecoding: Configuration = copy(strictDecoding = false)
  def withEnumAsStrings: Configuration = copy(enumAsStrings = true)
}
object Configuration {

  implicit val default: Configuration = Configuration()

  // Regex patterns matching upstream circe's case transformation algorithm.
  // Handles consecutive capitals correctly: HTMLParser → html_parser (not h_t_m_l_parser)
  private val basePattern = java.util.regex.Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val swapPattern = java.util.regex.Pattern.compile("([a-z\\d])([A-Z])")

  private def separateWords(s: String, separator: Char): String = {
    val partial = basePattern.matcher(s).replaceAll("$1" + separator + "$2")
    swapPattern.matcher(partial).replaceAll("$1" + separator + "$2")
  }

  private[circederivation] val snakeCase: String => String = { s =>
    separateWords(s, '_').toLowerCase
  }

  private[circederivation] val kebabCase: String => String = { s =>
    separateWords(s, '-').toLowerCase
  }

  private[circederivation] val pascalCase: String => String = { s =>
    if (s.isEmpty) s
    else s.charAt(0).toUpper.toString + s.substring(1)
  }

  private[circederivation] val screamingSnakeCase: String => String = { s =>
    separateWords(s, '_').toUpperCase
  }
}
