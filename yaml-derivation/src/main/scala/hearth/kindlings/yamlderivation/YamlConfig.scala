package hearth.kindlings.yamlderivation

/** Derivation configuration.
  *
  * The defaults match scala-yaml's own derivation (`derives YamlCodec`): constructor defaults fill in absent fields,
  * and the subtypes of sealed traits / enums are written without type information and decoded by trying every subtype
  * in turn (the first that decodes wins).
  *
  * @param discriminator
  *   write the subtype's name in this field of its mapping (instead of scala-yaml's untagged encoding)
  * @param enumAsStrings
  *   write the cases of an enum (a sealed family of case objects) as their names
  * @param useDefaults
  *   use constructor defaults for absent fields (default: on, as scala-yaml; [[withoutUseDefaults]] turns it off)
  * @param wrappedSubtypes
  *   without a discriminator, wrap each subtype in a single-key mapping `{ SubtypeName: ... }` (instead of scala-yaml's
  *   untagged encoding); the behaviour of Kindlings 0.3.x
  * @param emptyCollectionsAsFlow
  *   `toYamlString` prints empty sequences as `[]` and empty mappings as `{}` (also nested), so that the printed YAML
  *   decodes back (default: off - as scala-yaml's printer, an empty value, which reads back as `null`)
  */
final case class YamlConfig(
    transformMemberNames: String => String = identity,
    transformConstructorNames: String => String = identity,
    discriminator: Option[String] = None,
    enumAsStrings: Boolean = false,
    useDefaults: Boolean = true,
    wrappedSubtypes: Boolean = false,
    emptyCollectionsAsFlow: Boolean = false
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

  /** Use constructor defaults for absent fields (the default, as scala-yaml). */
  def withUseDefaults: YamlConfig = copy(useDefaults = true)

  /** Absent fields fail even when they have a constructor default (Kindlings 0.3.x's default). */
  def withoutUseDefaults: YamlConfig = copy(useDefaults = false)

  /** Without a discriminator, wrap each subtype in a single-key mapping `{ SubtypeName: ... }` (Kindlings 0.3.x's
    * default).
    */
  def withWrappedSubtypes: YamlConfig = copy(wrappedSubtypes = true)

  /** Without a discriminator, write subtypes untagged and decode by trying each subtype (the default, as scala-yaml).
    */
  def withUntaggedSubtypes: YamlConfig = copy(wrappedSubtypes = false)

  /** `toYamlString` prints empty sequences as `[]` and empty mappings as `{}`, so that the printed YAML decodes back.
    */
  def withEmptyCollectionsAsFlow: YamlConfig = copy(emptyCollectionsAsFlow = true)

  /** `toYamlString` prints empty sequences and mappings as an empty value (the default, as scala-yaml's printer). */
  def withoutEmptyCollectionsAsFlow: YamlConfig = copy(emptyCollectionsAsFlow = false)
}
object YamlConfig {

  implicit val default: YamlConfig = YamlConfig()

  /** The defaults of Kindlings 0.3.x: no constructor defaults for absent fields, subtypes wrapped in a single-key
    * mapping.
    */
  def kindlings03Defaults: YamlConfig = YamlConfig(useDefaults = false, wrappedSubtypes = true)

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
