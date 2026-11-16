package hearth.kindlings.circederivation

final case class Configuration(
    transformMemberNames: String => String = identity,
    transformConstructorNames: String => String = identity,
    useDefaults: Boolean = false,
    discriminator: Option[String] = None,
    strictDecoding: Boolean = false,
    enumAsStrings: Boolean = false,
    unwrapValueClasses: Boolean = true
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
  def withoutEnumAsStrings: Configuration = copy(enumAsStrings = false)

  /** `AnyVal` case classes are coded as their single field's value (the Kindlings default). */
  def withValueClassUnwrapping: Configuration = copy(unwrapValueClasses = true)

  /** `AnyVal` case classes are coded as JSON objects, like any other case class, as upstream circe (Scala 3
    * `ConfiguredCodec`, circe-generic and circe-generic-extras) does: `{"value":5}` instead of `5`.
    */
  def withoutValueClassUnwrapping: Configuration = copy(unwrapValueClasses = false)
}
object Configuration {

  implicit val default: Configuration = Configuration()

  // Kept for source compatibility; the public versions live in `renaming`.
  private[circederivation] val snakeCase: String => String = renaming.snakeCase
  private[circederivation] val kebabCase: String => String = renaming.kebabCase
  private[circederivation] val pascalCase: String => String = renaming.pascalCase
  private[circederivation] val screamingSnakeCase: String => String = renaming.screamingSnakeCase
}
