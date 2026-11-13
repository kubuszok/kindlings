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

  // Written out by hand (it is what the compiler would synthesize): Scala 2 does not synthesize a case class `copy`
  // when the class declares any other `copy`, and the deprecated overload below is needed for binary compatibility.
  def copy(
      transformMemberNames: String => String = this.transformMemberNames,
      transformConstructorNames: String => String = this.transformConstructorNames,
      useDefaults: Boolean = this.useDefaults,
      discriminator: Option[String] = this.discriminator,
      strictDecoding: Boolean = this.strictDecoding,
      enumAsStrings: Boolean = this.enumAsStrings,
      unwrapValueClasses: Boolean = this.unwrapValueClasses
  ): Configuration = new Configuration(
    transformMemberNames,
    transformConstructorNames,
    useDefaults,
    discriminator,
    strictDecoding,
    enumAsStrings,
    unwrapValueClasses
  )

  /** The `copy` of Kindlings 0.3.2 (without `unwrapValueClasses`, which keeps its value). */
  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; use copy with named arguments",
    "0.3.4"
  )
  def copy(
      transformMemberNames: String => String,
      transformConstructorNames: String => String,
      useDefaults: Boolean,
      discriminator: Option[String],
      strictDecoding: Boolean,
      enumAsStrings: Boolean
  ): Configuration = new Configuration(
    transformMemberNames,
    transformConstructorNames,
    useDefaults,
    discriminator,
    strictDecoding,
    enumAsStrings,
    this.unwrapValueClasses
  )

  /** The constructor of Kindlings 0.3.2 (without `unwrapValueClasses`, which gets its default). */
  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; use the primary constructor",
    "0.3.4"
  )
  def this(
      transformMemberNames: String => String,
      transformConstructorNames: String => String,
      useDefaults: Boolean,
      discriminator: Option[String],
      strictDecoding: Boolean,
      enumAsStrings: Boolean
  ) = this(
    transformMemberNames,
    transformConstructorNames,
    useDefaults,
    discriminator,
    strictDecoding,
    enumAsStrings,
    true
  )
}
object Configuration {

  implicit val default: Configuration = Configuration()

  /** The `apply` of Kindlings 0.3.2 (without `unwrapValueClasses`, which gets its default). */
  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; use apply with named arguments",
    "0.3.4"
  )
  def apply(
      transformMemberNames: String => String,
      transformConstructorNames: String => String,
      useDefaults: Boolean,
      discriminator: Option[String],
      strictDecoding: Boolean,
      enumAsStrings: Boolean
  ): Configuration =
    new Configuration(
      transformMemberNames,
      transformConstructorNames,
      useDefaults,
      discriminator,
      strictDecoding,
      enumAsStrings
    )

  // Kept for source compatibility; the public versions live in `renaming`.
  private[circederivation] val snakeCase: String => String = renaming.snakeCase
  private[circederivation] val kebabCase: String => String = renaming.kebabCase
  private[circederivation] val pascalCase: String => String = renaming.pascalCase
  private[circederivation] val screamingSnakeCase: String => String = renaming.screamingSnakeCase
}
