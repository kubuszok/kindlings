package hearth.kindlings.sconfigderivation

/** Compile-time configuration knobs that drive kindlings sconfig derivation.
  *
  * Defaults match PureConfig's `ProductHint` defaults exactly so users can move between
  * `kindlings-pureconfig-derivation` and `kindlings-sconfig-derivation` without rethinking
  * naming or strictness:
  *
  *  - `transformMemberNames = ConfigFieldMapping(CamelCase, KebabCase)` — case-class
  *    fields like `myFieldName` map to HOCON keys like `my-field-name`.
  *  - `transformConstructorNames = ConfigFieldMapping(PascalCase, KebabCase)` — sealed
  *    trait subtypes like `MyVariant` map to discriminator values like `my-variant`.
  *  - `discriminator = Some("type")` — sealed traits encode as `{"type": "variant", …}`
  *    by default; set to `None` to switch to single-key wrapping (`{"variant": {…}}`).
  *  - `useDefaults = true` — fall back to a case-class field's compile-time default value
  *    when the corresponding HOCON key is missing.
  *  - `allowUnknownKeys = true` — HOCON objects may contain keys that aren't fields of
  *    the case class. Set to `false` for strict mode (fail on unknown keys).
  *
  * For per-type customization (e.g. one case class wants `SnakeCase` while everything
  * else stays kebab-case), define an implicit [[ProductHint]] / [[CoproductHint]] for
  * the specific type. The macro looks them up at derivation time and falls back to this
  * global config when no per-type hint is in scope.
  */
final case class SConfig(
    transformMemberNames: String => String = ConfigFieldMapping(CamelCase, KebabCase),
    transformConstructorNames: String => String = ConfigFieldMapping(PascalCase, KebabCase),
    discriminator: Option[String] = Some("type"),
    useDefaults: Boolean = true,
    allowUnknownKeys: Boolean = true
) {

  def withTransformMemberNames(f: String => String): SConfig = copy(transformMemberNames = f)
  def withTransformConstructorNames(f: String => String): SConfig = copy(transformConstructorNames = f)
  def withSnakeCaseMemberNames: SConfig = copy(transformMemberNames = ConfigFieldMapping(CamelCase, SnakeCase))
  def withKebabCaseMemberNames: SConfig = copy(transformMemberNames = ConfigFieldMapping(CamelCase, KebabCase))
  def withPascalCaseMemberNames: SConfig = copy(transformMemberNames = ConfigFieldMapping(CamelCase, PascalCase))
  def withScreamingSnakeCaseMemberNames: SConfig =
    copy(transformMemberNames = ConfigFieldMapping(CamelCase, ScreamingSnakeCase))
  def withCamelCaseMemberNames: SConfig = copy(transformMemberNames = ConfigFieldMapping(CamelCase, CamelCase))
  def withSnakeCaseConstructorNames: SConfig =
    copy(transformConstructorNames = ConfigFieldMapping(PascalCase, SnakeCase))
  def withKebabCaseConstructorNames: SConfig =
    copy(transformConstructorNames = ConfigFieldMapping(PascalCase, KebabCase))
  def withDiscriminator(field: String): SConfig = copy(discriminator = Some(field))
  def withWrappedSubtypes: SConfig = copy(discriminator = None)
  def withUseDefaults: SConfig = copy(useDefaults = true)
  def withoutUseDefaults: SConfig = copy(useDefaults = false)
  def withAllowUnknownKeys: SConfig = copy(allowUnknownKeys = true)
  def withStrictDecoding: SConfig = copy(allowUnknownKeys = false)
}
object SConfig {

  implicit val default: SConfig = SConfig()
}
