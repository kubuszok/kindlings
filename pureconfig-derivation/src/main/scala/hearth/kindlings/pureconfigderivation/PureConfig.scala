package hearth.kindlings.pureconfigderivation

import pureconfig.{CamelCase, ConfigFieldMapping, KebabCase, PascalCase, ScreamingSnakeCase, SnakeCase}

/** Compile-time configuration knobs that drive kindlings PureConfig derivation.
  *
  * Defaults match upstream `pureconfig.generic.ProductHint` exactly so users moving between PureConfig's own derivation
  * and kindlings get identical default behaviour:
  *
  *   - `transformMemberNames = ConfigFieldMapping(CamelCase, KebabCase)` — case-class fields like `myFieldName` map to
  *     HOCON keys like `my-field-name`.
  *   - `transformConstructorNames = ConfigFieldMapping(PascalCase, KebabCase)` — sealed trait subtypes like `MyVariant`
  *     map to discriminator values like `my-variant` (matching upstream `FieldCoproductHint.defaultMapping`).
  *   - `discriminator = Some("type")` — sealed traits encode as `{"type": "variant", …}` by default; set to `None` to
  *     switch to single-key wrapping (`{"variant": {…}}`).
  *   - `useDefaults = true` — fall back to a case-class field's compile-time default value when the corresponding HOCON
  *     key is missing (matching upstream `useDefaultArgs = true`).
  *   - `allowUnknownKeys = true` — HOCON objects may contain keys that aren't fields of the case class. Set to `false`
  *     for strict mode (fail on unknown keys), matching upstream `allowUnknownKeys = false`.
  *
  * For per-type customization (e.g. one case class wants `SnakeCase` while everything else stays kebab-case), define an
  * implicit [[KindlingsProductHint]] / [[KindlingsCoproductHint]] for the specific type. The macro looks them up at
  * derivation time and falls back to this global config when no per-type hint is in scope.
  */
final case class PureConfig(
    transformMemberNames: String => String = ConfigFieldMapping(CamelCase, KebabCase),
    transformConstructorNames: String => String = ConfigFieldMapping(PascalCase, KebabCase),
    discriminator: Option[String] = Some("type"),
    useDefaults: Boolean = true,
    allowUnknownKeys: Boolean = true
) {

  def withTransformMemberNames(f: String => String): PureConfig = copy(transformMemberNames = f)
  def withTransformConstructorNames(f: String => String): PureConfig = copy(transformConstructorNames = f)
  def withSnakeCaseMemberNames: PureConfig = copy(transformMemberNames = ConfigFieldMapping(CamelCase, SnakeCase))
  def withKebabCaseMemberNames: PureConfig = copy(transformMemberNames = ConfigFieldMapping(CamelCase, KebabCase))
  def withPascalCaseMemberNames: PureConfig = copy(transformMemberNames = ConfigFieldMapping(CamelCase, PascalCase))
  def withScreamingSnakeCaseMemberNames: PureConfig =
    copy(transformMemberNames = ConfigFieldMapping(CamelCase, ScreamingSnakeCase))
  def withCamelCaseMemberNames: PureConfig = copy(transformMemberNames = ConfigFieldMapping(CamelCase, CamelCase))
  def withSnakeCaseConstructorNames: PureConfig =
    copy(transformConstructorNames = ConfigFieldMapping(PascalCase, SnakeCase))
  def withKebabCaseConstructorNames: PureConfig =
    copy(transformConstructorNames = ConfigFieldMapping(PascalCase, KebabCase))
  def withDiscriminator(field: String): PureConfig = copy(discriminator = Some(field))
  def withWrappedSubtypes: PureConfig = copy(discriminator = None)
  def withUseDefaults: PureConfig = copy(useDefaults = true)
  def withoutUseDefaults: PureConfig = copy(useDefaults = false)
  def withAllowUnknownKeys: PureConfig = copy(allowUnknownKeys = true)
  def withStrictDecoding: PureConfig = copy(allowUnknownKeys = false)
}
object PureConfig {

  implicit val default: PureConfig = PureConfig()
}
