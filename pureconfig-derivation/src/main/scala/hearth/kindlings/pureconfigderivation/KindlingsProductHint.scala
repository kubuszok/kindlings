package hearth.kindlings.pureconfigderivation

import pureconfig.{CamelCase, ConfigFieldMapping, KebabCase}

/** Per-type override of the global [[PureConfig]] config. The macro looks for an implicit `KindlingsProductHint[A]` for
  * every case class it derives; when present, it overrides the global config's `transformMemberNames` / `useDefaults` /
  * `allowUnknownKeys` for that specific type. Annotations (`@configKey`, `@transientField`) always win.
  *
  * Defaults match upstream `pureconfig.generic.ProductHint[A]`:
  *
  *   - `transformMemberNames = ConfigFieldMapping(CamelCase, KebabCase)`
  *   - `useDefaults = true`
  *   - `allowUnknownKeys = true`
  *
  * Example: read `MyType`'s fields as `snake_case` while everything else uses the global config:
  * {{{
  *   import pureconfig.{CamelCase, ConfigFieldMapping, SnakeCase}
  *   implicit val myTypeHint: KindlingsProductHint[MyType] =
  *     KindlingsProductHint[MyType](transformMemberNames = ConfigFieldMapping(CamelCase, SnakeCase))
  * }}}
  *
  * Note: this trait does NOT have an implicit `default[A]` — the absence of a hint is meaningful and tells the macro to
  * fall back to the global [[PureConfig]] config. This is a deliberate design difference from upstream PureConfig
  * (which always provides a default `ProductHint`) and lets users opt out of per-type customisation entirely by just
  * relying on the global config.
  */
final case class KindlingsProductHint[A](
    transformMemberNames: String => String,
    useDefaults: Boolean,
    allowUnknownKeys: Boolean
)
object KindlingsProductHint {

  def apply[A](
      transformMemberNames: String => String = ConfigFieldMapping(CamelCase, KebabCase),
      useDefaults: Boolean = true,
      allowUnknownKeys: Boolean = true
  ): KindlingsProductHint[A] =
    new KindlingsProductHint[A](transformMemberNames, useDefaults, allowUnknownKeys)

  /** Bridge from upstream `pureconfig.generic.ProductHint[A]` for users migrating from existing PureConfig hint
    * definitions. Note that PureConfig's `ProductHint` is an abstract class with three method-shaped operations
    * (`from`/`bottom`/`to`); this bridge only captures the configuration parameters of the default impl. Custom
    * `ProductHint` subclasses with non-default `from`/`bottom`/`to` semantics cannot be losslessly converted — see the
    * `Not ported` section of `FEATURE_PARITY.md`.
    */
  def fromPureConfigDefault[A](
      transformMemberNames: String => String = ConfigFieldMapping(CamelCase, KebabCase),
      useDefaultArgs: Boolean = true,
      allowUnknownKeys: Boolean = true
  ): KindlingsProductHint[A] =
    KindlingsProductHint[A](transformMemberNames, useDefaultArgs, allowUnknownKeys)
}
