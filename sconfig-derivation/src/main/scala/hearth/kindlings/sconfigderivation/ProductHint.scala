package hearth.kindlings.sconfigderivation

/** Per-type override of the global [[SConfig]] config. The macro looks for an implicit
  * `ProductHint[A]` for every case class it derives; when present, it overrides the
  * global config's `transformMemberNames` / `useDefaults` / `allowUnknownKeys` for that
  * specific type. Annotations (`@configKey`, `@transientField`) always win.
  *
  * Defaults match the global [[SConfig]] (which themselves match upstream PureConfig's
  * `ProductHint` defaults):
  *
  *  - `transformMemberNames = ConfigFieldMapping(CamelCase, KebabCase)`
  *  - `useDefaults = true`
  *  - `allowUnknownKeys = true`
  *
  * Example: read `MyType`'s fields as `snake_case` while everything else uses the global
  * config:
  * {{{
  *   implicit val myTypeHint: ProductHint[MyType] =
  *     ProductHint[MyType](transformMemberNames = ConfigFieldMapping(CamelCase, SnakeCase))
  * }}}
  *
  * Note: this trait does NOT have an implicit `default[A]` — the absence of a hint is
  * meaningful and tells the macro to fall back to the global [[SConfig]] config.
  */
final case class ProductHint[A](
    transformMemberNames: String => String,
    useDefaults: Boolean,
    allowUnknownKeys: Boolean
)
object ProductHint {

  def apply[A](
      transformMemberNames: String => String = ConfigFieldMapping(CamelCase, KebabCase),
      useDefaults: Boolean = true,
      allowUnknownKeys: Boolean = true
  ): ProductHint[A] =
    new ProductHint[A](transformMemberNames, useDefaults, allowUnknownKeys)
}
