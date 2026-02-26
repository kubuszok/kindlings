package hearth.kindlings.tapirschemaderivation

/** Type-level marker used to disambiguate when multiple JSON library configurations are in implicit scope.
  *
  * When both circe `Configuration` and jsoniter `JsoniterConfig` are on the classpath (both have companion-object
  * implicit defaults), the schema derivation macro finds multiple configs and fails. Import an implicit
  * `PreferSchemaConfig[ConfigType]` to tell the macro which one to use:
  *
  * {{{
  * import hearth.kindlings.circederivation.Configuration
  * implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
  * }}}
  *
  * Not needed when exactly one JSON library configuration is in implicit scope.
  *
  * @tparam ConfigType
  *   the JSON library's configuration type (e.g. `Configuration` for circe, `JsoniterConfig` for jsoniter)
  */
sealed trait PreferSchemaConfig[ConfigType]
object PreferSchemaConfig {
  private object Impl extends PreferSchemaConfig[Nothing]

  def apply[ConfigType]: PreferSchemaConfig[ConfigType] = Impl.asInstanceOf[PreferSchemaConfig[ConfigType]]
}
