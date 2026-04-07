package hearth.kindlings.sconfigderivation

/** A function `String => String` that translates Scala identifier names into HOCON keys (or HOCON keys into Scala names
  * — both directions are achievable by swapping the source and target conventions). Mirrors
  * `pureconfig.ConfigFieldMapping` exactly.
  *
  * The function is intentionally one-way: writers and readers both call the same forward function so that encoding
  * round-trips through decoding without needing an inverse.
  *
  * Build via `ConfigFieldMapping(source, target)`:
  * {{{
  *   val mapping = ConfigFieldMapping(CamelCase, KebabCase)
  *   mapping("myFieldName") // "my-field-name"
  * }}}
  *
  * Or via a custom `String => String`:
  * {{{
  *   val mapping = ConfigFieldMapping(s => s.toUpperCase)
  * }}}
  */
trait ConfigFieldMapping extends (String => String) {
  override def apply(fieldName: String): String

  /** Override individual entries while delegating everything else to this mapping.
    * {{{
    *   val mapping = ConfigFieldMapping(CamelCase, KebabCase)
    *     .withOverrides("myField" -> "my-special-key")
    * }}}
    */
  def withOverrides(overrides: (String, String)*): ConfigFieldMapping = {
    val table = overrides.toMap.withDefault(apply)
    ConfigFieldMapping(table)
  }
}
object ConfigFieldMapping {

  /** Wrap an arbitrary `String => String`. */
  def apply(f: String => String): ConfigFieldMapping =
    new ConfigFieldMapping {
      override def apply(fieldName: String): String = f(fieldName)
    }

  /** Compose two naming conventions: `source.toTokens andThen target.fromTokens`. If both conventions are equal,
    * returns the identity mapping for free.
    */
  def apply(typeFieldConvention: NamingConvention, configFieldConvention: NamingConvention): ConfigFieldMapping =
    if (typeFieldConvention == configFieldConvention) apply(identity[String])
    else apply(typeFieldConvention.toTokens _ andThen configFieldConvention.fromTokens)
}
