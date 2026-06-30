package hearth.kindlings.fastshowpretty

/** Configuration for rendering with indentation support. */
final case class RenderConfig(
    indentString: String = "  ", // 2 spaces default
    startLevel: Int = 0
)
object RenderConfig {
  val Default: RenderConfig = RenderConfig()
  val Compact: RenderConfig = RenderConfig(indentString = "") // No indent
  val Tabs: RenderConfig = RenderConfig(indentString = "\t")
  val FourSpaces: RenderConfig = RenderConfig(indentString = "    ")
}

trait FastShowPretty[A] {

  /** Renders a value to a StringBuilder with indentation support. */
  def render(sb: StringBuilder, config: RenderConfig, level: Int)(value: A): StringBuilder
}
object FastShowPretty extends FastShowPrettyCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.fastshowpretty.debug.logDerivation]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation

  /** Opt-in marker for the derivation policy. When the build sets
    * `-Xmacro-settings:fastShowPrettyDerivation.policy.enabled=opt-in` with `optInByImport=true`, importing an implicit
    * of this type (see [[hearth.kindlings.fastshowpretty.policy.allowDerivationForFastShowPretty]]) permits structural
    * derivation in the current scope.
    *
    * Kept OUTSIDE [[FastShowPretty]]'s implicit scope so it is never summoned automatically.
    */
  sealed trait AllowDerivation
  object AllowDerivation extends AllowDerivation
}
