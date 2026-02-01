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

  /** Renders a value to a StringBuilder without indentation (backward compatible). */
  def render(sb: StringBuilder)(value: A): StringBuilder

  /** Renders a value to a StringBuilder with indentation support. */
  def render(sb: StringBuilder, config: RenderConfig, level: Int)(value: A): StringBuilder
}
object FastShowPretty extends FastShowPrettyCompanionCompat
