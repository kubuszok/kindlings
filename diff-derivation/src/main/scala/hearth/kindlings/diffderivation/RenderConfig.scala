package hearth.kindlings.diffderivation

sealed trait NameStyle extends Product with Serializable
object NameStyle {
  case object Simple extends NameStyle
  case object Short extends NameStyle
  case object FullyQualified extends NameStyle
  case object Pretty extends NameStyle
}

sealed trait ColorMode extends Product with Serializable
object ColorMode {
  case object Plain extends ColorMode
  case object Ansi extends ColorMode
}

sealed trait Indent extends Product with Serializable
object Indent {
  final case class Spaces(count: Int) extends Indent
  case object Tab extends Indent
}

final case class RenderConfig(
    nameStyle: NameStyle,
    colorMode: ColorMode,
    indent: Indent
)
object RenderConfig {
  val default: RenderConfig = RenderConfig(NameStyle.Simple, ColorMode.Ansi, Indent.Spaces(2))
  val plain: RenderConfig = RenderConfig(NameStyle.Simple, ColorMode.Plain, Indent.Spaces(2))
}
