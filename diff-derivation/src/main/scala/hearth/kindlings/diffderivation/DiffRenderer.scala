package hearth.kindlings.diffderivation

import hearth.kindlings.diffderivation.DiffResult.*

object DiffRenderer {

  private val ESC = ''
  private val AnsiGreen = ESC.toString + "[32m"
  private val AnsiRed = ESC.toString + "[31m"
  private val AnsiYellow = ESC.toString + "[33m"
  private val AnsiReset = ESC.toString + "[0m"

  def render(result: DiffResult, config: RenderConfig = RenderConfig.default): String = {
    val sb = new StringBuilder()
    renderTo(sb, result, config, 0)
    sb.result()
  }

  private def nameOf(node: DiffResult, config: RenderConfig): String = config.nameStyle match {
    case NameStyle.Simple         => node.simpleName
    case NameStyle.Short          => node.shortName
    case NameStyle.FullyQualified => node.plainName
    case NameStyle.Pretty         => node.prettyName
  }

  private def indentStr(config: RenderConfig): String = config.indent match {
    case Indent.Spaces(n) => " " * n
    case Indent.Tab       => "\t"
  }

  private def appendIndent(sb: StringBuilder, config: RenderConfig, level: Int): Unit = {
    val unit = indentStr(config)
    var i = 0
    while (i < level) { sb ++= unit; i += 1 }
  }

  private def colorAdded(text: String, config: RenderConfig): String = config.colorMode match {
    case ColorMode.Ansi  => AnsiGreen + text + AnsiReset
    case ColorMode.Plain => text
  }

  private def colorRemoved(text: String, config: RenderConfig): String = config.colorMode match {
    case ColorMode.Ansi  => AnsiRed + text + AnsiReset
    case ColorMode.Plain => text
  }

  private def colorChanged(text: String, config: RenderConfig): String = config.colorMode match {
    case ColorMode.Ansi  => AnsiYellow + text + AnsiReset
    case ColorMode.Plain => text
  }

  private def renderTo(sb: StringBuilder, result: DiffResult, config: RenderConfig, level: Int): Unit =
    result match {
      case v: Identical =>
        sb ++= v.display

      case v: ValueChanged =>
        sb ++= colorRemoved(v.left, config)
        sb ++= " -> "
        sb ++= colorAdded(v.right, config)

      case v: Record =>
        sb ++= nameOf(v, config)
        sb += '('
        val changed = v.fields.filterNot(_._2.isIdentical)
        if (changed.isEmpty && v.fields.nonEmpty) {
          sb ++= "..."
        } else {
          var first = true
          v.fields.foreach { case (name, fieldDiff) =>
            sb += '\n'
            appendIndent(sb, config, level + 1)
            if (fieldDiff.isIdentical) {
              sb ++= name
              sb ++= " = "
              renderTo(sb, fieldDiff, config, level + 1)
            } else {
              sb ++= colorChanged(name, config)
              sb ++= " = "
              renderTo(sb, fieldDiff, config, level + 1)
            }
            if (!first || v.fields.size > 1) sb += ','
            first = false
          }
          sb += '\n'
          appendIndent(sb, config, level)
        }
        sb += ')'

      case v: Variant =>
        sb ++= nameOf(v, config)
        sb += '.'
        sb ++= v.variantName
        sb += '('
        if (!v.body.isIdentical) {
          sb += '\n'
          appendIndent(sb, config, level + 1)
          renderTo(sb, v.body, config, level + 1)
          sb += '\n'
          appendIndent(sb, config, level)
        } else {
          sb ++= "..."
        }
        sb += ')'

      case v: TypeMismatch =>
        sb ++= nameOf(v, config)
        sb ++= ": "
        sb ++= colorRemoved(v.leftVariant, config)
        sb ++= " -> "
        sb ++= colorAdded(v.rightVariant, config)
        sb += '\n'
        appendIndent(sb, config, level + 1)
        sb ++= "- "
        renderTo(sb, v.leftSnapshot, config, level + 1)
        sb += '\n'
        appendIndent(sb, config, level + 1)
        sb ++= "+ "
        renderTo(sb, v.rightSnapshot, config, level + 1)

      case v: SeqDiff =>
        sb ++= nameOf(v, config)
        sb += '('
        v.edits.foreach { edit =>
          sb += '\n'
          appendIndent(sb, config, level + 1)
          edit match {
            case Edit.Equal(d) =>
              sb ++= "  "
              renderTo(sb, d, config, level + 1)
            case Edit.Insert(d) =>
              sb ++= colorAdded("+ ", config)
              renderTo(sb, d, config, level + 1)
            case Edit.Delete(d) =>
              sb ++= colorRemoved("- ", config)
              renderTo(sb, d, config, level + 1)
          }
          sb += ','
        }
        sb += '\n'
        appendIndent(sb, config, level)
        sb += ')'

      case v: MapDiff =>
        sb ++= nameOf(v, config)
        sb += '('
        v.entries.foreach { entry =>
          sb += '\n'
          appendIndent(sb, config, level + 1)
          entry match {
            case MapEntry.Matched(key, valueDiff) =>
              sb ++= "  "
              sb ++= key
              sb ++= " -> "
              renderTo(sb, valueDiff, config, level + 1)
            case MapEntry.Added(key, snapshot) =>
              sb ++= colorAdded("+ ", config)
              sb ++= key
              sb ++= " -> "
              renderTo(sb, snapshot, config, level + 1)
            case MapEntry.Removed(key, snapshot) =>
              sb ++= colorRemoved("- ", config)
              sb ++= key
              sb ++= " -> "
              renderTo(sb, snapshot, config, level + 1)
          }
          sb += ','
        }
        sb += '\n'
        appendIndent(sb, config, level)
        sb += ')'

      case v: SetDiff =>
        sb ++= nameOf(v, config)
        sb += '('
        v.entries.foreach { entry =>
          sb += '\n'
          appendIndent(sb, config, level + 1)
          entry match {
            case SetEntry.Matched(d) =>
              sb ++= "  "
              renderTo(sb, d, config, level + 1)
            case SetEntry.Added(snapshot) =>
              sb ++= colorAdded("+ ", config)
              renderTo(sb, snapshot, config, level + 1)
            case SetEntry.Removed(snapshot) =>
              sb ++= colorRemoved("- ", config)
              renderTo(sb, snapshot, config, level + 1)
          }
          sb += ','
        }
        sb += '\n'
        appendIndent(sb, config, level)
        sb += ')'

      case v: OptionalDiff =>
        v.inner match {
          case OptionalContent.BothPresent(d) =>
            sb ++= nameOf(v, config)
            sb += '('
            renderTo(sb, d, config, level)
            sb += ')'
          case OptionalContent.LeftOnly(snapshot) =>
            sb ++= colorRemoved("Some", config)
            sb += '('
            renderTo(sb, snapshot, config, level)
            sb += ')'
            sb ++= " -> "
            sb ++= colorAdded("None", config)
          case OptionalContent.RightOnly(snapshot) =>
            sb ++= colorRemoved("None", config)
            sb ++= " -> "
            sb ++= colorAdded("Some", config)
            sb += '('
            renderTo(sb, snapshot, config, level)
            sb += ')'
          case OptionalContent.BothAbsent =>
            sb ++= "None"
        }

      case v: StringDiff =>
        renderStringDiff(sb, v, config, level)
    }

  private def renderStringDiff(
      sb: StringBuilder,
      v: StringDiff,
      config: RenderConfig,
      level: Int
  ): Unit = {
    sb ++= nameOf(v, config)
    sb ++= "(\n"
    v.chunks.foreach { chunk =>
      appendIndent(sb, config, level + 1)
      chunk match {
        case StringChunk.EqualLine(text) =>
          sb ++= "  "
          sb ++= text
        case StringChunk.InsertLine(text) =>
          sb ++= colorAdded("+ " + text, config)
        case StringChunk.DeleteLine(text) =>
          sb ++= colorRemoved("- " + text, config)
        case StringChunk.ChangedLine(words) =>
          sb ++= "~ "
          words.foreach {
            case WordChunk.EqualWord(t)       => sb ++= t
            case WordChunk.InsertWord(t)      => sb ++= colorAdded(t, config)
            case WordChunk.DeleteWord(t)      => sb ++= colorRemoved(t, config)
            case WordChunk.ChangedWord(chars) =>
              chars.foreach {
                case CharChunk.EqualChar(t)  => sb ++= t
                case CharChunk.InsertChar(t) => sb ++= colorAdded(t, config)
                case CharChunk.DeleteChar(t) => sb ++= colorRemoved(t, config)
              }
          }
      }
      sb += '\n'
    }
    appendIndent(sb, config, level)
    sb += ')'
  }
}
