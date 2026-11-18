package hearth.kindlings.derivation.compiletime

/** Renders the errors a failed derivation collected into the bullet list shown in the "Macro derivation failed with the
  * following errors" message.
  *
  * Never throws: a `Throwable` whose `getMessage` is `null` (e.g. an abort exception raised after the real error was
  * already reported) is rendered by its `toString`, which falls back to the class name.
  */
object DerivationErrorRendering {

  /** The message of `e`, or `e.toString` (its class name, when there is no message) if `getMessage` is `null`. */
  def messageOf(e: Throwable): String = {
    val message = e.getMessage
    if (message eq null) e.toString else message
  }

  private val AnsiEscape = "\u001b\\[[0-9;]*[A-Za-z]".r

  /** `text` without ANSI escape codes. Error messages are built with `Type#prettyPrint`, which colours type names for
    * the logs; in an error message the codes are noise (e.g. in munit's `compileErrors` or an IDE).
    */
  def withoutAnsiCodes(text: String): String =
    if (text.indexOf('\u001b') < 0) text else AnsiEscape.replaceAllIn(text, "")

  /** Renders one error as `"  - first line"`, with every further line indented by 4 spaces, without ANSI codes. */
  def renderError(e: Throwable): String = {
    val message = withoutAnsiCodes(messageOf(e))
    message.split("\n").toList match {
      case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
      case Nil          => "  - " + message
    }
  }

  /** Renders every error with [[renderError]], one after another. */
  def renderErrors(errors: Iterable[Throwable]): String = errors.iterator.map(renderError).mkString("\n")
}
