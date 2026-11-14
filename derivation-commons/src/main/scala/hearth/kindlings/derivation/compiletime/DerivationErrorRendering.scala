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

  /** Renders one error as `"  - first line"`, with every further line indented by 4 spaces. */
  def renderError(e: Throwable): String = {
    val message = messageOf(e)
    message.split("\n").toList match {
      case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
      case Nil          => "  - " + message
    }
  }

  /** Renders every error with [[renderError]], one after another. */
  def renderErrors(errors: Iterable[Throwable]): String = errors.iterator.map(renderError).mkString("\n")
}
