package hearth.kindlings.parser

/** A syntax error: unexpected input at `offset` (0-based) / `line`:`column` (1-based).
  *
  * @param expected
  *   display names of the tokens that would have been accepted at this point (sorted)
  * @param found
  *   display name (or text) of what was found instead
  * @param endOfInput
  *   the input ended while the parser still expected more: the input is a valid *prefix* of the language. A REPL can
  *   use it to ask for another line instead of reporting an error.
  */
final class ParseError(
    val offset: Long,
    val line: Int,
    val column: Int,
    val expected: List[String],
    val found: String,
    val detail: String,
    val endOfInput: Boolean
) extends Exception(ParseError.render(line, column, expected, found, detail)) {

  override def toString: String = s"ParseError($getMessage)"
}
object ParseError {

  private def render(line: Int, column: Int, expected: List[String], found: String, detail: String): String = {
    val exp =
      if (expected.isEmpty) ""
      else if (expected.size == 1) s", expected ${expected.head}"
      else s", expected one of: ${expected.mkString(", ")}"
    s"$detail at $line:$column: found $found$exp"
  }
}

/** How a [[ParseError]] is represented in the error channel `E` of `Either[E, *]` parsers. */
trait ParseErrorLift[E] {
  def lift(error: ParseError): E
}
object ParseErrorLift {

  implicit val parseError: ParseErrorLift[ParseError] = (error: ParseError) => error
  implicit val throwable: ParseErrorLift[Throwable] = (error: ParseError) => error
  implicit val string: ParseErrorLift[String] = (error: ParseError) => error.getMessage
}
