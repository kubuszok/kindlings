package hearth.kindlings.circederivation

/** Name transformations for [[Configuration.transformMemberNames]] / [[Configuration.transformConstructorNames]], like
  * Scala 3 core circe's `io.circe.derivation.renaming` (and generic-extras' `Configuration.snakeCaseTransformation`
  * etc.).
  *
  * Consecutive capitals are kept together, as upstream: `HTMLParser` becomes `html_parser`.
  */
object renaming {

  private val basePattern = java.util.regex.Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val swapPattern = java.util.regex.Pattern.compile("([a-z\\d])([A-Z])")

  private def separateWords(s: String, separator: Char): String = {
    val partial = basePattern.matcher(s).replaceAll("$1" + separator + "$2")
    swapPattern.matcher(partial).replaceAll("$1" + separator + "$2")
  }

  /** `firstName` → `first_name` */
  val snakeCase: String => String = s => separateWords(s, '_').toLowerCase

  /** `firstName` → `FIRST_NAME` */
  val screamingSnakeCase: String => String = s => separateWords(s, '_').toUpperCase

  /** `firstName` → `first-name` */
  val kebabCase: String => String = s => separateWords(s, '-').toLowerCase

  /** `firstName` → `FirstName` */
  val pascalCase: String => String = s => if (s.isEmpty) s else s.charAt(0).toUpper.toString + s.substring(1)

  /** Replaces the listed names, keeps the others: `replaceWith("Circle" -> "circle")`. */
  def replaceWith(pairs: (String, String)*): String => String = {
    val map = pairs.toMap
    original => map.getOrElse(original, original)
  }
}
