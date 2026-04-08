package hearth.kindlings.sconfigderivation

import scala.collection.compat.immutable.ArraySeq

/** Bidirectional naming convention used by [[ConfigFieldMapping]] to translate between Scala identifier styles and
  * HOCON key conventions. The implementation mirrors `pureconfig.NamingConvention` exactly so the two modules behave
  * identically when users expect the same field-name transformations.
  *
  * Each convention can `toTokens(s)` parse an identifier into its underlying word list, and `fromTokens(words)` render
  * that word list back into the convention's preferred spelling. Composing two conventions (`source.toTokens andThen
  * target.fromTokens`) gives a one-way `String => String` transformation suitable for the `transformMemberNames` /
  * `transformConstructorNames` slots on [[SConfig]].
  */
trait NamingConvention {
  def toTokens(s: String): Seq[String]
  def fromTokens(l: Seq[String]): String
}

/** Marker trait for `CamelCase` / `PascalCase` style conventions. Both share the same tokeniser; they differ only in
  * how they recombine tokens.
  *
  * The tokeniser splits on the same boundaries as `pureconfig.CapitalizedWordsNamingConvention`:
  *   - acronym → word: `XMLParser` → `XML | Parser` (uppercase followed by uppercase+lowercase)
  *   - lower → upper: `myField` → `my | Field`
  *   - letter → non-letter: `field2name` → `field | 2 | name`
  *
  * Implemented with a manual char-by-char loop instead of a lookbehind regex so it works on Scala.js without requiring
  * ES2018+ mode (lookbehind assertions are an ES2018 feature and would otherwise force every downstream Scala.js build
  * to opt in).
  */
trait CapitalizedWordsNamingConvention extends NamingConvention {
  override def toTokens(s: String): Seq[String] = {
    if (s.isEmpty) return Seq.empty[String]
    val result = scala.collection.mutable.ListBuffer.empty[String]
    val current = new StringBuilder
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      val splitBefore: Boolean =
        if (i == 0) false
        else {
          val prev = s.charAt(i - 1)
          // rule 1: acronym → word, e.g. `XMLParser` between `L` and `P`
          val rule1 = prev.isUpper && c.isUpper && (i + 1 < s.length) && s.charAt(i + 1).isLower
          // rule 2: lower → upper, e.g. `myField` between `y` and `F`
          val rule2 = !prev.isUpper && c.isUpper
          // rule 3: letter → non-letter, e.g. `field2name` between `d` and `2`
          val rule3 = prev.isLetter && !c.isLetter
          rule1 || rule2 || rule3
        }
      if (splitBefore) {
        result += current.toString.toLowerCase
        current.clear()
      }
      current.append(c)
      i += 1
    }
    if (current.nonEmpty) result += current.toString.toLowerCase
    result.toList
  }
}

/** Lowercase first token, capitalize the rest. `myFieldName` ↔ `["my", "field", "name"]`. */
object CamelCase extends CapitalizedWordsNamingConvention {
  override def fromTokens(l: Seq[String]): String = l match {
    case Seq()      => ""
    case h +: Seq() => h.toLowerCase
    case h +: t     => h.toLowerCase + t.map(_.capitalize).mkString
  }
}

/** Capitalize every token. `MyFieldName` ↔ `["my", "field", "name"]`. */
object PascalCase extends CapitalizedWordsNamingConvention {
  override def fromTokens(l: Seq[String]): String = l.map(_.capitalize).mkString
}

/** Tokens joined by an arbitrary delimiter, all lowercase. */
class StringDelimitedNamingConvention(d: String) extends NamingConvention {
  override def toTokens(s: String): Seq[String] =
    ArraySeq.unsafeWrapArray(s.split(d)).map(_.toLowerCase)
  override def fromTokens(l: Seq[String]): String =
    l.map(_.toLowerCase).mkString(d)
}

/** `my-field-name` */
object KebabCase extends StringDelimitedNamingConvention("-")

/** `my_field_name` */
object SnakeCase extends StringDelimitedNamingConvention("_")

/** `MY_FIELD_NAME` — parsed as snake_case but emitted in upper case. */
object ScreamingSnakeCase extends NamingConvention {
  override def toTokens(s: String): Seq[String] = SnakeCase.toTokens(s)
  override def fromTokens(l: Seq[String]): String = l.map(_.toUpperCase).mkString("_")
}
