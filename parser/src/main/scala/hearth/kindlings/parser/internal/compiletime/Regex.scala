package hearth.kindlings.parser
package internal.compiletime

/** Parser for the DFA-compatible subset of Java regex syntax accepted in terminals.
  *
  * Supported: literal chars, `.`, escapes (`\n \t \r \f \e \0 \xHH \uHHHH`, escaped punctuation), classes `\d \D \w \W
  * \s \S`, bracket classes with ranges and negation (`[a-z_]`, `[^"\\]`), groups `(...)` and `(?:...)`, alternation
  * `|`, quantifiers `* + ? {n} {n,} {n,m}`. Rejected (they cannot be compiled into a DFA or are ambiguous for a lexer):
  * anchors, back-references, look-arounds, lazy/possessive quantifiers, flags.
  */
private[parser] object Regex {

  /** Sorted, disjoint, non-adjacent inclusive ranges of UTF-16 code units. */
  final case class CharSet(ranges: List[(Int, Int)]) {
    def union(other: CharSet): CharSet = CharSet.normalize(ranges ++ other.ranges)
    def complement: CharSet = {
      val sb = List.newBuilder[(Int, Int)]
      var next = 0
      ranges.foreach { case (lo, hi) =>
        if (lo > next) sb += (next -> (lo - 1))
        next = hi + 1
      }
      if (next <= CharSet.Max) sb += (next -> CharSet.Max)
      CharSet(sb.result())
    }
  }
  object CharSet {
    val Max: Int = 0xffff
    def char(c: Int): CharSet = CharSet(List(c -> c))
    def range(lo: Int, hi: Int): CharSet = CharSet(List(lo -> hi))
    val empty: CharSet = CharSet(Nil)
    val digit: CharSet = range('0', '9')
    val word: CharSet = normalize(
      List('a'.toInt -> 'z'.toInt, 'A'.toInt -> 'Z'.toInt, '0'.toInt -> '9'.toInt, '_'.toInt -> '_'.toInt)
    )
    val space: CharSet = normalize(List(' ', '\t', '\n', '\r', '\f', '\u000b').map(c => c.toInt -> c.toInt))
    val dot: CharSet = normalize(List('\n', '\r', '\u0085', ' ', ' ').map(c => c.toInt -> c.toInt)).complement

    def normalize(ranges: List[(Int, Int)]): CharSet = {
      val sorted = ranges.filter { case (lo, hi) => lo <= hi }.sortBy(_._1)
      val out = List.newBuilder[(Int, Int)]
      var current: Option[(Int, Int)] = None
      sorted.foreach { case (lo, hi) =>
        current match {
          case Some((clo, chi)) if lo <= chi + 1 => current = Some(clo -> math.max(chi, hi))
          case Some(c)                           => out += c; current = Some(lo -> hi)
          case None                              => current = Some(lo -> hi)
        }
      }
      current.foreach(out += _)
      CharSet(out.result())
    }
  }

  sealed trait Node
  final case class Chars(set: CharSet) extends Node
  final case class Concat(nodes: List[Node]) extends Node
  final case class Alternation(nodes: List[Node]) extends Node
  final case class Repeat(node: Node, min: Int, max: Option[Int]) extends Node

  val MaxRepeat: Int = 1000

  def literal(text: String): Node = Concat(text.toList.map(c => Chars(CharSet.char(c.toInt))))

  def parse(pattern: String): Either[String, Node] =
    try Right(new RegexParser(pattern).parseAll())
    catch { case e: RegexError => Left(e.getMessage) }

  final private class RegexError(message: String) extends Exception(message)

  final private class RegexParser(p: String) {
    private var i = 0

    private def fail(message: String): Nothing = throw new RegexError(s"$message at index $i of /$p/")
    private def peek: Char = p.charAt(i)
    private def atEnd: Boolean = i >= p.length

    def parseAll(): Node = {
      val node = alternation()
      if (!atEnd) fail(s"unexpected '$peek'")
      node
    }

    private def alternation(): Node = {
      val first = concat()
      if (!atEnd && peek == '|') {
        val alts = List.newBuilder[Node]
        alts += first
        while (!atEnd && peek == '|') {
          i += 1
          alts += concat()
        }
        Alternation(alts.result())
      } else first
    }

    private def concat(): Node = {
      val items = List.newBuilder[Node]
      while (!atEnd && peek != '|' && peek != ')') items += quantified(atom())
      Concat(items.result())
    }

    private def quantified(node: Node): Node = {
      var n = node
      var continue = true
      while (continue && !atEnd) {
        val q = peek match {
          case '*' => i += 1; Some(Repeat(n, 0, None))
          case '+' => i += 1; Some(Repeat(n, 1, None))
          case '?' => i += 1; Some(Repeat(n, 0, Some(1)))
          case '{' => Some(bounds(n))
          case _   => None
        }
        q match {
          case Some(r) =>
            if (!atEnd && (peek == '?' || peek == '+')) fail("lazy and possessive quantifiers are not supported")
            n = r
          case None => continue = false
        }
      }
      n
    }

    private def bounds(node: Node): Node = {
      i += 1 // '{'
      val min = number()
      val max =
        if (!atEnd && peek == ',') {
          i += 1
          if (!atEnd && peek == '}') None else Some(number())
        } else Some(min)
      if (atEnd || peek != '}') fail("expected '}'")
      i += 1
      if (max.exists(_ < min)) fail("invalid repetition bounds")
      if (min > MaxRepeat || max.exists(_ > MaxRepeat)) fail(s"repetition bounds above $MaxRepeat are not supported")
      Repeat(node, min, max)
    }

    private def number(): Int = {
      val start = i
      while (!atEnd && peek.isDigit) i += 1
      if (start == i) fail("expected a number")
      p.substring(start, i).toInt
    }

    private def atom(): Node = peek match {
      case '(' =>
        i += 1
        if (!atEnd && peek == '?') {
          if (i + 1 < p.length && p.charAt(i + 1) == ':') i += 2
          else fail("only non-capturing groups (?:...) are supported")
        }
        val inner = alternation()
        if (atEnd || peek != ')') fail("expected ')'")
        i += 1
        inner
      case '['                   => Chars(bracket())
      case '.'                   => i += 1; Chars(CharSet.dot)
      case '\\'                  => Chars(escape(inClass = false))
      case '^' | '$'             => fail("anchors are not supported")
      case '*' | '+' | '?' | '{' => fail(s"dangling quantifier '$peek'")
      case c                     =>
        i += 1
        Chars(CharSet.char(c.toInt))
    }

    private def bracket(): CharSet = {
      i += 1 // '['
      val negated = !atEnd && peek == '^'
      if (negated) i += 1
      var set = CharSet.empty
      var first = true
      while (!atEnd && (peek != ']' || first)) {
        first = false
        val lo = classAtom()
        if (!atEnd && peek == '-' && i + 1 < p.length && p.charAt(i + 1) != ']') {
          i += 1
          val hi = classAtom()
          (lo.ranges, hi.ranges) match {
            case (List((a, b)), List((c, d))) if a == b && c == d =>
              if (c < a) fail("invalid range")
              set = set.union(CharSet.range(a, c))
            case _ => fail("invalid range")
          }
        } else set = set.union(lo)
      }
      if (atEnd) fail("unterminated character class")
      i += 1 // ']'
      if (negated) set.complement else set
    }

    private def classAtom(): CharSet = peek match {
      case '\\' => escape(inClass = true)
      case '['  => fail("nested character classes are not supported")
      case c    => i += 1; CharSet.char(c.toInt)
    }

    private def escape(inClass: Boolean): CharSet = {
      i += 1 // '\'
      if (atEnd) fail("dangling escape")
      val c = peek
      i += 1
      c match {
        case 'n'                            => CharSet.char('\n')
        case 't'                            => CharSet.char('\t')
        case 'r'                            => CharSet.char('\r')
        case 'f'                            => CharSet.char('\f')
        case 'e'                            => CharSet.char(0x1b)
        case '0'                            => CharSet.char(0)
        case 'd'                            => CharSet.digit
        case 'D'                            => CharSet.digit.complement
        case 'w'                            => CharSet.word
        case 'W'                            => CharSet.word.complement
        case 's'                            => CharSet.space
        case 'S'                            => CharSet.space.complement
        case 'x'                            => CharSet.char(hex(2))
        case 'u'                            => CharSet.char(hex(4))
        case 'b' if inClass                 => CharSet.char('\b')
        case other if other.isLetterOrDigit =>
          fail(s"escape '\\$other' is not supported (anchors, back-references and Unicode properties are not allowed)")
        case other => CharSet.char(other.toInt)
      }
    }

    private def hex(digits: Int): Int = {
      if (i + digits > p.length) fail("incomplete hex escape")
      val text = p.substring(i, i + digits)
      i += digits
      try Integer.parseInt(text, 16)
      catch { case _: NumberFormatException => fail(s"invalid hex escape '$text'") }
    }
  }
}
