package hearth.kindlings.catsderivation.internal.runtime

object ShowPrettyRuntime {

  def indentSubsequentLines(s: String): String = {
    val idx = s.indexOf('\n')
    if (idx < 0) s
    else {
      val sb = new StringBuilder(s.length + 16)
      sb.append(s.substring(0, idx))
      var i = idx
      while (i < s.length) {
        sb.append(s.charAt(i))
        if (s.charAt(i) == '\n' && i + 1 < s.length) sb.append("  ")
        i += 1
      }
      sb.toString
    }
  }
}
