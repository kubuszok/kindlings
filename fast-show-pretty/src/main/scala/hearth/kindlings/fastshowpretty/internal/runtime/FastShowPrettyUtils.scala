package hearth.kindlings.fastshowpretty.internal.runtime

object FastShowPrettyUtils {
  
  def renderBoolean(sb: StringBuilder)(value: Boolean): StringBuilder =
    if (value) sb.append("true") else sb.append("false")
  def renderByte(sb: StringBuilder)(value: Byte): StringBuilder =
    sb.append(value.toString).append(".toByte")
  def renderShort(sb: StringBuilder)(value: Short): StringBuilder =
    sb.append(value.toString).append(".toShort")
  def renderInt(sb: StringBuilder)(value: Int): StringBuilder =
    sb.append(value.toString)
  def renderLong(sb: StringBuilder)(value: Long): StringBuilder =
    sb.append(value.toString).append('L')
  def renderFloat(sb: StringBuilder)(value: Float): StringBuilder = {
    val result = value.toString
    sb.append(result)
    // Workaround for https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
    if (result.contains(".")) {
      sb.append(".0") 
    }
    sb.append('f')
  }
  def renderDouble(sb: StringBuilder)(value: Double): StringBuilder = {
    val result = value.toString
    sb.append(result)
    // Workaround for https://www.scala-js.org/doc/semantics.html#tostring-of-float-double-and-unit
    if (result.contains(".")) {
      sb.append(".0") 
    }
    sb.append('d')
  }
  def renderChar(sb: StringBuilder)(value: Char): StringBuilder =
    sb.append("'").append(value.toString).append("'")
  def renderString(sb: StringBuilder)(value: String): StringBuilder =
    sb.append('"').append(value.replaceAll("\"", "\\\"").replaceAll("\n", "\\n")).append('"')
}
