package hearth.kindlings.ubjsonderivation

/** Test helper: renders UBJson bytes as compact JSON text, so that tests can assert the encoded shape. */
object UbRender {

  def apply(bytes: Array[Byte]): String = {
    val in = new UBJsonReader(bytes)
    val sb = new StringBuilder
    def put(a: Any): Unit = { val _ = sb.append(a) }
    def str(s: String): Unit = put("\"" + s.replace("\"", "\\\"") + "\"")
    def value(): Unit = in.peekToken() match {
      case 'Z' => in.readNull(); put("null")
      case 'T' | 'F' => put(in.readBoolean())
      case 'i' | 'U' | 'I' | 'l' => put(in.readInt())
      case 'L' => put(in.readLong())
      case 'd' => put(in.readFloat())
      case 'D' => put(in.readDouble())
      case 'H' => put(in.readBigDecimal())
      case 'C' => str(in.readChar().toString)
      case 'S' => str(in.readString())
      case '[' =>
        in.readArrayStart(); put('[')
        var first = true
        while (!in.isArrayEnd()) { if (!first) put(','); first = false; value() }
        put(']')
      case '{' =>
        in.readObjectStart(); put('{')
        var first = true
        while (!in.isObjectEnd()) {
          if (!first) put(','); first = false
          str(in.readFieldName()); put(':'); value()
        }
        put('}')
      case other => throw new IllegalArgumentException(s"unexpected marker ${other.toChar}")
    }
    value()
    sb.toString
  }

  def encode[A](value: A)(codec: UBJsonValueCodec[A]): String = {
    val w = new UBJsonWriter()
    codec.encode(w, value)
    apply(w.toByteArray)
  }

  def roundTrip[A](value: A)(codec: UBJsonValueCodec[A]): A = {
    val w = new UBJsonWriter()
    codec.encode(w, value)
    codec.decode(new UBJsonReader(w.toByteArray))
  }
}
