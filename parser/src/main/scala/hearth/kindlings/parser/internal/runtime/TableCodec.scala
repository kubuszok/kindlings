package hearth.kindlings.parser
package internal.runtime

/** Encodes `Int` sequences as printable ASCII text, so that compile-time tables can be embedded in generated code as
  * string literals (arrays of literals would hit the JVM's 64KB method-size limit much sooner).
  *
  * Each value is zig-zag encoded and written as base-32 digits (least significant first), each digit as one char in
  * `'0' + digit`, with `+ 32` marking "more digits follow". All chars are in `'0'..'o'`.
  */
private[parser] object TableCodec {

  /** Maximal length of one string literal chunk (JVM constant pool strings are limited to 65535 bytes). */
  val ChunkSize: Int = 60000

  final class Writer {
    private val sb = new StringBuilder

    def int(value: Int): Unit = {
      var v = (value << 1) ^ (value >> 31)
      while ((v & ~31) != 0) {
        sb += (48 + (v & 31) + 32).toChar
        v = v >>> 5
      }
      sb += (48 + v).toChar
      ()
    }

    def ints(values: Array[Int]): Unit = {
      int(values.length)
      values.foreach(int)
    }

    def string(value: String): Unit = {
      int(value.length)
      value.foreach(c => int(c.toInt))
    }

    def result: String = sb.toString

    def chunks: List[String] = result.grouped(ChunkSize).toList
  }

  final class Reader(text: String) {
    private var pos = 0

    def int(): Int = {
      var result = 0
      var shift = 0
      var more = true
      while (more) {
        val d = text.charAt(pos) - 48
        pos += 1
        result |= (d & 31) << shift
        shift += 5
        more = d >= 32
      }
      (result >>> 1) ^ -(result & 1)
    }

    def ints(): Array[Int] = {
      val n = int()
      val array = new Array[Int](n)
      var i = 0
      while (i < n) { array(i) = int(); i += 1 }
      array
    }

    def string(): String = {
      val n = int()
      val sb = new StringBuilder(n)
      var i = 0
      while (i < n) { sb.append(int().toChar); i += 1 }
      sb.toString
    }
  }
}
