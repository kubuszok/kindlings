package hearth.kindlings.ubjsonderivation

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

/** A streaming UBJson reader that reads from a byte array or input stream.
  *
  * UBJson type markers:
  *   - Z = null, N = noop, T = true, F = false
  *   - i = int8, U = uint8, I = int16, l = int32, L = int64
  *   - d = float32, D = float64, H = high-precision number (string)
  *   - C = char, S = string (length-prefixed)
  *   - [ / ] = array start/end, { / } = object start/end
  */
final class UBJsonReader private (private val in: InputStream) {

  private var pos: Int = 0
  private var peeked: Int = -1 // -1 means no peeked byte

  def this(bytes: Array[Byte]) = this(new ByteArrayInputStream(bytes))

  // --- Low-level reading ---

  private def readRawByte(): Byte = {
    val b = if (peeked != -1) {
      val v = peeked.toByte
      peeked = -1
      v
    } else {
      val v = in.read()
      if (v == -1) decodeError("unexpected end of input")
      v.toByte
    }
    pos += 1
    b
  }

  private def readRawBytes(n: Int): Array[Byte] = {
    val buf = new Array[Byte](n)
    var offset = 0
    while (offset < n) {
      val read = in.read(buf, offset, n - offset)
      if (read == -1) decodeError("unexpected end of input")
      offset += read
    }
    pos += n
    buf
  }

  def decodeError(msg: String): Nothing =
    throw new UBJsonReaderException(msg, pos)

  // --- Token management ---

  /** Read the next type marker byte without consuming it. */
  def peekToken(): Byte = {
    if (peeked == -1) {
      val v = in.read()
      if (v == -1) decodeError("unexpected end of input")
      peeked = v
    }
    peeked.toByte
  }

  /** Read the next type marker byte and consume it. */
  def nextToken(): Byte = {
    val b = readRawByte()
    b
  }

  /** Read a length (used for strings and high-precision numbers). UBJson lengths are preceded by a type marker indicating
    * the integer type of the length.
    */
  private def readLength(): Int = {
    val marker = readRawByte()
    marker match {
      case 'i' => readRawByte().toInt
      case 'U' => readRawByte().toInt & 0xFF
      case 'I' => readInt16().toInt
      case 'l' => readInt32()
      case 'L' =>
        val v = readInt64()
        if (v > Int.MaxValue || v < 0) decodeError(s"string length too large: $v")
        v.toInt
      case other => decodeError(s"expected length type marker, got '${other.toChar}' (0x${"%02X".format(other)})")
    }
  }

  private def readInt16(): Short = {
    val b = readRawBytes(2)
    ((b(0) & 0xFF) << 8 | (b(1) & 0xFF)).toShort
  }

  private def readInt32(): Int = {
    val b = readRawBytes(4)
    (b(0) & 0xFF) << 24 | (b(1) & 0xFF) << 16 | (b(2) & 0xFF) << 8 | (b(3) & 0xFF)
  }

  private def readInt64(): Long = {
    val b = readRawBytes(8)
    (b(0) & 0xFFL) << 56 | (b(1) & 0xFFL) << 48 | (b(2) & 0xFFL) << 40 | (b(3) & 0xFFL) << 32 |
      (b(4) & 0xFFL) << 24 | (b(5) & 0xFFL) << 16 | (b(6) & 0xFFL) << 8 | (b(7) & 0xFFL)
  }

  // --- Public read methods ---

  def readNull(): Unit = {
    val m = readRawByte()
    if (m != 'Z') decodeError(s"expected null marker 'Z', got '${m.toChar}'")
  }

  def readBoolean(): Boolean = {
    val m = readRawByte()
    m match {
      case 'T' => true
      case 'F' => false
      case _   => decodeError(s"expected boolean marker 'T' or 'F', got '${m.toChar}'")
    }
  }

  def readByte(): Byte = {
    val m = readRawByte()
    if (m != 'i') decodeError(s"expected int8 marker 'i', got '${m.toChar}'")
    readRawByte()
  }

  def readShort(): Short = {
    val m = readRawByte()
    m match {
      case 'i' => readRawByte().toShort
      case 'U' => (readRawByte().toInt & 0xFF).toShort
      case 'I' => readInt16()
      case _   => decodeError(s"expected integer marker for short, got '${m.toChar}'")
    }
  }

  def readInt(): Int = {
    val m = readRawByte()
    m match {
      case 'i' => readRawByte().toInt
      case 'U' => readRawByte().toInt & 0xFF
      case 'I' => readInt16().toInt
      case 'l' => readInt32()
      case _   => decodeError(s"expected integer marker for int, got '${m.toChar}'")
    }
  }

  def readLong(): Long = {
    val m = readRawByte()
    m match {
      case 'i' => readRawByte().toLong
      case 'U' => (readRawByte().toInt & 0xFF).toLong
      case 'I' => readInt16().toLong
      case 'l' => readInt32().toLong
      case 'L' => readInt64()
      case _   => decodeError(s"expected integer marker for long, got '${m.toChar}'")
    }
  }

  def readFloat(): Float = {
    val m = readRawByte()
    m match {
      case 'd' =>
        val bits = readInt32()
        java.lang.Float.intBitsToFloat(bits)
      case 'i' => readRawByte().toFloat
      case 'U' => (readRawByte().toInt & 0xFF).toFloat
      case 'I' => readInt16().toFloat
      case 'l' => readInt32().toFloat
      case _   => decodeError(s"expected float marker 'd', got '${m.toChar}'")
    }
  }

  def readDouble(): Double = {
    val m = readRawByte()
    m match {
      case 'D' =>
        val bits = readInt64()
        java.lang.Double.longBitsToDouble(bits)
      case 'd' =>
        val bits = readInt32()
        java.lang.Float.intBitsToFloat(bits).toDouble
      case 'i' => readRawByte().toDouble
      case 'U' => (readRawByte().toInt & 0xFF).toDouble
      case 'I' => readInt16().toDouble
      case 'l' => readInt32().toDouble
      case 'L' => readInt64().toDouble
      case _   => decodeError(s"expected double marker 'D', got '${m.toChar}'")
    }
  }

  def readBigDecimal(): BigDecimal = {
    val m = readRawByte()
    m match {
      case 'H' =>
        val len = readLength()
        val bytes = readRawBytes(len)
        val str = new String(bytes, StandardCharsets.UTF_8)
        try BigDecimal(str)
        catch { case _: NumberFormatException => decodeError(s"invalid high-precision number: $str") }
      case 'D' =>
        val bits = readInt64()
        BigDecimal(java.lang.Double.longBitsToDouble(bits))
      case 'd' =>
        val bits = readInt32()
        BigDecimal(java.lang.Float.intBitsToFloat(bits).toDouble)
      case 'i' => BigDecimal(readRawByte().toInt)
      case 'U' => BigDecimal(readRawByte().toInt & 0xFF)
      case 'I' => BigDecimal(readInt16().toInt)
      case 'l' => BigDecimal(readInt32())
      case 'L' => BigDecimal(readInt64())
      case _   => decodeError(s"expected number marker, got '${m.toChar}'")
    }
  }

  def readString(): String = {
    val m = readRawByte()
    if (m != 'S') decodeError(s"expected string marker 'S', got '${m.toChar}'")
    readStringValue()
  }

  /** Read a string value (the part after the 'S' marker). */
  private[ubjsonderivation] def readStringValue(): String = {
    val len = readLength()
    if (len == 0) ""
    else {
      val bytes = readRawBytes(len)
      new String(bytes, StandardCharsets.UTF_8)
    }
  }

  def readChar(): Char = {
    val m = readRawByte()
    if (m != 'C') decodeError(s"expected char marker 'C', got '${m.toChar}'")
    readRawByte().toChar
  }

  // --- Container methods ---

  def readArrayStart(): Unit = {
    val m = readRawByte()
    if (m != '[') decodeError(s"expected array start '[', got '${m.toChar}'")
  }

  /** Check if the next byte is the array end marker ']'. If yes, consume it and return true. */
  def isArrayEnd(): Boolean = {
    val next = peekToken()
    if (next == ']') {
      val _ = readRawByte() // consume
      true
    } else false
  }

  def readObjectStart(): Unit = {
    val m = readRawByte()
    if (m != '{') decodeError(s"expected object start, got '${m.toChar}' (0x${"%02X".format(m)})")
  }

  /** Check if the next byte is the object end marker '}'. If yes, consume it and return true. */
  def isObjectEnd(): Boolean = {
    val next = peekToken()
    if (next == '}') {
      val _ = readRawByte() // consume
      true
    } else false
  }

  /** Read a field name. In UBJson, object keys are length-prefixed strings (without the 'S' marker). The length is
    * encoded the same way as other lengths.
    */
  def readFieldName(): String = {
    val len = readLength()
    if (len == 0) ""
    else {
      val bytes = readRawBytes(len)
      new String(bytes, StandardCharsets.UTF_8)
    }
  }

  // --- Value reading (type-dispatched) ---

  /** Read any value and discard it. */
  def skip(): Unit = {
    val m = readRawByte()
    m match {
      case 'Z' | 'N' | 'T' | 'F' => () // no payload
      case 'i'                     => { val _ = readRawByte() }
      case 'U'                     => { val _ = readRawByte() }
      case 'C'                     => { val _ = readRawByte() }
      case 'I'                     => { val _ = readRawBytes(2) }
      case 'l'                     => { val _ = readRawBytes(4) }
      case 'L'                     => { val _ = readRawBytes(8) }
      case 'd'                     => { val _ = readRawBytes(4) }
      case 'D'                     => { val _ = readRawBytes(8) }
      case 'S' | 'H' =>
        val len = readLength()
        val _ = readRawBytes(len)
      case '[' => skipArray()
      case '{' => skipObject()
      case _   => decodeError(s"unknown UBJson marker '${m.toChar}' (0x${"%02X".format(m)})")
    }
  }

  private def skipArray(): Unit =
    while (!isArrayEnd()) skip()

  private def skipObject(): Unit =
    while (!isObjectEnd()) {
      val _ = readFieldName() // skip field name
      skip() // skip value
    }

  /** Check if there is a null marker next. If yes, consume it and return true. */
  def isNextNull(): Boolean = {
    val next = peekToken()
    if (next == 'Z') {
      val _ = readRawByte() // consume
      true
    } else false
  }

  /** Read any numeric value as an Int, regardless of the integer type marker. */
  def readNumericAsInt(): Int = {
    val m = peekToken()
    m match {
      case 'i' | 'U' | 'I' | 'l' => readInt()
      case 'L'                     =>
        val v = readLong()
        if (v > Int.MaxValue || v < Int.MinValue) decodeError(s"long value $v overflows int")
        v.toInt
      case 'd' =>
        readFloat().toInt
      case 'D' =>
        readDouble().toInt
      case _ => decodeError(s"expected numeric marker, got '${m.toChar}'")
    }
  }

  /** Get the current read position. */
  def currentPosition: Int = pos
}
