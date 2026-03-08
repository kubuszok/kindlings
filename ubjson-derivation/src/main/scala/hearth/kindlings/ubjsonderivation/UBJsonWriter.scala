package hearth.kindlings.ubjsonderivation

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.charset.StandardCharsets

/** A streaming UBJson writer that writes to a byte array or output stream.
  *
  * UBJson type markers:
  *   - Z = null, N = noop, T = true, F = false
  *   - i = int8, U = uint8, I = int16, l = int32, L = int64
  *   - d = float32, D = float64, H = high-precision number (string)
  *   - C = char, S = string (length-prefixed)
  *   - [ / ] = array start/end, { / } = object start/end
  */
final class UBJsonWriter(private val out: OutputStream) {

  def this() = this(new ByteArrayOutputStream(256))

  private val baos: ByteArrayOutputStream = out match {
    case b: ByteArrayOutputStream => b
    case _                        => null
  }

  // --- Low-level writing ---

  private def writeByte(b: Byte): Unit = out.write(b.toInt)

  private def writeBytes(bytes: Array[Byte]): Unit = out.write(bytes)

  private def writeInt16(v: Short): Unit = {
    out.write((v >> 8) & 0xFF)
    out.write(v & 0xFF)
  }

  private def writeInt32(v: Int): Unit = {
    out.write((v >> 24) & 0xFF)
    out.write((v >> 16) & 0xFF)
    out.write((v >> 8) & 0xFF)
    out.write(v & 0xFF)
  }

  private def writeInt64(v: Long): Unit = {
    out.write(((v >> 56) & 0xFF).toInt)
    out.write(((v >> 48) & 0xFF).toInt)
    out.write(((v >> 40) & 0xFF).toInt)
    out.write(((v >> 32) & 0xFF).toInt)
    out.write(((v >> 24) & 0xFF).toInt)
    out.write(((v >> 16) & 0xFF).toInt)
    out.write(((v >> 8) & 0xFF).toInt)
    out.write((v & 0xFF).toInt)
  }

  /** Write a length-prefixed integer using the smallest possible type. */
  private def writeLength(len: Int): Unit =
    if (len >= 0 && len <= 127) {
      writeByte('i')
      writeByte(len.toByte)
    } else if (len >= 0 && len <= 255) {
      writeByte('U')
      writeByte(len.toByte)
    } else if (len >= Short.MinValue && len <= Short.MaxValue) {
      writeByte('I')
      writeInt16(len.toShort)
    } else {
      writeByte('l')
      writeInt32(len)
    }

  // --- Public write methods ---

  def writeNull(): Unit = writeByte('Z')

  def writeBoolean(v: Boolean): Unit =
    if (v) writeByte('T') else writeByte('F')

  def writeByte(marker: Char, v: Byte): Unit = {
    writeByte(marker.toByte)
    writeByte(v)
  }

  def writeInt8(v: Byte): Unit = {
    writeByte('i')
    writeByte(v)
  }

  def writeUInt8(v: Int): Unit = {
    writeByte('U')
    writeByte((v & 0xFF).toByte)
  }

  def writeInt(v: Int): Unit =
    if (v >= Byte.MinValue && v <= Byte.MaxValue) {
      writeByte('i')
      writeByte(v.toByte)
    } else if (v >= 0 && v <= 255) {
      writeByte('U')
      writeByte(v.toByte)
    } else if (v >= Short.MinValue && v <= Short.MaxValue) {
      writeByte('I')
      writeInt16(v.toShort)
    } else {
      writeByte('l')
      writeInt32(v)
    }

  def writeLong(v: Long): Unit =
    if (v >= Byte.MinValue && v <= Byte.MaxValue) {
      writeByte('i')
      writeByte(v.toByte)
    } else if (v >= 0 && v <= 255) {
      writeByte('U')
      writeByte(v.toByte)
    } else if (v >= Short.MinValue && v <= Short.MaxValue) {
      writeByte('I')
      writeInt16(v.toShort)
    } else if (v >= Int.MinValue && v <= Int.MaxValue) {
      writeByte('l')
      writeInt32(v.toInt)
    } else {
      writeByte('L')
      writeInt64(v)
    }

  def writeFloat(v: Float): Unit = {
    writeByte('d')
    writeInt32(java.lang.Float.floatToIntBits(v))
  }

  def writeDouble(v: Double): Unit = {
    writeByte('D')
    writeInt64(java.lang.Double.doubleToLongBits(v))
  }

  def writeBigDecimal(v: BigDecimal): Unit = {
    val str = v.underlying().toPlainString
    val bytes = str.getBytes(StandardCharsets.UTF_8)
    writeByte('H')
    writeLength(bytes.length)
    writeBytes(bytes)
  }

  def writeString(v: String): Unit = {
    val bytes = v.getBytes(StandardCharsets.UTF_8)
    writeByte('S')
    writeLength(bytes.length)
    writeBytes(bytes)
  }

  def writeChar(v: Char): Unit = {
    writeByte('C')
    writeByte(v.toByte)
  }

  // --- Container methods ---

  def writeArrayStart(): Unit = writeByte('[')

  def writeArrayEnd(): Unit = writeByte(']')

  def writeObjectStart(): Unit = writeByte('{')

  def writeObjectEnd(): Unit = writeByte('}')

  /** Write a field name. In UBJson, object keys are length-prefixed strings (without the 'S' marker). */
  def writeFieldName(name: String): Unit = {
    val bytes = name.getBytes(StandardCharsets.UTF_8)
    writeLength(bytes.length)
    writeBytes(bytes)
  }

  // --- Output ---

  /** Get the written bytes. Only works when the writer was constructed without an explicit output stream. */
  def toByteArray: Array[Byte] =
    if (baos != null) baos.toByteArray
    else throw new UnsupportedOperationException("toByteArray is only available when writing to internal buffer")

  /** Flush the underlying output stream. */
  def flush(): Unit = out.flush()

  /** Reset the internal buffer. Only works when writing to internal buffer. */
  def reset(): Unit =
    if (baos != null) baos.reset()
    else throw new UnsupportedOperationException("reset is only available when writing to internal buffer")
}
