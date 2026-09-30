package hearth.kindlings.parser
package internal.runtime

/** Primitive values on the parse stack: generated grammars keep the values of non-terminals declared with a primitive
  * type in a `Long` stack (`Machine.stackPrims`), as bits, instead of boxing them. Kinds are per non-terminal
  * (`Tables.ntPrim`); `Boxed` values stay in the value stack.
  */
object Prims {
  final val Boxed = 0
  final val IntKind = 1
  final val LongKind = 2
  final val DoubleKind = 3
  final val FloatKind = 4
  final val BooleanKind = 5
  final val CharKind = 6
  final val ShortKind = 7
  final val ByteKind = 8

  /** The bits of a boxed primitive of `kind` (generic paths: resuming after an effect). */
  def encode(kind: Int, value: Any): Long = (kind: @scala.annotation.switch) match {
    case IntKind     => value.asInstanceOf[Int].toLong
    case LongKind    => value.asInstanceOf[Long]
    case DoubleKind  => java.lang.Double.doubleToRawLongBits(value.asInstanceOf[Double])
    case FloatKind   => java.lang.Float.floatToRawIntBits(value.asInstanceOf[Float]).toLong
    case BooleanKind => if (value.asInstanceOf[Boolean]) 1L else 0L
    case CharKind    => value.asInstanceOf[Char].toLong
    case ShortKind   => value.asInstanceOf[Short].toLong
    case _           => value.asInstanceOf[Byte].toLong
  }

  /** The boxed value of `bits` of `kind` (generic paths: the parse result). */
  def decode(kind: Int, bits: Long): Any = (kind: @scala.annotation.switch) match {
    case IntKind     => bits.toInt
    case LongKind    => bits
    case DoubleKind  => java.lang.Double.longBitsToDouble(bits)
    case FloatKind   => java.lang.Float.intBitsToFloat(bits.toInt)
    case BooleanKind => bits != 0L
    case CharKind    => bits.toChar
    case ShortKind   => bits.toShort
    case _           => bits.toByte
  }
}
