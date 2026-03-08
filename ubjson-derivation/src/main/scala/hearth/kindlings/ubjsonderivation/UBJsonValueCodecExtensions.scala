package hearth.kindlings.ubjsonderivation

object UBJsonValueCodecExtensions {

  implicit class UBJsonValueCodecOps[A](private val codec: UBJsonValueCodec[A]) extends AnyVal {

    def map[B](f: A => B)(g: B => A): UBJsonValueCodec[B] =
      new UBJsonValueCodec[B] {
        val nullValue: B = {
          val a = codec.nullValue
          if (a == null) null.asInstanceOf[B] else f(a)
        }
        def decode(reader: UBJsonReader): B = f(codec.decode(reader))
        def encode(writer: UBJsonWriter, value: B): Unit = codec.encode(writer, g(value))
      }

    def mapDecode[B](f: A => Either[String, B])(g: B => A): UBJsonValueCodec[B] =
      new UBJsonValueCodec[B] {
        val nullValue: B = {
          val a = codec.nullValue
          if (a == null) null.asInstanceOf[B]
          else
            f(a) match {
              case Right(b) => b
              case Left(_)  => null.asInstanceOf[B]
            }
        }
        def decode(reader: UBJsonReader): B = {
          val a = codec.decode(reader)
          f(a) match {
            case Right(b)  => b
            case Left(msg) => reader.decodeError(msg)
          }
        }
        def encode(writer: UBJsonWriter, value: B): Unit = codec.encode(writer, g(value))
      }
  }
}
