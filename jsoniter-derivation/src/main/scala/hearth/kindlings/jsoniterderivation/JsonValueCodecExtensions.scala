package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}

object JsonValueCodecExtensions {

  implicit class JsonValueCodecOps[A](private val codec: JsonValueCodec[A]) extends AnyVal {

    def map[B](f: A => B)(g: B => A): JsonValueCodec[B] =
      new JsonValueCodec[B] {
        val nullValue: B = {
          val a = codec.nullValue
          if (a == null) null.asInstanceOf[B] else f(a)
        }
        def decodeValue(in: JsonReader, default: B): B = f(codec.decodeValue(in, codec.nullValue))
        def encodeValue(x: B, out: JsonWriter): Unit = codec.encodeValue(g(x), out)
      }

    def mapDecode[B](f: A => Either[String, B])(g: B => A): JsonValueCodec[B] =
      new JsonValueCodec[B] {
        val nullValue: B = {
          val a = codec.nullValue
          if (a == null) null.asInstanceOf[B]
          else
            f(a) match {
              case Right(b) => b
              case Left(_)  => null.asInstanceOf[B]
            }
        }
        def decodeValue(in: JsonReader, default: B): B = {
          val a = codec.decodeValue(in, codec.nullValue)
          f(a) match {
            case Right(b)  => b
            case Left(msg) => in.decodeError(msg)
          }
        }
        def encodeValue(x: B, out: JsonWriter): Unit = codec.encodeValue(g(x), out)
      }
  }
}
