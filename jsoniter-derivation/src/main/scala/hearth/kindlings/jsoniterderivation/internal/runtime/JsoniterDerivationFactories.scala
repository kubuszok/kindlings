package hearth.kindlings.jsoniterderivation.internal.runtime

import hearth.kindlings.jsoniterderivation.{KindlingsJsonCodec, KindlingsJsonValueCodec}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter}

object JsoniterDerivationFactories {

  def codecInstance[A](
      nullVal: A,
      decodeFn: (JsonReader, A) => A,
      encodeFn: (A, JsonWriter) => Unit
  ): KindlingsJsonValueCodec[A] = new KindlingsJsonValueCodec[A] {
    def nullValue: A = nullVal
    def decodeValue(in: JsonReader, default: A): A = decodeFn(in, default)
    def encodeValue(x: A, out: JsonWriter): Unit = encodeFn(x, out)
  }

  def jsonCodecInstance[A](
      nullVal: A,
      decodeFn: (JsonReader, A) => A,
      encodeFn: (A, JsonWriter) => Unit,
      decodeKeyFn: JsonReader => A,
      encodeKeyFn: (A, JsonWriter) => Unit
  ): KindlingsJsonCodec[A] = new KindlingsJsonCodec[A] {
    def nullValue: A = nullVal
    def decodeValue(in: JsonReader, default: A): A = decodeFn(in, default)
    def encodeValue(x: A, out: JsonWriter): Unit = encodeFn(x, out)
    def decodeKey(in: JsonReader): A = decodeKeyFn(in)
    def encodeKey(x: A, out: JsonWriter): Unit = encodeKeyFn(x, out)
  }
}
