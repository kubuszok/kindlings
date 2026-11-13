package hearth.kindlings.jsoniterderivation.internal.runtime

import hearth.kindlings.jsoniterderivation.{KindlingsJsonCodec, KindlingsJsonValueCodec}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter}

object JsoniterDerivationFactories {

  /** `nullVal` is by-name: it is evaluated on each `nullValue` call (it may refer to codecs initialized later). */
  def lazyCodecInstance[A](
      nullVal: => A,
      decodeFn: (JsonReader, A) => A,
      encodeFn: (A, JsonWriter) => Unit
  ): KindlingsJsonValueCodec[A] = new KindlingsJsonValueCodec[A] {
    def nullValue: A = nullVal
    def decodeValue(in: JsonReader, default: A): A = decodeFn(in, default)
    def encodeValue(x: A, out: JsonWriter): Unit = encodeFn(x, out)
  }

  /** `nullVal` is by-name: it is evaluated on each `nullValue` call (it may refer to codecs initialized later). */
  def lazyJsonCodecInstance[A](
      nullVal: => A,
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

  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; the macro emits lazyCodecInstance",
    "0.3.4"
  )
  def codecInstance[A](
      nullVal: A,
      decodeFn: (JsonReader, A) => A,
      encodeFn: (A, JsonWriter) => Unit
  ): KindlingsJsonValueCodec[A] = new KindlingsJsonValueCodec[A] {
    def nullValue: A = nullVal
    def decodeValue(in: JsonReader, default: A): A = decodeFn(in, default)
    def encodeValue(x: A, out: JsonWriter): Unit = encodeFn(x, out)
  }

  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; the macro emits lazyJsonCodecInstance",
    "0.3.4"
  )
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
