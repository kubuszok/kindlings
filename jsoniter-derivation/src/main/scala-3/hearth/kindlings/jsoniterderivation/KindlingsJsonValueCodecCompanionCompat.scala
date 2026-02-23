package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReaderException, JsonValueCodec}

private[jsoniterderivation] trait KindlingsJsonValueCodecCompanionCompat {
  this: KindlingsJsonValueCodec.type =>

  inline def derive[A](using config: JsoniterConfig): JsonValueCodec[A] = ${
    internal.compiletime.CodecMacros.deriveCodecImpl[A]('config)
  }

  inline given derived[A](using config: JsoniterConfig): KindlingsJsonValueCodec[A] = ${
    internal.compiletime.CodecMacros.deriveKindlingsCodecImpl[A]('config)
  }

  inline def writeToString[A](inline value: A)(using config: JsoniterConfig): String = ${
    internal.compiletime.CodecMacros.deriveInlineWriteToStringImpl[A]('value, 'config)
  }

  inline def readFromString[A](json: String)(using config: JsoniterConfig): Either[JsonReaderException, A] = ${
    internal.compiletime.CodecMacros.deriveInlineReadFromStringImpl[A]('json, 'config)
  }
}
