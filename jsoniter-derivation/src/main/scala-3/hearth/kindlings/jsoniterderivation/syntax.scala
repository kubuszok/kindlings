package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException

object syntax {

  extension [A](value: A) {
    inline def toJsonString(using config: JsoniterConfig): String = ${
      internal.compiletime.CodecMacros.deriveInlineWriteToStringImpl[A]('value, 'config)
    }
  }

  extension (json: String) {
    inline def fromJsonString[A](using config: JsoniterConfig): Either[JsonReaderException, A] = ${
      internal.compiletime.CodecMacros.deriveInlineReadFromStringImpl[A]('json, 'config)
    }
  }
}
