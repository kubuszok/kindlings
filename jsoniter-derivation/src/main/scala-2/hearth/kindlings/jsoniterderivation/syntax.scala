package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException
import scala.language.experimental.macros

object syntax {

  implicit class JsoniterWriteOps[A](private val value: A) extends AnyVal {
    def toJsonString(implicit config: JsoniterConfig): String =
      macro internal.compiletime.CodecMacros.deriveInlineWriteToStringOpsImpl[A]
  }

  implicit class JsoniterReadOps(private val json: String) extends AnyVal {
    def fromJsonString[A](implicit config: JsoniterConfig): Either[JsonReaderException, A] =
      macro internal.compiletime.CodecMacros.deriveInlineReadFromStringOpsImpl[A]
  }
}
