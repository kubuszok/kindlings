package hearth.kindlings.jsoniterderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReaderException, JsonValueCodec}
import scala.quoted.*

final private[jsoniterderivation] class CodecMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      CodecMacrosImpl
private[jsoniterderivation] object CodecMacros {

  def deriveCodecImpl[A: Type](
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[JsonValueCodec[A]] =
    new CodecMacros(q).deriveCodecTypeClass[A](config)

  def deriveKindlingsCodecImpl[A: Type](
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[KindlingsJsonValueCodec[A]] =
    new CodecMacros(q).deriveCodecTypeClass[A](config)

  def deriveInlineWriteToStringImpl[A: Type](
      value: Expr[A],
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[String] =
    new CodecMacros(q).deriveInlineWriteToString[A](value, config)

  def deriveInlineReadFromStringImpl[A: Type](
      json: Expr[String],
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[Either[JsonReaderException, A]] =
    new CodecMacros(q).deriveInlineReadFromString[A](json, config)

  def deriveKeyCodecImpl[A: Type](
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[JsonKeyCodec[A]] =
    new CodecMacros(q).deriveKeyCodecTypeClass[A](config)
}
