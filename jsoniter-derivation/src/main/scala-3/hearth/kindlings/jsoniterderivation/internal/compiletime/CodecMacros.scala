package hearth.kindlings.jsoniterderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import scala.quoted.*

final private[jsoniterderivation] class CodecMacros(q: Quotes) extends MacroCommonsScala3(using q), CodecMacrosImpl
private[jsoniterderivation] object CodecMacros {

  def deriveCodecImpl[A: Type](
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[JsonValueCodec[A]] =
    new CodecMacros(q).deriveCodecTypeClass[A](config)

  def deriveKindlingsCodecImpl[A: Type](
      config: Expr[JsoniterConfig]
  )(using q: Quotes): Expr[KindlingsJsonValueCodec[A]] =
    new CodecMacros(q).deriveCodecTypeClass[A](config)
}
