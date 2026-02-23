package hearth.kindlings.jsoniterderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import scala.reflect.macros.blackbox

final private[jsoniterderivation] class CodecMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CodecMacrosImpl {

  def deriveCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[JsonValueCodec[A]] = deriveCodecTypeClass[A](config).asInstanceOf[c.Expr[JsonValueCodec[A]]]

  def deriveKindlingsCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[JsoniterConfig]
  ): c.Expr[KindlingsJsonValueCodec[A]] = deriveCodecTypeClass[A](config)
}
