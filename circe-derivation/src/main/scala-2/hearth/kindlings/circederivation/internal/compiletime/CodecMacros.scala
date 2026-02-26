package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommonsScala2
import io.circe.Codec
import scala.reflect.macros.blackbox

final private[circederivation] class CodecMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with EncoderMacrosImpl
    with DecoderMacrosImpl
    with CodecMacrosImpl {

  def deriveCodecAsObjectImpl[A: c.WeakTypeTag](
      config: c.Expr[Configuration]
  ): c.Expr[Codec.AsObject[A]] =
    deriveCodecAsObjectTypeClass[A](config).asInstanceOf[c.Expr[Codec.AsObject[A]]]

  def deriveKindlingsCodecAsObjectImpl[A: c.WeakTypeTag](
      config: c.Expr[Configuration]
  ): c.Expr[KindlingsCodecAsObject[A]] =
    deriveCodecAsObjectTypeClass[A](config).asInstanceOf[c.Expr[KindlingsCodecAsObject[A]]]
}
