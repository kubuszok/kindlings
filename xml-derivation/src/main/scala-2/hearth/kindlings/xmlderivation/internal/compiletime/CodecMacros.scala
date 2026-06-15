package hearth.kindlings.xmlderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[xmlderivation] class CodecMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupport
    with EncoderMacrosImpl
    with DecoderMacrosImpl
    with CodecMacrosImpl {

  def deriveCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[XmlConfig]
  ): c.Expr[KindlingsXmlCodec[A]] =
    deriveCodecTypeClass[A](config)

  def deriveKindlingsCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[XmlConfig]
  ): c.Expr[KindlingsXmlCodec[A]] =
    deriveCodecTypeClass[A](config)
}
