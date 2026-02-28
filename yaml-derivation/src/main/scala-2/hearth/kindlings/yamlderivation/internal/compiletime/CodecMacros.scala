package hearth.kindlings.yamlderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[yamlderivation] class CodecMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with EncoderMacrosImpl
    with DecoderMacrosImpl
    with CodecMacrosImpl {

  def deriveCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[YamlConfig]
  ): c.Expr[KindlingsYamlCodec[A]] =
    deriveCodecTypeClass[A](config)

  def deriveKindlingsCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[YamlConfig]
  ): c.Expr[KindlingsYamlCodec[A]] =
    deriveCodecTypeClass[A](config)
}
