package hearth.kindlings.ubjsonderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[ubjsonderivation] class CodecMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with CodecMacrosImpl {

  def deriveCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[UBJsonConfig]
  ): c.Expr[UBJsonValueCodec[A]] = deriveCodecTypeClass[A](config)
}
