package hearth.kindlings.sconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[sconfigderivation] class CodecMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with LoadStandardExtensionsOnce
    with ReaderMacrosImpl
    with WriterMacrosImpl
    with CodecMacrosImpl {

  def deriveCodecImpl[A: c.WeakTypeTag](
      config: c.Expr[SConfig]
  ): c.Expr[ConfigCodec[A]] = deriveCodecTypeClass[A](config)
}
