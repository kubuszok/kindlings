package hearth.kindlings.sconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[sconfigderivation] class WriterMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with LoadStandardExtensionsOnce
    with WriterMacrosImpl {

  def deriveWriterImpl[A: c.WeakTypeTag](
      config: c.Expr[SConfig]
  ): c.Expr[ConfigWriter[A]] = deriveWriterTypeClass[A](config)
}
