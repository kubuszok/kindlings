package hearth.kindlings.sconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[sconfigderivation] class ReaderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with LoadStandardExtensionsOnce
    with ReaderMacrosImpl {

  def deriveReaderImpl[A: c.WeakTypeTag](
      config: c.Expr[SConfig]
  ): c.Expr[ConfigReader[A]] = deriveReaderTypeClass[A](config)
}
