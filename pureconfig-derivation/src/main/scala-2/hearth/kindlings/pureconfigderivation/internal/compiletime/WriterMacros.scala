package hearth.kindlings.pureconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import pureconfig.ConfigWriter
import scala.reflect.macros.blackbox

final private[pureconfigderivation] class WriterMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with LoadStandardExtensionsOnce
    with WriterMacrosImpl {

  def deriveWriterImpl[A: c.WeakTypeTag](
      config: c.Expr[PureConfig]
  ): c.Expr[ConfigWriter[A]] = deriveWriterTypeClass[A](config).asInstanceOf[c.Expr[ConfigWriter[A]]]

  def deriveKindlingsWriterImpl[A: c.WeakTypeTag](
      config: c.Expr[PureConfig]
  ): c.Expr[KindlingsConfigWriter[A]] = deriveWriterTypeClass[A](config)
}
