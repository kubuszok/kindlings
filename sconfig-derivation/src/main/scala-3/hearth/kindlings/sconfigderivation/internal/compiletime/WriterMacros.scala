package hearth.kindlings.sconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[sconfigderivation] class WriterMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      LoadStandardExtensionsOnce,
      WriterMacrosImpl

private[sconfigderivation] object WriterMacros {

  def deriveWriterImpl[A: Type](
      config: Expr[SConfig]
  )(using q: Quotes): Expr[ConfigWriter[A]] =
    new WriterMacros(q).deriveWriterTypeClass[A](config)
}
