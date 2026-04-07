package hearth.kindlings.sconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[sconfigderivation] class ReaderMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      LoadStandardExtensionsOnce,
      ReaderMacrosImpl

private[sconfigderivation] object ReaderMacros {

  def deriveReaderImpl[A: Type](
      config: Expr[SConfig]
  )(using q: Quotes): Expr[ConfigReader[A]] =
    new ReaderMacros(q).deriveReaderTypeClass[A](config)
}
