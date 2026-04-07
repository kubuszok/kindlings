package hearth.kindlings.pureconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import pureconfig.ConfigWriter
import scala.quoted.*

final private[pureconfigderivation] class WriterMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      LoadStandardExtensionsOnce,
      WriterMacrosImpl

private[pureconfigderivation] object WriterMacros {

  def deriveWriterImpl[A: Type](
      config: Expr[PureConfig]
  )(using q: Quotes): Expr[ConfigWriter[A]] =
    new WriterMacros(q).deriveWriterTypeClass[A](config)

  def deriveKindlingsWriterImpl[A: Type](
      config: Expr[PureConfig]
  )(using q: Quotes): Expr[KindlingsConfigWriter[A]] =
    new WriterMacros(q).deriveWriterTypeClass[A](config)
}
