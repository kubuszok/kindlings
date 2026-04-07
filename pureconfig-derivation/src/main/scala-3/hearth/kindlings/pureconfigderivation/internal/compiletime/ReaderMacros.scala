package hearth.kindlings.pureconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import pureconfig.ConfigReader
import scala.quoted.*

final private[pureconfigderivation] class ReaderMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      LoadStandardExtensionsOnce,
      ReaderMacrosImpl

private[pureconfigderivation] object ReaderMacros {

  def deriveReaderImpl[A: Type](
      config: Expr[PureConfig]
  )(using q: Quotes): Expr[ConfigReader[A]] =
    new ReaderMacros(q).deriveReaderTypeClass[A](config)

  def deriveKindlingsReaderImpl[A: Type](
      config: Expr[PureConfig]
  )(using q: Quotes): Expr[KindlingsConfigReader[A]] =
    new ReaderMacros(q).deriveReaderTypeClass[A](config)
}
