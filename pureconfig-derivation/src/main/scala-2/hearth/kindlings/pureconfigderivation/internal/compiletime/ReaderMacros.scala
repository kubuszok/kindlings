package hearth.kindlings.pureconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import pureconfig.ConfigReader
import scala.reflect.macros.blackbox

final private[pureconfigderivation] class ReaderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with LoadStandardExtensionsOnce
    with ReaderMacrosImpl {

  def deriveReaderImpl[A: c.WeakTypeTag](
      config: c.Expr[PureConfig]
  ): c.Expr[ConfigReader[A]] = deriveReaderTypeClass[A](config).asInstanceOf[c.Expr[ConfigReader[A]]]

  def deriveKindlingsReaderImpl[A: c.WeakTypeTag](
      config: c.Expr[PureConfig]
  ): c.Expr[KindlingsConfigReader[A]] = deriveReaderTypeClass[A](config)
}
