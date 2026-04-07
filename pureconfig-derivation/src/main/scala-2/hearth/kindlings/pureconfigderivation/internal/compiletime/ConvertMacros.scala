package hearth.kindlings.pureconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import pureconfig.ConfigConvert
import scala.reflect.macros.blackbox

final private[pureconfigderivation] class ConvertMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with LoadStandardExtensionsOnce
    with ReaderMacrosImpl
    with WriterMacrosImpl
    with ConvertMacrosImpl {

  def deriveConvertImpl[A: c.WeakTypeTag](
      config: c.Expr[PureConfig]
  ): c.Expr[ConfigConvert[A]] = deriveConvertTypeClass[A](config).asInstanceOf[c.Expr[ConfigConvert[A]]]

  def deriveKindlingsConvertImpl[A: c.WeakTypeTag](
      config: c.Expr[PureConfig]
  ): c.Expr[KindlingsConfigConvert[A]] = deriveConvertTypeClass[A](config)
}
