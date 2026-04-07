package hearth.kindlings.pureconfigderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import pureconfig.ConfigConvert
import scala.quoted.*

final private[pureconfigderivation] class ConvertMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      LoadStandardExtensionsOnce,
      ReaderMacrosImpl,
      WriterMacrosImpl,
      ConvertMacrosImpl

private[pureconfigderivation] object ConvertMacros {

  def deriveConvertImpl[A: Type](
      config: Expr[PureConfig]
  )(using q: Quotes): Expr[ConfigConvert[A]] =
    new ConvertMacros(q).deriveConvertTypeClass[A](config)

  def deriveKindlingsConvertImpl[A: Type](
      config: Expr[PureConfig]
  )(using q: Quotes): Expr[KindlingsConfigConvert[A]] =
    new ConvertMacros(q).deriveConvertTypeClass[A](config)
}
