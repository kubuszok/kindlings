package hearth.kindlings.ubjsonderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[ubjsonderivation] class CodecMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      CodecMacrosImpl
private[ubjsonderivation] object CodecMacros {

  def deriveCodecImpl[A: Type](
      config: Expr[UBJsonConfig]
  )(using q: Quotes): Expr[UBJsonValueCodec[A]] =
    new CodecMacros(q).deriveCodecTypeClass[A](config)

  def deriveKindlingsCodecImpl[A: Type](
      config: Expr[UBJsonConfig]
  )(using q: Quotes): Expr[KindlingsUBJsonValueCodec[A]] =
    new CodecMacros(q).deriveCodecTypeClass[A](config)
}
