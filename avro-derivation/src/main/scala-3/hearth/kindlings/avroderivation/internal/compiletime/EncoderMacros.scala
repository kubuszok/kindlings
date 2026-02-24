package hearth.kindlings.avroderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[avroderivation] class EncoderMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      SchemaForMacrosImpl,
      EncoderMacrosImpl
private[avroderivation] object EncoderMacros {

  def deriveEncoderImpl[A: Type](
      config: Expr[AvroConfig]
  )(using q: Quotes): Expr[AvroEncoder[A]] =
    new EncoderMacros(q).deriveEncoderTypeClass[A](config)

  def deriveInlineEncodeImpl[A: Type](
      value: Expr[A],
      config: Expr[AvroConfig]
  )(using q: Quotes): Expr[Any] =
    new EncoderMacros(q).deriveInlineEncode[A](value, config)
}
