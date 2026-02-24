package hearth.kindlings.avroderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[avroderivation] class DecoderMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      SchemaForMacrosImpl,
      DecoderMacrosImpl
private[avroderivation] object DecoderMacros {

  def deriveDecoderImpl[A: Type](
      config: Expr[AvroConfig]
  )(using q: Quotes): Expr[AvroDecoder[A]] =
    new DecoderMacros(q).deriveDecoderTypeClass[A](config)

  def deriveInlineDecodeImpl[A: Type](
      value: Expr[Any],
      config: Expr[AvroConfig]
  )(using q: Quotes): Expr[A] =
    new DecoderMacros(q).deriveInlineDecode[A](value, config)
}
