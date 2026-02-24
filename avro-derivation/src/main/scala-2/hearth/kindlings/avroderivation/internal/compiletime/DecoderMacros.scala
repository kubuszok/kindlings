package hearth.kindlings.avroderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[avroderivation] class DecoderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with SchemaForMacrosImpl
    with DecoderMacrosImpl {

  def deriveDecoderImpl[A: c.WeakTypeTag](
      config: c.Expr[AvroConfig]
  ): c.Expr[AvroDecoder[A]] = deriveDecoderTypeClass[A](config)

  def deriveInlineDecodeImpl[A: c.WeakTypeTag](
      value: c.Expr[Any]
  )(config: c.Expr[AvroConfig]): c.Expr[A] = deriveInlineDecode[A](value, config)
}
