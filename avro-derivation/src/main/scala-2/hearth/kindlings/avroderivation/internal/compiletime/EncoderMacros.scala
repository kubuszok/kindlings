package hearth.kindlings.avroderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[avroderivation] class EncoderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with SchemaForMacrosImpl
    with EncoderMacrosImpl {

  def deriveEncoderImpl[A: c.WeakTypeTag](
      config: c.Expr[AvroConfig]
  ): c.Expr[AvroEncoder[A]] = deriveEncoderTypeClass[A](config)

  def deriveInlineEncodeImpl[A: c.WeakTypeTag](
      value: c.Expr[A]
  )(config: c.Expr[AvroConfig]): c.Expr[Any] = deriveInlineEncode[A](value, config)
}
