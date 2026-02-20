package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommonsScala2
import io.circe.{Decoder, DecodingFailure, HCursor}
import scala.reflect.macros.blackbox

final private[circederivation] class DecoderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with DecoderMacrosImpl {

  def deriveDecoderImpl[A: c.WeakTypeTag](
      config: c.Expr[Configuration]
  ): c.Expr[Decoder[A]] = deriveDecoderTypeClass[A](config).asInstanceOf[c.Expr[Decoder[A]]]

  def deriveKindlingsDecoderImpl[A: c.WeakTypeTag](
      config: c.Expr[Configuration]
  ): c.Expr[KindlingsDecoder[A]] = deriveDecoderTypeClass[A](config)

  def deriveInlineDecodeImpl[A: c.WeakTypeTag](
      cursor: c.Expr[HCursor]
  )(config: c.Expr[Configuration]): c.Expr[Either[DecodingFailure, A]] = deriveInlineDecode[A](cursor, config)
}
