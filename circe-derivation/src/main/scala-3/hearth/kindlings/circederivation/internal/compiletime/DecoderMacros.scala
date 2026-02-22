package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommonsScala3
import io.circe.{Decoder, DecodingFailure, HCursor, Json}
import scala.quoted.*

final private[circederivation] class DecoderMacros(q: Quotes) extends MacroCommonsScala3(using q), DecoderMacrosImpl
private[circederivation] object DecoderMacros {

  def deriveDecoderImpl[A: Type](
      config: Expr[Configuration]
  )(using q: Quotes): Expr[Decoder[A]] =
    new DecoderMacros(q).deriveDecoderTypeClass[A](config)

  def deriveKindlingsDecoderImpl[A: Type](
      config: Expr[Configuration]
  )(using q: Quotes): Expr[KindlingsDecoder[A]] =
    new DecoderMacros(q).deriveDecoderTypeClass[A](config)

  def deriveInlineDecodeImpl[A: Type](
      json: Expr[Json],
      config: Expr[Configuration]
  )(using q: Quotes): Expr[Either[DecodingFailure, A]] =
    new DecoderMacros(q).deriveInlineDecode[A](json, config)
}
