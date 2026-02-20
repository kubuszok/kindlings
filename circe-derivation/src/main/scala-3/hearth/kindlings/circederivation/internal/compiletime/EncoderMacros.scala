package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommonsScala3
import io.circe.{Encoder, Json}
import scala.quoted.*

final private[circederivation] class EncoderMacros(q: Quotes) extends MacroCommonsScala3(using q), EncoderMacrosImpl
private[circederivation] object EncoderMacros {

  def deriveEncoderImpl[A: Type](
      config: Expr[Configuration]
  )(using q: Quotes): Expr[Encoder[A]] =
    new EncoderMacros(q).deriveEncoderTypeClass[A](config)

  def deriveKindlingsEncoderImpl[A: Type](
      config: Expr[Configuration]
  )(using q: Quotes): Expr[KindlingsEncoder[A]] =
    new EncoderMacros(q).deriveEncoderTypeClass[A](config)

  def deriveInlineEncodeImpl[A: Type](
      value: Expr[A],
      config: Expr[Configuration]
  )(using q: Quotes): Expr[Json] =
    new EncoderMacros(q).deriveInlineEncode[A](value, config)
}
