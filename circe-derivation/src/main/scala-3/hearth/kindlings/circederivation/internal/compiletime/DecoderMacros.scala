package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommonsScala3
import io.circe.{Decoder, DecodingFailure, HCursor, Json}
import scala.quoted.*

final private[circederivation] class DecoderMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupport,
      LoadStandardExtensionsOnce,
      SemiEvalConfigScala3,
      DecoderMacrosImpl
private[circederivation] object DecoderMacros {

  def deriveDecoderImpl[A: Type](
      config: Expr[Configuration]
  )(using q: Quotes): Expr[Decoder[A]] =
    new DecoderMacros(q).deriveDecoderTypeClass[A](config)

  def deriveKindlingsDecoderImpl[A: Type](
      config: Expr[Configuration]
  )(using q: Quotes): Expr[KindlingsDecoder[A]] =
    new DecoderMacros(q).deriveDecoderTypeClass[A](config)

  def derivePatchDecoderImpl[A: Type](
      config: Expr[Configuration]
  )(using q: Quotes): Expr[Decoder[A => A]] =
    new DecoderMacros(q).derivePatchDecoderTypeClass[A](config)

  def deriveIncompleteDecoderImpl[F: Type](
      config: Expr[Configuration]
  )(using q: Quotes): Expr[Decoder[F]] =
    new DecoderMacros(q).deriveIncompleteDecoderTypeClass[F](config)

  def deriveUnwrappedDecoderImpl[A: Type](
      config: Expr[Configuration]
  )(using q: Quotes): Expr[KindlingsDecoder[A]] =
    new DecoderMacros(q).deriveUnwrappedDecoderTypeClass[A](config)

  def deriveInlineDecodeImpl[A: Type](
      json: Expr[Json],
      config: Expr[Configuration]
  )(using q: Quotes): Expr[Either[DecodingFailure, A]] =
    new DecoderMacros(q).deriveInlineDecode[A](json, config)
}
