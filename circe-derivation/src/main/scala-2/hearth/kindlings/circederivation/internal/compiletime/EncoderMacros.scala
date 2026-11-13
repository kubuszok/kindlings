package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommonsScala2
import io.circe.{Encoder, Json}
import scala.reflect.macros.blackbox

final private[circederivation] class EncoderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupport
    with LoadStandardExtensionsOnce
    with SemiEvalConfigScala2
    with EncoderMacrosImpl {

  def deriveEncoderImpl[A: c.WeakTypeTag](
      config: c.Expr[Configuration]
  ): c.Expr[Encoder[A]] = deriveEncoderTypeClass[A](config).asInstanceOf[c.Expr[Encoder[A]]]

  def deriveKindlingsEncoderImpl[A: c.WeakTypeTag](
      config: c.Expr[Configuration]
  ): c.Expr[KindlingsEncoder[A]] = deriveEncoderTypeClass[A](config)

  def deriveEncoderAsObjectImpl[A: c.WeakTypeTag](
      config: c.Expr[Configuration]
  ): c.Expr[Encoder.AsObject[A]] =
    deriveEncoderAsObjectTypeClass[A](config).asInstanceOf[c.Expr[Encoder.AsObject[A]]]

  def deriveUnwrappedEncoderImpl[A: c.WeakTypeTag](
      config: c.Expr[Configuration]
  ): c.Expr[KindlingsEncoder[A]] = deriveUnwrappedEncoderTypeClass[A](config)

  def deriveInlineEncodeImpl[A: c.WeakTypeTag](
      value: c.Expr[A]
  )(config: c.Expr[Configuration]): c.Expr[Json] = deriveInlineEncode[A](value, config)
}
