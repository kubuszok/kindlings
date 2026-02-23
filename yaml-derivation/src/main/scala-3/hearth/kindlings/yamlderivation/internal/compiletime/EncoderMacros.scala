package hearth.kindlings.yamlderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import org.virtuslab.yaml.{Node, YamlEncoder}
import scala.quoted.*

final private[yamlderivation] class EncoderMacros(q: Quotes) extends MacroCommonsScala3(using q), EncoderMacrosImpl
private[yamlderivation] object EncoderMacros {

  def deriveEncoderImpl[A: Type](
      config: Expr[YamlConfig]
  )(using q: Quotes): Expr[YamlEncoder[A]] =
    new EncoderMacros(q).deriveEncoderTypeClass[A](config)

  def deriveKindlingsEncoderImpl[A: Type](
      config: Expr[YamlConfig]
  )(using q: Quotes): Expr[KindlingsYamlEncoder[A]] =
    new EncoderMacros(q).deriveEncoderTypeClass[A](config)

  def deriveInlineEncodeImpl[A: Type](
      value: Expr[A],
      config: Expr[YamlConfig]
  )(using q: Quotes): Expr[Node] =
    new EncoderMacros(q).deriveInlineEncode[A](value, config)

  def deriveInlineToYamlStringImpl[A: Type](
      value: Expr[A],
      config: Expr[YamlConfig]
  )(using q: Quotes): Expr[String] =
    new EncoderMacros(q).deriveInlineToYamlString[A](value, config)
}
