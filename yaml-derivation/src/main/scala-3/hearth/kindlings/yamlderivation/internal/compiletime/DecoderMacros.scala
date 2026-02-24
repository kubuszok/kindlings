package hearth.kindlings.yamlderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import org.virtuslab.yaml.{ConstructError, Node, YamlDecoder, YamlError}
import scala.quoted.*

final private[yamlderivation] class DecoderMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      DecoderMacrosImpl
private[yamlderivation] object DecoderMacros {

  def deriveDecoderImpl[A: Type](
      config: Expr[YamlConfig]
  )(using q: Quotes): Expr[YamlDecoder[A]] =
    new DecoderMacros(q).deriveDecoderTypeClass[A](config)

  def deriveKindlingsDecoderImpl[A: Type](
      config: Expr[YamlConfig]
  )(using q: Quotes): Expr[KindlingsYamlDecoder[A]] =
    new DecoderMacros(q).deriveDecoderTypeClass[A](config)

  def deriveInlineDecodeImpl[A: Type](
      node: Expr[Node],
      config: Expr[YamlConfig]
  )(using q: Quotes): Expr[Either[ConstructError, A]] =
    new DecoderMacros(q).deriveInlineDecode[A](node, config)

  def deriveInlineFromYamlStringImpl[A: Type](
      yaml: Expr[String],
      config: Expr[YamlConfig]
  )(using q: Quotes): Expr[Either[YamlError, A]] =
    new DecoderMacros(q).deriveInlineFromYamlString[A](yaml, config)
}
