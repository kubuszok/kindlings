package hearth.kindlings.yamlderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import org.virtuslab.yaml.{ConstructError, Node, YamlDecoder, YamlError}
import scala.reflect.macros.blackbox

final private[yamlderivation] class DecoderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with DecoderMacrosImpl {

  def deriveDecoderImpl[A: c.WeakTypeTag](
      config: c.Expr[YamlConfig]
  ): c.Expr[YamlDecoder[A]] = deriveDecoderTypeClass[A](config).asInstanceOf[c.Expr[YamlDecoder[A]]]

  def deriveKindlingsDecoderImpl[A: c.WeakTypeTag](
      config: c.Expr[YamlConfig]
  ): c.Expr[KindlingsYamlDecoder[A]] = deriveDecoderTypeClass[A](config)

  def deriveInlineDecodeImpl[A: c.WeakTypeTag](
      node: c.Expr[Node]
  )(config: c.Expr[YamlConfig]): c.Expr[Either[ConstructError, A]] = deriveInlineDecode[A](node, config)

  def deriveInlineFromYamlStringImpl[A: c.WeakTypeTag](
      yaml: c.Expr[String]
  )(config: c.Expr[YamlConfig]): c.Expr[Either[YamlError, A]] = deriveInlineFromYamlString[A](yaml, config)

  @scala.annotation.nowarn("msg=unchecked")
  def deriveInlineFromYamlStringOpsImpl[A: c.WeakTypeTag](
      config: c.Expr[YamlConfig]
  ): c.Expr[Either[YamlError, A]] = {
    val yaml = c.Expr[String](c.prefix.tree match {
      case c.universe.Apply(_, List(arg)) => arg
      case other                          => c.abort(c.enclosingPosition, s"Unexpected prefix: $other")
    })
    deriveInlineFromYamlString[A](yaml, config)
  }
}
