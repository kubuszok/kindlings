package hearth.kindlings.yamlderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import org.virtuslab.yaml.{Node, YamlEncoder}
import scala.reflect.macros.blackbox

final private[yamlderivation] class EncoderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with EncoderMacrosImpl {

  def deriveEncoderImpl[A: c.WeakTypeTag](
      config: c.Expr[YamlConfig]
  ): c.Expr[YamlEncoder[A]] = deriveEncoderTypeClass[A](config).asInstanceOf[c.Expr[YamlEncoder[A]]]

  def deriveKindlingsEncoderImpl[A: c.WeakTypeTag](
      config: c.Expr[YamlConfig]
  ): c.Expr[KindlingsYamlEncoder[A]] = deriveEncoderTypeClass[A](config)

  def deriveInlineEncodeImpl[A: c.WeakTypeTag](
      value: c.Expr[A]
  )(config: c.Expr[YamlConfig]): c.Expr[Node] = deriveInlineEncode[A](value, config)

  def deriveInlineToYamlStringImpl[A: c.WeakTypeTag](
      value: c.Expr[A]
  )(config: c.Expr[YamlConfig]): c.Expr[String] = deriveInlineToYamlString[A](value, config)

  @scala.annotation.nowarn("msg=unchecked")
  def deriveInlineToYamlStringOpsImpl[A: c.WeakTypeTag](
      config: c.Expr[YamlConfig]
  ): c.Expr[String] = {
    val value = c.Expr[A](c.prefix.tree match {
      case c.universe.Apply(_, List(arg)) => arg
      case other                          => c.abort(c.enclosingPosition, s"Unexpected prefix: $other")
    })
    deriveInlineToYamlString[A](value, config)
  }
}
