package hearth.kindlings.xmlderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[xmlderivation] class EncoderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with EncoderMacrosImpl {

  def deriveEncoderImpl[A: c.WeakTypeTag](
      config: c.Expr[XmlConfig]
  ): c.Expr[XmlEncoder[A]] = deriveEncoderTypeClass[A](config).asInstanceOf[c.Expr[XmlEncoder[A]]]

  def deriveKindlingsEncoderImpl[A: c.WeakTypeTag](
      config: c.Expr[XmlConfig]
  ): c.Expr[KindlingsXmlEncoder[A]] = deriveEncoderTypeClass[A](config)

  def deriveInlineEncodeImpl[A: c.WeakTypeTag](
      value: c.Expr[A],
      elementName: c.Expr[String]
  )(config: c.Expr[XmlConfig]): c.Expr[scala.xml.Elem] = deriveInlineEncode[A](value, elementName, config)

  def deriveInlineToXmlStringImpl[A: c.WeakTypeTag](
      value: c.Expr[A],
      elementName: c.Expr[String]
  )(config: c.Expr[XmlConfig]): c.Expr[String] = deriveInlineToXmlString[A](value, elementName, config)

  @scala.annotation.nowarn("msg=unchecked")
  def deriveInlineToXmlStringOpsImpl[A: c.WeakTypeTag](
      elementName: c.Expr[String]
  )(config: c.Expr[XmlConfig]): c.Expr[String] = {
    val value = c.Expr[A](c.prefix.tree match {
      case c.universe.Apply(_, List(arg)) => arg
      case other                          => c.abort(c.enclosingPosition, s"Unexpected prefix: $other")
    })
    deriveInlineToXmlString[A](value, elementName, config)
  }
}
