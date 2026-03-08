package hearth.kindlings.xmlderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[xmlderivation] class DecoderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotationSupportScala2
    with DecoderMacrosImpl {

  def deriveDecoderImpl[A: c.WeakTypeTag](
      config: c.Expr[XmlConfig]
  ): c.Expr[XmlDecoder[A]] = deriveDecoderTypeClass[A](config).asInstanceOf[c.Expr[XmlDecoder[A]]]

  def deriveKindlingsDecoderImpl[A: c.WeakTypeTag](
      config: c.Expr[XmlConfig]
  ): c.Expr[KindlingsXmlDecoder[A]] = deriveDecoderTypeClass[A](config)

  def deriveInlineDecodeImpl[A: c.WeakTypeTag](
      elem: c.Expr[scala.xml.Elem]
  )(config: c.Expr[XmlConfig]): c.Expr[Either[XmlDecodingError, A]] = deriveInlineDecode[A](elem, config)

  def deriveInlineFromXmlStringImpl[A: c.WeakTypeTag](
      xml: c.Expr[String]
  )(config: c.Expr[XmlConfig]): c.Expr[Either[XmlDecodingError, A]] = deriveInlineFromXmlString[A](xml, config)

  @scala.annotation.nowarn("msg=unchecked")
  def deriveInlineFromXmlStringOpsImpl[A: c.WeakTypeTag](
      config: c.Expr[XmlConfig]
  ): c.Expr[Either[XmlDecodingError, A]] = {
    val xml = c.Expr[String](c.prefix.tree match {
      case c.universe.Apply(_, List(arg)) => arg
      case other                          => c.abort(c.enclosingPosition, s"Unexpected prefix: $other")
    })
    deriveInlineFromXmlString[A](xml, config)
  }
}
