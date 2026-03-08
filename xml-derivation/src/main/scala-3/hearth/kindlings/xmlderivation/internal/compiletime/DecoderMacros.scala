package hearth.kindlings.xmlderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[xmlderivation] class DecoderMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      DecoderMacrosImpl
private[xmlderivation] object DecoderMacros {

  def deriveDecoderImpl[A: Type](
      config: Expr[XmlConfig]
  )(using q: Quotes): Expr[XmlDecoder[A]] =
    new DecoderMacros(q).deriveDecoderTypeClass[A](config)

  def deriveKindlingsDecoderImpl[A: Type](
      config: Expr[XmlConfig]
  )(using q: Quotes): Expr[KindlingsXmlDecoder[A]] =
    new DecoderMacros(q).deriveDecoderTypeClass[A](config)

  def deriveInlineDecodeImpl[A: Type](
      elem: Expr[scala.xml.Elem],
      config: Expr[XmlConfig]
  )(using q: Quotes): Expr[Either[XmlDecodingError, A]] =
    new DecoderMacros(q).deriveInlineDecode[A](elem, config)

  def deriveInlineFromXmlStringImpl[A: Type](
      xml: Expr[String],
      config: Expr[XmlConfig]
  )(using q: Quotes): Expr[Either[XmlDecodingError, A]] =
    new DecoderMacros(q).deriveInlineFromXmlString[A](xml, config)
}
