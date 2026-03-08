package hearth.kindlings.xmlderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[xmlderivation] class EncoderMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnnotationSupportScala3,
      EncoderMacrosImpl
private[xmlderivation] object EncoderMacros {

  def deriveEncoderImpl[A: Type](
      config: Expr[XmlConfig]
  )(using q: Quotes): Expr[XmlEncoder[A]] =
    new EncoderMacros(q).deriveEncoderTypeClass[A](config)

  def deriveKindlingsEncoderImpl[A: Type](
      config: Expr[XmlConfig]
  )(using q: Quotes): Expr[KindlingsXmlEncoder[A]] =
    new EncoderMacros(q).deriveEncoderTypeClass[A](config)

  def deriveInlineEncodeImpl[A: Type](
      value: Expr[A],
      elementName: Expr[String],
      config: Expr[XmlConfig]
  )(using q: Quotes): Expr[scala.xml.Elem] =
    new EncoderMacros(q).deriveInlineEncode[A](value, elementName, config)

  def deriveInlineToXmlStringImpl[A: Type](
      value: Expr[A],
      elementName: Expr[String],
      config: Expr[XmlConfig]
  )(using q: Quotes): Expr[String] =
    new EncoderMacros(q).deriveInlineToXmlString[A](value, elementName, config)
}
