package hearth.kindlings.xmlderivation.internal.runtime

import hearth.kindlings.xmlderivation.{KindlingsXmlCodec, KindlingsXmlDecoder, KindlingsXmlEncoder, XmlDecodingError}

object XmlDerivationFactories {

  def encoderInstance[A](encodeFn: (A, String) => scala.xml.Elem): KindlingsXmlEncoder[A] =
    new KindlingsXmlEncoder[A] {
      def encode(value: A, elementName: String): scala.xml.Elem = encodeFn(value, elementName)
    }

  def decoderInstance[A](decodeFn: scala.xml.Elem => Either[XmlDecodingError, A]): KindlingsXmlDecoder[A] =
    new KindlingsXmlDecoder[A] {
      def decode(elem: scala.xml.Elem): Either[XmlDecodingError, A] = decodeFn(elem)
    }

  def codecInstance[A](
      encodeFn: (A, String) => scala.xml.Elem,
      decodeFn: scala.xml.Elem => Either[XmlDecodingError, A]
  ): KindlingsXmlCodec[A] =
    new KindlingsXmlCodec[A] {
      def encode(value: A, elementName: String): scala.xml.Elem = encodeFn(value, elementName)
      def decode(elem: scala.xml.Elem): Either[XmlDecodingError, A] = decodeFn(elem)
    }
}
