package hearth.kindlings.circederivation.internal.runtime

import cats.data.ValidatedNel
import hearth.kindlings.circederivation.{
  KindlingsCodecAsObject,
  KindlingsDecoder,
  KindlingsEncoder,
  KindlingsEncoderAsObject
}
import io.circe.{Decoder, DecodingFailure, HCursor, Json, JsonObject}

object CirceDerivationFactories {

  def encoderInstance[A](applyFn: A => Json): KindlingsEncoder[A] =
    new KindlingsEncoder[A] {
      def apply(a: A): Json = applyFn(a)
    }

  def encoderAsObjectInstance[A](encodeObjectFn: A => JsonObject): KindlingsEncoderAsObject[A] =
    new KindlingsEncoderAsObject[A] {
      def encodeObject(a: A): JsonObject = encodeObjectFn(a)
    }

  def decoderInstance[A](applyFn: HCursor => Decoder.Result[A]): KindlingsDecoder[A] =
    new KindlingsDecoder[A] {
      def apply(c: HCursor): Decoder.Result[A] = applyFn(c)
    }

  def decoderInstanceWithFields[A](
      applyFn: HCursor => Decoder.Result[A],
      fields: Set[String]
  ): KindlingsDecoder[A] =
    new KindlingsDecoder[A] {
      def apply(c: HCursor): Decoder.Result[A] = applyFn(c)
      override def expectedFields: Option[Set[String]] = Some(fields)
    }

  def decoderWithAccInstance[A](
      applyFn: HCursor => Decoder.Result[A],
      decodeAccFn: HCursor => ValidatedNel[DecodingFailure, A]
  ): KindlingsDecoder[A] =
    new KindlingsDecoder[A] {
      def apply(c: HCursor): Decoder.Result[A] = applyFn(c)
      override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] = decodeAccFn(c)
    }

  def decoderWithAccInstanceAndFields[A](
      applyFn: HCursor => Decoder.Result[A],
      decodeAccFn: HCursor => ValidatedNel[DecodingFailure, A],
      fields: Set[String]
  ): KindlingsDecoder[A] =
    new KindlingsDecoder[A] {
      def apply(c: HCursor): Decoder.Result[A] = applyFn(c)
      override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] = decodeAccFn(c)
      override def expectedFields: Option[Set[String]] = Some(fields)
    }

  def codecAsObjectInstance[A](
      encodeObjectFn: A => JsonObject,
      applyFn: HCursor => Decoder.Result[A]
  ): KindlingsCodecAsObject[A] =
    new KindlingsCodecAsObject[A] {
      def encodeObject(a: A): JsonObject = encodeObjectFn(a)
      def apply(c: HCursor): Decoder.Result[A] = applyFn(c)
    }
}
