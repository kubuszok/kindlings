package hearth.kindlings.circederivation.internal.runtime

import cats.data.ValidatedNel
import hearth.kindlings.circederivation.{
  ExtrasDecoder,
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

  /** [[encoderInstance]] for a sum type (see [[KindlingsSumEncoder]]). */
  def sumEncoderInstance[A](applyFn: A => Json): KindlingsEncoder[A] =
    new KindlingsEncoder[A] with KindlingsSumEncoder {
      def apply(a: A): Json = applyFn(a)
    }

  /** [[encoderAsObjectInstance]] for a sum type (see [[KindlingsSumEncoder]]). */
  def sumEncoderAsObjectInstance[A](encodeObjectFn: A => JsonObject): KindlingsEncoderAsObject[A] =
    new KindlingsEncoderAsObject[A] with KindlingsSumEncoder {
      def encodeObject(a: A): JsonObject = encodeObjectFn(a)
    }

  /** [[decoderWithAccInstance]] for a sum type (see [[KindlingsSumDecoder]]). */
  def sumDecoderWithAccInstance[A](
      applyFn: HCursor => Decoder.Result[A],
      decodeAccFn: HCursor => ValidatedNel[DecodingFailure, A],
      handles: String => Boolean
  ): KindlingsDecoder[A] =
    new KindlingsDecoder[A] with KindlingsSumDecoder {
      def apply(c: HCursor): Decoder.Result[A] = applyFn(c)
      override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] = decodeAccFn(c)
      def kindlingsHandlesConstructor(constructorName: String): Boolean = handles(constructorName)
    }

  /** [[decoderWithAccInstance]] for a case class: exposes its expected fields and, with `strict`, reports the
    * extraneous fields in [[hearth.kindlings.circederivation.ExtrasDecoder.decodeStrict]] (generic-extras parity).
    */
  def productDecoderWithAccInstance[A](
      applyFn: HCursor => Decoder.Result[A],
      decodeAccFn: HCursor => ValidatedNel[DecodingFailure, A],
      strict: Boolean,
      fields: List[String]
  ): KindlingsDecoder[A] =
    new KindlingsDecoder[A] {
      private[this] val fieldSet: Set[String] = fields.toSet
      def apply(c: HCursor): Decoder.Result[A] = applyFn(c)
      override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] = decodeAccFn(c)
      override def expectedFields: Option[Set[String]] = Some(fieldSet)
      override def isStrict: Boolean = strict
      override def decodeStrict(c: HCursor): ExtrasDecoder.StrictResult[A] = applyFn(c) match {
        case Right(value) => Right(value)
        case Left(df)     =>
          Left((df, if (strict) c.keys.fold(List.empty[String])(_.toList.filterNot(fieldSet)) else Nil))
      }
    }

  def decoderInstance[A](applyFn: HCursor => Decoder.Result[A]): KindlingsDecoder[A] =
    new KindlingsDecoder[A] {
      def apply(c: HCursor): Decoder.Result[A] = applyFn(c)
    }

  /** A `KindlingsDecoder` delegating every decoding method (incl. `decodeAccumulating` and `tryDecode`) to `decoder`.
    */
  def delegatingDecoderInstance[A](decoder: Decoder[A]): KindlingsDecoder[A] =
    new KindlingsDecoder[A] {
      def apply(c: HCursor): Decoder.Result[A] = decoder(c)
      override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] = decoder.decodeAccumulating(c)
      override def tryDecode(c: io.circe.ACursor): Decoder.Result[A] = decoder.tryDecode(c)
      override def tryDecodeAccumulating(c: io.circe.ACursor): Decoder.AccumulatingResult[A] =
        decoder.tryDecodeAccumulating(c)
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
