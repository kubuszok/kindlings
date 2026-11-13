package hearth.kindlings.circederivation

import io.circe.{Decoder, DecodingFailure, HCursor}

/** Port of circe-generic-extras' `ExtrasDecoder`: a decoder that tells whether it decodes strictly and, when it does,
  * reports the extraneous fields of a failed decoding.
  *
  * Kindlings-derived decoders of case classes are strict when the configuration enables `strictDecoding`; other
  * decoders (sums, value types, collections...) are not strict, like generic-extras' ADT decoders.
  */
trait ExtrasDecoder[A] extends Decoder[A] {

  /** Whether this decoder rejects unexpected fields (`strictDecoding` for a derived case-class decoder). */
  def isStrict: Boolean = false

  /** Decodes like `apply`; on failure a strict decoder also returns the extraneous fields of the JSON object. */
  def decodeStrict(c: HCursor): ExtrasDecoder.StrictResult[A] = apply(c) match {
    case Right(value) => Right(value)
    case Left(df)     => Left((df, Nil))
  }
}
object ExtrasDecoder {

  /** Includes a list of extraneous fields on failure if the decoder is configured to be strict. */
  type StrictResult[A] = Either[(DecodingFailure, List[String]), A]
}
