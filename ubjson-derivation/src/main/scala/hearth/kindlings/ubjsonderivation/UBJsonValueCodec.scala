package hearth.kindlings.ubjsonderivation

/** Type class for encoding and decoding values to/from UBJson binary format. */
trait UBJsonValueCodec[A] {

  /** Decode a value from the given UBJson reader. */
  def decode(reader: UBJsonReader): A

  /** Encode a value to the given UBJson writer. */
  def encode(writer: UBJsonWriter, value: A): Unit

  /** The null/default value for type A. */
  def nullValue: A
}
