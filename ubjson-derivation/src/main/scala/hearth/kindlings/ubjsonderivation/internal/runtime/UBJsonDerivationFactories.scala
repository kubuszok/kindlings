package hearth.kindlings.ubjsonderivation.internal.runtime

import hearth.kindlings.ubjsonderivation.{UBJsonReader, UBJsonValueCodec, UBJsonWriter}

object UBJsonDerivationFactories {

  def codecInstance[A](
      nullVal: A,
      decodeFn: UBJsonReader => A,
      encodeFn: (UBJsonWriter, A) => Unit
  ): UBJsonValueCodec[A] = new UBJsonValueCodec[A] {
    def nullValue: A = nullVal
    def decode(reader: UBJsonReader): A = decodeFn(reader)
    def encode(writer: UBJsonWriter, value: A): Unit = encodeFn(writer, value)
  }
}
