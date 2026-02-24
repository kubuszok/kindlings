package hearth.kindlings.avroderivation

object syntax {

  extension [A](value: A) {
    inline def toAvroBinary(using config: AvroConfig): Array[Byte] = {
      implicit val encoder: AvroEncoder[A] = AvroEncoder.derived[A]
      AvroIO.toBinary(value)
    }
    inline def toAvroJson(using config: AvroConfig): String = {
      implicit val encoder: AvroEncoder[A] = AvroEncoder.derived[A]
      AvroIO.toJson(value)
    }
  }

  extension (bytes: Array[Byte]) {
    inline def fromAvroBinary[A](using config: AvroConfig): A = {
      implicit val decoder: AvroDecoder[A] = AvroDecoder.derived[A]
      AvroIO.fromBinary[A](bytes)
    }
  }

  extension (json: String) {
    inline def fromAvroJson[A](using config: AvroConfig): A = {
      implicit val decoder: AvroDecoder[A] = AvroDecoder.derived[A]
      AvroIO.fromJson[A](json)
    }
  }
}
