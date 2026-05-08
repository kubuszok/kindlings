package hearth.kindlings.benchmarks

import com.sksamuel.avro4s.{Decoder, Encoder, RecordFormat, SchemaFor}

object OriginalAvroSemiAutoInstances {
  implicit val addressSchemaFor: SchemaFor[Address] = SchemaFor[Address]
  implicit val addressEncoder: Encoder[Address] = Encoder[Address]
  implicit val addressDecoder: Decoder[Address] = Decoder[Address]

  implicit val simpleCCSchemaFor: SchemaFor[SimpleCC] = SchemaFor[SimpleCC]
  implicit val simpleCCEncoder: Encoder[SimpleCC] = Encoder[SimpleCC]
  implicit val simpleCCDecoder: Decoder[SimpleCC] = Decoder[SimpleCC]

  implicit val personSchemaFor: SchemaFor[Person] = SchemaFor[Person]
  implicit val personEncoder: Encoder[Person] = Encoder[Person]
  implicit val personDecoder: Decoder[Person] = Decoder[Person]

  val simpleCCFormat: RecordFormat[SimpleCC] = RecordFormat[SimpleCC]
  val personFormat: RecordFormat[Person] = RecordFormat[Person]
}
