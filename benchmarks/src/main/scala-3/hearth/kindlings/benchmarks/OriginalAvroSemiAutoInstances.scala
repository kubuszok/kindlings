package hearth.kindlings.benchmarks

import com.sksamuel.avro4s.{Decoder as AvroDecoder, Encoder as AvroEncoder, SchemaFor}

object OriginalAvroSemiAutoInstances {
  given SchemaFor[Address] = SchemaFor.derived
  given avroEncoderAddress: AvroEncoder[Address] = AvroEncoder.derived
  given avroDecoderAddress: AvroDecoder[Address] = AvroDecoder.derived

  given SchemaFor[SimpleCC] = SchemaFor.derived
  given avroEncoderSimpleCC: AvroEncoder[SimpleCC] = AvroEncoder.derived
  given avroDecoderSimpleCC: AvroDecoder[SimpleCC] = AvroDecoder.derived

  given SchemaFor[Person] = SchemaFor.derived
  given avroEncoderPerson: AvroEncoder[Person] = AvroEncoder.derived
  given avroDecoderPerson: AvroDecoder[Person] = AvroDecoder.derived

  val simpleCCFormat: Avro4sFormat[SimpleCC] =
    new Avro4sFormat(summon[SchemaFor[SimpleCC]].schema, avroEncoderSimpleCC, avroDecoderSimpleCC)
  val personFormat: Avro4sFormat[Person] =
    new Avro4sFormat(summon[SchemaFor[Person]].schema, avroEncoderPerson, avroDecoderPerson)
}
